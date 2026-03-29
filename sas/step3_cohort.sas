/*****************************************************************************
 Step 3: Combined Diabetes + DFU Cohort Builder (Passthrough Optimized)

 Replaces separate step3_diabetes_cohort.sas and step4_dfu_identification.sas.
 Uses explicit SQL passthrough to push all filtering to the database server.
 Each table is scanned ONCE for both DM (E10/E11) and DFU (L97/combo) codes,
 halving total table scans from ~30 to ~15.

 Architecture:
   Part A: Helper macros for generating DX field conditions
   Part B: Commercial passthrough queries (8 year tables, 2017-2024)
   Part C: Commercial aggregation -> dm_cohort_commercial + dfu_cohort_commercial
   Part D: Medicare passthrough queries (7 claim tables)
   Part E: Medicare aggregation -> dm_cohort_medicare + dfu_cohort_medicare
   Part F: Cross-validation with BEN_SUM_CC
   Part G: Summary reports + cleanup

 ICD-10-CM Codes Scanned:
   Diabetes:  E10.x (T1D), E11.x (T2D)
   DFU:       L97.1xx-L97.9xx (non-pressure chronic ulcer of lower limb)
   DFU combo: E10.621, E10.622, E11.621, E11.622

 Output (4 WORK datasets consumed by step5):
   dm_cohort_commercial:  submitter, group_policy, person_code, dm_type,
                          first_dm_year, last_dm_year, n_dm_claims, first_dm_date
   dm_cohort_medicare:    bene_id, dm_type, first_dm_year, last_dm_year,
                          n_dm_claims, first_dm_date
   dfu_cohort_commercial: submitter, group_policy, person_code, first_dfu_year,
                          first_dfu_date, last_dfu_year, n_dfu_claims,
                          ever_l97, ever_dm_combo, max_severity_rank
   dfu_cohort_medicare:   bene_id, first_dfu_year, first_dfu_date, last_dfu_year,
                          n_dfu_claims, ever_l97, ever_dm_combo, max_severity_rank

 Person Keys:
   Commercial: mc001 (submitter) + mc006 (group/policy) + mc009 (person seq)
   Medicare:   bene_id

 DX Fields:
   Commercial (14): mc039 (admitting), mc041 (principal), mc042-mc053
   Medicare: prncpal_dgns_cd + icd_dgns_cd1..N [+ admtg_dgns_cd on INP/SNF]

 Server-confirmed facts (step2):
   - ICD codes stored WITHOUT dots (e.g., E1165, L97522)
   - ZIPs are 5-digit
   - admtg_dgns_cd exists on INP and SNF tables
   - All 18 tables present

 L97 Severity (character 6, after removing dots — codes have no dots on server):
   4 = Necrosis of bone (most severe)
   3 = Necrosis of muscle
   2 = Fat layer exposed
   1 = Skin breakdown only
   0 = Other / unspecified / code too short
*****************************************************************************/

/* Libname for cross-validation queries (Part F) */
libname arapcd odbc
    noprompt="dsn=APCD-24D;Trusted_connection=yes"
    schema=public;

/* Single-quote character for use inside passthrough SQL strings.
   &macro_vars do not resolve inside single quotes, so we store a
   quote character in &q and concatenate: &q.E10%&q. -> 'E10%'     */
%let q = %str(%');

/* ======================================================================= */
/* PART A: Helper Macros                                                    */
/*   These generate native SQL fragments. SAS macros resolve BEFORE the     */
/*   SQL is sent to the database via passthrough.                           */
/* ======================================================================= */

/*--- Commercial: LIKE 'prefix%' across 14 DX fields ---*/
%macro comm_like(prefix);
    upper(mc039) like &q.&prefix.%&q.
    or upper(mc041) like &q.&prefix.%&q.
    %do i = 42 %to 53;
        or upper(mc0&i.) like &q.&prefix.%&q.
    %end;
%mend;

/*--- Commercial: IN (DFU combo codes) across 14 DX fields ---*/
%macro comm_in_combo;
    upper(mc039) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.)
    or upper(mc041) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.)
    %do i = 42 %to 53;
        or upper(mc0&i.) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.)
    %end;
%mend;

/*--- Commercial: extract first L97 code found across 14 DX fields ---*/
%macro comm_first_l97;
    case
        when upper(mc039) like &q.L97%&q. then upper(mc039)
        when upper(mc041) like &q.L97%&q. then upper(mc041)
        %do i = 42 %to 53;
            when upper(mc0&i.) like &q.L97%&q. then upper(mc0&i.)
        %end;
        else &q.&q.
    end
%mend;

/*--- Medicare: LIKE 'prefix%' across prncpal + icd_dgns_cd1..N [+admtg] ---*/
%macro mcr_like(prefix, max_dx, has_admtg);
    %if &has_admtg = 1 %then %do;
        upper(admtg_dgns_cd) like &q.&prefix.%&q. or
    %end;
    upper(prncpal_dgns_cd) like &q.&prefix.%&q.
    %do i = 1 %to &max_dx;
        or upper(icd_dgns_cd&i.) like &q.&prefix.%&q.
    %end;
%mend;

/*--- Medicare: IN (DFU combo codes) across DX fields ---*/
%macro mcr_in_combo(max_dx, has_admtg);
    %if &has_admtg = 1 %then %do;
        upper(admtg_dgns_cd) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.) or
    %end;
    upper(prncpal_dgns_cd) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.)
    %do i = 1 %to &max_dx;
        or upper(icd_dgns_cd&i.) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.)
    %end;
%mend;

/*--- Medicare: extract first L97 code found ---*/
%macro mcr_first_l97(max_dx, has_admtg);
    case
        %if &has_admtg = 1 %then %do;
            when upper(admtg_dgns_cd) like &q.L97%&q. then upper(admtg_dgns_cd)
        %end;
        when upper(prncpal_dgns_cd) like &q.L97%&q. then upper(prncpal_dgns_cd)
        %do i = 1 %to &max_dx;
            when upper(icd_dgns_cd&i.) like &q.L97%&q. then upper(icd_dgns_cd&i.)
        %end;
        else &q.&q.
    end
%mend;

/* ======================================================================= */
/* PART B: Commercial Passthrough Queries (2017-2024)                       */
/*   One connection per year table. Each scan finds DM + DFU in one pass.   */
/*   WHERE clause: any DX field has E10% OR E11% OR L97%.                   */
/*   (E10621/E11621 combo codes are subsets of E10%/E11%, so already        */
/*    captured by the WHERE; flagged separately in SELECT.)                 */
/* ======================================================================= */

%macro get_comm(yr);
    proc sql;
        connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
        create table _comm_&yr. as
        select * from connection to odbc (
            select mc001 as submitter,
                   mc006 as group_policy,
                   mc009 as person_code,
                   mc017 as service_date,
                   &yr. as claim_year,
                   /* Diabetes flags */
                   case when (%comm_like(E10)) then 1 else 0 end as has_t1d,
                   case when (%comm_like(E11)) then 1 else 0 end as has_t2d,
                   /* DFU flags */
                   case when (%comm_like(L97)) then 1 else 0 end as has_l97,
                   case when (%comm_in_combo)  then 1 else 0 end as has_dm_combo,
                   /* First L97 code found (for severity parsing in SAS) */
                   cast(%comm_first_l97 as varchar(10)) as l97_code
            from public.CLAIM_SVC_DT_&yr.
            where (%comm_like(E10))
               or (%comm_like(E11))
               or (%comm_like(L97))
        );
        disconnect from odbc;
    quit;
%mend;

%get_comm(2017);
%get_comm(2018);
%get_comm(2019);
%get_comm(2020);
%get_comm(2021);
%get_comm(2022);
%get_comm(2023);
%get_comm(2024);

/* Stack all commercial results and parse L97 severity */
data dm_dfu_claims_commercial;
    set _comm_2017 _comm_2018 _comm_2019 _comm_2020
        _comm_2021 _comm_2022 _comm_2023 _comm_2024;
    length _sev $1;
    if l97_code ne '' and length(strip(l97_code)) >= 6 then do;
        _sev = substr(strip(l97_code), 6, 1);
        if      _sev = '4' then l97_severity_rank = 4;
        else if _sev = '3' then l97_severity_rank = 3;
        else if _sev = '2' then l97_severity_rank = 2;
        else if _sev = '1' then l97_severity_rank = 1;
        else l97_severity_rank = 0;
    end;
    else l97_severity_rank = 0;
    drop _sev;
run;

/* Free year-level temp tables */
proc datasets lib=work nolist;
    delete _comm_2017 _comm_2018 _comm_2019 _comm_2020
           _comm_2021 _comm_2022 _comm_2023 _comm_2024;
quit;

/* ======================================================================= */
/* PART C: Commercial Aggregation                                           */
/* ======================================================================= */

/* C-1: Diabetes cohort — all patients with E10/E11 codes */
proc sql;
    create table dm_cohort_commercial as
    select submitter,
           group_policy,
           person_code,
           case
               when max(has_t1d) = 1 and max(has_t2d) = 1 then 'AMBIGUOUS'
               when max(has_t1d) = 1 then 'T1D'
               when max(has_t2d) = 1 then 'T2D'
               else 'UNKNOWN'
           end as dm_type length=10,
           min(service_date) as first_dm_date format=yymmdd10.,
           min(claim_year) as first_dm_year,
           max(claim_year) as last_dm_year,
           count(*) as n_dm_claims,
           'COMMERCIAL' as data_source length=10
    from dm_dfu_claims_commercial
    where has_t1d = 1 or has_t2d = 1
    group by submitter, group_policy, person_code;
quit;

/* C-2: DFU cohort — patients with L97/combo codes who are in DM cohort */
proc sql;
    create table dfu_cohort_commercial as
    select a.submitter,
           a.group_policy,
           a.person_code,
           min(a.service_date) as first_dfu_date format=yymmdd10.,
           min(a.claim_year) as first_dfu_year,
           max(a.claim_year) as last_dfu_year,
           count(*) as n_dfu_claims,
           max(a.has_l97) as ever_l97,
           max(a.has_dm_combo) as ever_dm_combo,
           max(a.l97_severity_rank) as max_severity_rank,
           'COMMERCIAL' as data_source length=10
    from dm_dfu_claims_commercial a
    inner join dm_cohort_commercial b
        on a.submitter = b.submitter
       and a.group_policy = b.group_policy
       and a.person_code = b.person_code
    where a.has_l97 = 1 or a.has_dm_combo = 1
    group by a.submitter, a.group_policy, a.person_code;
quit;

/* Free commercial claims */
proc datasets lib=work nolist; delete dm_dfu_claims_commercial; quit;

/* ======================================================================= */
/* PART D: Medicare Passthrough Queries (7 claim tables)                    */
/*   One connection per table. Same dual DM+DFU scan.                      */
/*   Date filter: clm_from_dt < '2025-01-01' (equivalent to year <= 2024)  */
/* ======================================================================= */

%macro get_mcr(name, tbl, max_dx, has_admtg);
    proc sql;
        connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
        create table _mcr_&name. as
        select * from connection to odbc (
            select bene_id,
                   clm_from_dt as service_date,
                   /* Diabetes flags */
                   case when (%mcr_like(E10, &max_dx., &has_admtg.)) then 1 else 0 end as has_t1d,
                   case when (%mcr_like(E11, &max_dx., &has_admtg.)) then 1 else 0 end as has_t2d,
                   /* DFU flags */
                   case when (%mcr_like(L97, &max_dx., &has_admtg.)) then 1 else 0 end as has_l97,
                   case when (%mcr_in_combo(&max_dx., &has_admtg.))  then 1 else 0 end as has_dm_combo,
                   /* First L97 code for severity */
                   cast(%mcr_first_l97(&max_dx., &has_admtg.) as varchar(10)) as l97_code
            from public.APCD_MCR_&tbl._CLM
            where ((%mcr_like(E10, &max_dx., &has_admtg.))
                or (%mcr_like(E11, &max_dx., &has_admtg.))
                or (%mcr_like(L97, &max_dx., &has_admtg.)))
              and clm_from_dt < &q.2025-01-01&q.
        );
        disconnect from odbc;
    quit;
%mend;

/* Part B Carrier: 12 DX fields, no admtg_dgns_cd */
%get_mcr(prtb, PRTB_CAR, 12, 0);

/* Outpatient: 25 DX fields, no admtg_dgns_cd */
%get_mcr(out, OUT, 25, 0);

/* Inpatient: 25 DX fields + admtg_dgns_cd */
%get_mcr(inp, INP, 25, 1);

/* SNF: 25 DX fields + admtg_dgns_cd */
%get_mcr(snf, SNF, 25, 1);

/* HHA: 25 DX fields */
%get_mcr(hha, HHA, 25, 0);

/* HSP (Hospice): 25 DX fields */
%get_mcr(hsp, HSP, 25, 0);

/* DME: 12 DX fields */
%get_mcr(dme, DME, 12, 0);

/* Stack all Medicare results and parse L97 severity */
data dm_dfu_claims_medicare;
    set _mcr_prtb _mcr_out _mcr_inp _mcr_snf _mcr_hha _mcr_hsp _mcr_dme;
    claim_year = year(service_date);
    length _sev $1;
    if l97_code ne '' and length(strip(l97_code)) >= 6 then do;
        _sev = substr(strip(l97_code), 6, 1);
        if      _sev = '4' then l97_severity_rank = 4;
        else if _sev = '3' then l97_severity_rank = 3;
        else if _sev = '2' then l97_severity_rank = 2;
        else if _sev = '1' then l97_severity_rank = 1;
        else l97_severity_rank = 0;
    end;
    else l97_severity_rank = 0;
    drop _sev;
run;

/* Free table-level temp datasets */
proc datasets lib=work nolist;
    delete _mcr_prtb _mcr_out _mcr_inp _mcr_snf _mcr_hha _mcr_hsp _mcr_dme;
quit;

/* ======================================================================= */
/* PART E: Medicare Aggregation                                             */
/* ======================================================================= */

/* E-1: Diabetes cohort */
proc sql;
    create table dm_cohort_medicare as
    select bene_id,
           case
               when max(has_t1d) = 1 and max(has_t2d) = 1 then 'AMBIGUOUS'
               when max(has_t1d) = 1 then 'T1D'
               when max(has_t2d) = 1 then 'T2D'
               else 'UNKNOWN'
           end as dm_type length=10,
           min(service_date) as first_dm_date format=yymmdd10.,
           min(claim_year) as first_dm_year,
           max(claim_year) as last_dm_year,
           count(*) as n_dm_claims,
           'MEDICARE' as data_source length=10
    from dm_dfu_claims_medicare
    where has_t1d = 1 or has_t2d = 1
    group by bene_id;
quit;

/* E-2: DFU cohort — restricted to DM patients */
proc sql;
    create table dfu_cohort_medicare as
    select a.bene_id,
           min(a.service_date) as first_dfu_date format=yymmdd10.,
           min(a.claim_year) as first_dfu_year,
           max(a.claim_year) as last_dfu_year,
           count(*) as n_dfu_claims,
           max(a.has_l97) as ever_l97,
           max(a.has_dm_combo) as ever_dm_combo,
           max(a.l97_severity_rank) as max_severity_rank,
           'MEDICARE' as data_source length=10
    from dm_dfu_claims_medicare a
    inner join dm_cohort_medicare b
        on a.bene_id = b.bene_id
    where a.has_l97 = 1 or a.has_dm_combo = 1
    group by a.bene_id;
quit;

/* Free Medicare claims */
proc datasets lib=work nolist; delete dm_dfu_claims_medicare; quit;

/* ======================================================================= */
/* PART F: Cross-Validate Medicare with BEN_SUM_CC Diabetes Flags           */
/* ======================================================================= */

title "Step 3F: Medicare BEN_SUM_CC Diabetes Flags vs Claims-Based Cohort";
proc sql;
    select 'Claims-based DM patients' as source,
           count(distinct a.bene_id) as n_patients format=comma12.
    from dm_cohort_medicare a

    union all

    select 'Also flagged in BEN_SUM_CC',
           count(distinct a.bene_id)
    from dm_cohort_medicare a
    inner join arapcd.APCD_MCR_BEN_SUM_CC b
        on a.bene_id = b.bene_id
    where b.diabetes is not null and b.diabetes ne ''

    union all

    select 'In BEN_SUM_CC only (not in claims)',
           count(distinct b.bene_id)
    from arapcd.APCD_MCR_BEN_SUM_CC b
    left join dm_cohort_medicare a
        on a.bene_id = b.bene_id
    where (b.diabetes is not null and b.diabetes ne '')
      and a.bene_id is null;
quit;

/* ======================================================================= */
/* PART G: Summary Reports                                                  */
/* ======================================================================= */

title "Step 3G-1: Diabetes Cohort Summary - Commercial";
proc sql;
    select dm_type,
           count(*) as n_patients format=comma12.,
           min(first_dm_year) as earliest_year,
           max(last_dm_year) as latest_year,
           sum(n_dm_claims) as total_claims format=comma15.
    from dm_cohort_commercial
    group by dm_type;
quit;

title "Step 3G-2: Diabetes Cohort Summary - Medicare";
proc sql;
    select dm_type,
           count(*) as n_patients format=comma12.,
           min(first_dm_year) as earliest_year,
           max(last_dm_year) as latest_year,
           sum(n_dm_claims) as total_claims format=comma15.
    from dm_cohort_medicare
    group by dm_type;
quit;

title "Step 3G-3: DFU Cohort Summary by Data Source";
proc sql;
    select data_source,
           count(*) as n_dfu_patients format=comma12.,
           sum(n_dfu_claims) as total_dfu_claims format=comma15.,
           sum(ever_l97) as with_l97 format=comma12.,
           sum(ever_dm_combo) as with_combo format=comma12.
    from (
        select data_source, n_dfu_claims, ever_l97, ever_dm_combo
            from dfu_cohort_commercial
        union all
        select data_source, n_dfu_claims, ever_l97, ever_dm_combo
            from dfu_cohort_medicare
    )
    group by data_source;
quit;

title "Step 3G-4: Combined Pipeline Summary";
proc sql;
    select 'COMMERCIAL DM' as category,
           count(*) as n_patients format=comma12.
    from dm_cohort_commercial
    union all
    select 'COMMERCIAL DFU', count(*) from dfu_cohort_commercial
    union all
    select 'MEDICARE DM',    count(*) from dm_cohort_medicare
    union all
    select 'MEDICARE DFU',   count(*) from dfu_cohort_medicare;
quit;

title;

/*****************************************************************************
 NOTES:

 1. PASSTHROUGH OPTIMIZATION: Each table is scanned exactly once. The WHERE
    clause filters on the database server, returning only matching rows
    through ODBC. This avoids pulling billions of rows into SAS memory.

 2. COMBINED SCAN: DM (E10/E11) and DFU (L97/combo) codes are captured in
    a single pass per table. The DFU combo codes (E10621, E10622, E11621,
    E11622) are subsets of E10%/E11%, so the WHERE clause only needs three
    LIKE patterns: E10%, E11%, L97%.

 3. DFU RESTRICTION: The DFU cohort is restricted to patients already in the
    DM cohort via INNER JOIN. L97 claims from non-diabetic patients are
    excluded during aggregation.

 4. SEVERITY PARSING: L97 codes are 6-7 characters (no dots on server).
    Character 6 encodes severity: 4=bone necrosis, 3=muscle necrosis,
    2=fat exposed, 1=skin breakdown. max_severity_rank takes the worst
    severity across all claims for each patient.

 5. MEMORY MANAGEMENT: Intermediate temp tables (_comm_YYYY, _mcr_*)
    and claims-level datasets are deleted after aggregation to free WORK
    space. Only the 4 cohort datasets persist for step5.

 6. AMBIGUOUS DM TYPE: Patients with both E10.x and E11.x across their
    claim history are flagged as AMBIGUOUS per study protocol.

 7. COLUMN CONTRACT: The 4 output datasets match the exact column names
    and types expected by step5_zip_extract.sas. No changes to step5
    are required.
*****************************************************************************/
