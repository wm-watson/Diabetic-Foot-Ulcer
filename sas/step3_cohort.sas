/*****************************************************************************
 Step 3: Combined DM + DFU Cohort Builder (Memory-Optimized Passthrough)

 Replaces separate step3_diabetes_cohort.sas and step4_dfu_identification.sas.
 Uses explicit SQL passthrough with GROUP BY so the database returns
 patient-level summaries (not claim-level rows), drastically reducing the
 volume of data transferred through ODBC and stored in SAS.

 Memory strategy:
   - Each passthrough query aggregates to 1 row per patient (commercial:
     per patient per year; Medicare: per patient per table)
   - Results are appended incrementally to D:\WPWatson (not WORK)
   - Only the final 4 cohort datasets live in WORK for step5
   - All intermediates deleted after use

 Architecture:
   Part A: Helper macros (DX field conditions + severity extraction)
   Part B: Commercial passthrough w/ GROUP BY (8 year tables -> D:\WPWatson)
   Part C: Commercial final aggregation -> WORK.dm/dfu_cohort_commercial
   Part D: Medicare passthrough w/ GROUP BY (7 claim tables -> D:\WPWatson)
   Part E: Medicare final aggregation -> WORK.dm/dfu_cohort_medicare
   Part F: Cross-validation with BEN_SUM_CC
   Part G: Summary reports + cleanup

 ICD-10-CM Codes:
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

 NOTE: Passthrough SQL uses PostgreSQL syntax (length(), extract()).
       If the database is SQL Server, replace length() with len() and
       extract(year from col) with year(col).
*****************************************************************************/

/* Persistent library on personal drive — more space than WORK */
libname mylib 'D:\WPWatson';

/* Libname for BEN_SUM_CC cross-validation (Part F) */
libname arapcd odbc
    noprompt="dsn=APCD-24D;Trusted_connection=yes"
    schema=public;

/* Single-quote character for passthrough SQL strings.
   &macro_vars do not resolve inside SAS single quotes, so we store
   a quote character in &q and concatenate: &q.E10%&q. -> 'E10%'   */
%let q = %str(%');

/* ======================================================================= */
/* PART A: Helper Macros                                                    */
/*   Generate native SQL fragments. SAS macros resolve BEFORE the SQL       */
/*   is sent to the database via passthrough.                               */
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

/*--- Commercial: per-row L97 severity rank (first L97 code found) ---*/
/*    Returns integer 0-4. Used inside max() in GROUP BY.             */
%macro comm_row_severity;
    case
        when upper(mc039) like &q.L97%&q. and length(mc039) >= 6 then
            case substring(upper(mc039), 6, 1)
                when &q.4&q. then 4 when &q.3&q. then 3
                when &q.2&q. then 2 when &q.1&q. then 1 else 0 end
        when upper(mc041) like &q.L97%&q. and length(mc041) >= 6 then
            case substring(upper(mc041), 6, 1)
                when &q.4&q. then 4 when &q.3&q. then 3
                when &q.2&q. then 2 when &q.1&q. then 1 else 0 end
        %do i = 42 %to 53;
            when upper(mc0&i.) like &q.L97%&q. and length(mc0&i.) >= 6 then
                case substring(upper(mc0&i.), 6, 1)
                    when &q.4&q. then 4 when &q.3&q. then 3
                    when &q.2&q. then 2 when &q.1&q. then 1 else 0 end
        %end;
        else 0
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

/*--- Medicare: IN (DFU combo codes) ---*/
%macro mcr_in_combo(max_dx, has_admtg);
    %if &has_admtg = 1 %then %do;
        upper(admtg_dgns_cd) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.) or
    %end;
    upper(prncpal_dgns_cd) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.)
    %do i = 1 %to &max_dx;
        or upper(icd_dgns_cd&i.) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.)
    %end;
%mend;

/*--- Medicare: per-row L97 severity rank ---*/
%macro mcr_row_severity(max_dx, has_admtg);
    case
        %if &has_admtg = 1 %then %do;
            when upper(admtg_dgns_cd) like &q.L97%&q. and length(admtg_dgns_cd) >= 6 then
                case substring(upper(admtg_dgns_cd), 6, 1)
                    when &q.4&q. then 4 when &q.3&q. then 3
                    when &q.2&q. then 2 when &q.1&q. then 1 else 0 end
        %end;
        when upper(prncpal_dgns_cd) like &q.L97%&q. and length(prncpal_dgns_cd) >= 6 then
            case substring(upper(prncpal_dgns_cd), 6, 1)
                when &q.4&q. then 4 when &q.3&q. then 3
                when &q.2&q. then 2 when &q.1&q. then 1 else 0 end
        %do i = 1 %to &max_dx;
            when upper(icd_dgns_cd&i.) like &q.L97%&q. and length(icd_dgns_cd&i.) >= 6 then
                case substring(upper(icd_dgns_cd&i.), 6, 1)
                    when &q.4&q. then 4 when &q.3&q. then 3
                    when &q.2&q. then 2 when &q.1&q. then 1 else 0 end
        %end;
        else 0
    end
%mend;

/* ======================================================================= */
/* PART B: Commercial Passthrough with GROUP BY (2017-2024)                 */
/*   Each query returns 1 row per patient per year (not per claim).         */
/*   Results appended incrementally to mylib.comm_patient_year.             */
/* ======================================================================= */

%macro get_comm(yr, first=0);
    proc sql;
        connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
        create table _comm_yr as
        select * from connection to odbc (
            select mc001 as submitter,
                   mc006 as group_policy,
                   mc009 as person_code,
                   &yr. as claim_year,
                   /* DM flags: did this patient have E10/E11 this year? */
                   max(case when (%comm_like(E10)) then 1 else 0 end) as has_t1d,
                   max(case when (%comm_like(E11)) then 1 else 0 end) as has_t2d,
                   /* DFU flags: did this patient have L97/combo this year? */
                   max(case when (%comm_like(L97)) then 1 else 0 end) as has_l97,
                   max(case when (%comm_in_combo)  then 1 else 0 end) as has_dm_combo,
                   /* Claim counts */
                   sum(case when (%comm_like(E10)) or (%comm_like(E11))
                            then 1 else 0 end) as n_dm_claims,
                   sum(case when (%comm_like(L97)) or (%comm_in_combo)
                            then 1 else 0 end) as n_dfu_claims,
                   /* Earliest dates (null if no matching claims) */
                   min(case when (%comm_like(E10)) or (%comm_like(E11))
                            then mc017 else null end) as first_dm_date,
                   min(case when (%comm_like(L97)) or (%comm_in_combo)
                            then mc017 else null end) as first_dfu_date,
                   /* Worst L97 severity this year */
                   max(%comm_row_severity) as max_severity_rank
            from public.CLAIM_SVC_DT_&yr.
            where (%comm_like(E10))
               or (%comm_like(E11))
               or (%comm_like(L97))
            group by mc001, mc006, mc009
        );
        disconnect from odbc;
    quit;

    /* Append to accumulator on D:\WPWatson */
    %if &first = 1 %then %do;
        data mylib.comm_patient_year; set _comm_yr; run;
    %end;
    %else %do;
        proc append base=mylib.comm_patient_year data=_comm_yr force; run;
    %end;

    /* Free WORK immediately */
    proc datasets lib=work nolist; delete _comm_yr; quit;

    %put NOTE: Commercial &yr. complete.;
%mend;

%get_comm(2017, first=1);
%get_comm(2018);
%get_comm(2019);
%get_comm(2020);
%get_comm(2021);
%get_comm(2022);
%get_comm(2023);
%get_comm(2024);

/* ======================================================================= */
/* PART C: Commercial Final Aggregation (across all years)                  */
/*   Reads from mylib.comm_patient_year -> WORK cohort datasets             */
/* ======================================================================= */

/* C-1: Diabetes cohort */
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
           min(first_dm_date) as first_dm_date format=yymmdd10.,
           min(case when has_t1d = 1 or has_t2d = 1
                    then claim_year else . end) as first_dm_year,
           max(case when has_t1d = 1 or has_t2d = 1
                    then claim_year else . end) as last_dm_year,
           sum(n_dm_claims) as n_dm_claims,
           'COMMERCIAL' as data_source length=10
    from mylib.comm_patient_year
    group by submitter, group_policy, person_code
    having max(has_t1d) = 1 or max(has_t2d) = 1;
quit;

/* C-2: DFU cohort — restricted to DM patients */
proc sql;
    create table dfu_cohort_commercial as
    select a.submitter,
           a.group_policy,
           a.person_code,
           min(a.first_dfu_date) as first_dfu_date format=yymmdd10.,
           min(case when a.has_l97 = 1 or a.has_dm_combo = 1
                    then a.claim_year else . end) as first_dfu_year,
           max(case when a.has_l97 = 1 or a.has_dm_combo = 1
                    then a.claim_year else . end) as last_dfu_year,
           sum(a.n_dfu_claims) as n_dfu_claims,
           max(a.has_l97) as ever_l97,
           max(a.has_dm_combo) as ever_dm_combo,
           max(a.max_severity_rank) as max_severity_rank,
           'COMMERCIAL' as data_source length=10
    from mylib.comm_patient_year a
    inner join dm_cohort_commercial b
        on a.submitter = b.submitter
       and a.group_policy = b.group_policy
       and a.person_code = b.person_code
    where a.has_l97 = 1 or a.has_dm_combo = 1
    group by a.submitter, a.group_policy, a.person_code;
quit;

/* Free commercial intermediate */
proc datasets lib=mylib nolist; delete comm_patient_year; quit;

title "Step 3C: Commercial Cohort Counts";
proc sql;
    select 'DM patients' as metric, count(*) as n format=comma12.
        from dm_cohort_commercial
    union all
    select 'DFU patients', count(*) from dfu_cohort_commercial;
quit;

/* ======================================================================= */
/* PART D: Medicare Passthrough with GROUP BY (7 claim tables)              */
/*   Each query returns 1 row per patient per table (not per claim).        */
/*   Year stats computed via extract(year from clm_from_dt).                */
/*   Date filter: clm_from_dt < '2025-01-01' (= year <= 2024).             */
/* ======================================================================= */

%macro get_mcr(name, tbl, max_dx, has_admtg, first=0);
    proc sql;
        connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
        create table _mcr_tbl as
        select * from connection to odbc (
            select bene_id,
                   /* DM flags */
                   max(case when (%mcr_like(E10, &max_dx., &has_admtg.))
                            then 1 else 0 end) as has_t1d,
                   max(case when (%mcr_like(E11, &max_dx., &has_admtg.))
                            then 1 else 0 end) as has_t2d,
                   /* DFU flags */
                   max(case when (%mcr_like(L97, &max_dx., &has_admtg.))
                            then 1 else 0 end) as has_l97,
                   max(case when (%mcr_in_combo(&max_dx., &has_admtg.))
                            then 1 else 0 end) as has_dm_combo,
                   /* Claim counts */
                   sum(case when (%mcr_like(E10, &max_dx., &has_admtg.))
                              or (%mcr_like(E11, &max_dx., &has_admtg.))
                            then 1 else 0 end) as n_dm_claims,
                   sum(case when (%mcr_like(L97, &max_dx., &has_admtg.))
                              or (%mcr_in_combo(&max_dx., &has_admtg.))
                            then 1 else 0 end) as n_dfu_claims,
                   /* Dates */
                   min(case when (%mcr_like(E10, &max_dx., &has_admtg.))
                              or (%mcr_like(E11, &max_dx., &has_admtg.))
                            then clm_from_dt else null end) as first_dm_date,
                   min(case when (%mcr_like(L97, &max_dx., &has_admtg.))
                              or (%mcr_in_combo(&max_dx., &has_admtg.))
                            then clm_from_dt else null end) as first_dfu_date,
                   /* Year ranges */
                   min(case when (%mcr_like(E10, &max_dx., &has_admtg.))
                              or (%mcr_like(E11, &max_dx., &has_admtg.))
                            then extract(year from clm_from_dt) else null end)
                       as first_dm_year,
                   max(case when (%mcr_like(E10, &max_dx., &has_admtg.))
                              or (%mcr_like(E11, &max_dx., &has_admtg.))
                            then extract(year from clm_from_dt) else null end)
                       as last_dm_year,
                   min(case when (%mcr_like(L97, &max_dx., &has_admtg.))
                              or (%mcr_in_combo(&max_dx., &has_admtg.))
                            then extract(year from clm_from_dt) else null end)
                       as first_dfu_year,
                   max(case when (%mcr_like(L97, &max_dx., &has_admtg.))
                              or (%mcr_in_combo(&max_dx., &has_admtg.))
                            then extract(year from clm_from_dt) else null end)
                       as last_dfu_year,
                   /* Worst L97 severity */
                   max(%mcr_row_severity(&max_dx., &has_admtg.)) as max_severity_rank
            from public.APCD_MCR_&tbl._CLM
            where ((%mcr_like(E10, &max_dx., &has_admtg.))
                or (%mcr_like(E11, &max_dx., &has_admtg.))
                or (%mcr_like(L97, &max_dx., &has_admtg.)))
              and clm_from_dt < &q.2025-01-01&q.
            group by bene_id
        );
        disconnect from odbc;
    quit;

    /* Append to accumulator on D:\WPWatson */
    %if &first = 1 %then %do;
        data mylib.mcr_patient_tbl; set _mcr_tbl; run;
    %end;
    %else %do;
        proc append base=mylib.mcr_patient_tbl data=_mcr_tbl force; run;
    %end;

    /* Free WORK immediately */
    proc datasets lib=work nolist; delete _mcr_tbl; quit;

    %put NOTE: Medicare &name. (&tbl.) complete.;
%mend;

/* Part B Carrier: 12 DX fields, no admtg_dgns_cd */
%get_mcr(prtb, PRTB_CAR, 12, 0, first=1);

/* Outpatient: 25 DX fields */
%get_mcr(out, OUT, 25, 0);

/* Inpatient: 25 DX fields + admtg_dgns_cd */
%get_mcr(inp, INP, 25, 1);

/* SNF: 25 DX fields + admtg_dgns_cd */
%get_mcr(snf, SNF, 25, 1);

/* HHA: 25 DX fields */
%get_mcr(hha, HHA, 25, 0);

/* Hospice: 25 DX fields */
%get_mcr(hsp, HSP, 25, 0);

/* DME: 12 DX fields */
%get_mcr(dme, DME, 12, 0);

/* ======================================================================= */
/* PART E: Medicare Final Aggregation (across all 7 tables)                 */
/*   A patient may appear in multiple tables; combine their stats.          */
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
           min(first_dm_date) as first_dm_date format=yymmdd10.,
           min(first_dm_year) as first_dm_year,
           max(last_dm_year) as last_dm_year,
           sum(n_dm_claims) as n_dm_claims,
           'MEDICARE' as data_source length=10
    from mylib.mcr_patient_tbl
    group by bene_id
    having max(has_t1d) = 1 or max(has_t2d) = 1;
quit;

/* E-2: DFU cohort — restricted to DM patients */
proc sql;
    create table dfu_cohort_medicare as
    select a.bene_id,
           min(a.first_dfu_date) as first_dfu_date format=yymmdd10.,
           min(a.first_dfu_year) as first_dfu_year,
           max(a.last_dfu_year) as last_dfu_year,
           sum(a.n_dfu_claims) as n_dfu_claims,
           max(a.has_l97) as ever_l97,
           max(a.has_dm_combo) as ever_dm_combo,
           max(a.max_severity_rank) as max_severity_rank,
           'MEDICARE' as data_source length=10
    from mylib.mcr_patient_tbl a
    inner join dm_cohort_medicare b
        on a.bene_id = b.bene_id
    where a.has_l97 = 1 or a.has_dm_combo = 1
    group by a.bene_id;
quit;

/* Free Medicare intermediate */
proc datasets lib=mylib nolist; delete mcr_patient_tbl; quit;

title "Step 3E: Medicare Cohort Counts";
proc sql;
    select 'DM patients' as metric, count(*) as n format=comma12.
        from dm_cohort_medicare
    union all
    select 'DFU patients', count(*) from dfu_cohort_medicare;
quit;

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

 1. MEMORY OPTIMIZATION: The database performs GROUP BY, returning 1 row per
    patient (not per claim). A table with 10M claims and 300K diabetic
    patients returns only ~300K rows through ODBC instead of ~1M claim rows.

 2. INCREMENTAL PROCESSING: Each table is queried independently. Results are
    appended to D:\WPWatson (not WORK), and the temp table is deleted
    immediately. At most one temp table exists in WORK at any time.

 3. PASSTHROUGH BENEFITS: WHERE filtering runs on the database server. The
    database scans the table once, applies the WHERE, computes aggregates,
    and returns the small result set. SAS never sees the raw claims.

 4. SEVERITY COMPUTATION: L97 severity is computed per-row in the database
    SQL (CASE on 6th character of each L97 code found). MAX() in GROUP BY
    takes the worst severity across all claims for each patient.

 5. DFU RESTRICTION: The DFU cohort is restricted to patients in the DM
    cohort via INNER JOIN during final aggregation. L97 claims from
    non-diabetic patients are excluded.

 6. COLUMN CONTRACT: The 4 output datasets match the exact column names
    expected by step5_zip_extract.sas. No changes to step5 needed.

 7. POSTGRESQL FUNCTIONS: This code uses length() and extract(year from ...).
    If the database is SQL Server, replace:
      length(col) -> len(col)
      extract(year from col) -> year(col)

 8. AMBIGUOUS DM TYPE: Patients with both E10.x and E11.x across their
    full claim history are flagged as AMBIGUOUS per study protocol.
*****************************************************************************/
