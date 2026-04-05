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
   Part A:  Helper macros (DX field conditions + severity)
   Part B:  Commercial passthrough w/ GROUP BY (8 year tables -> D:\WPWatson)
   Part C:  Commercial final aggregation -> WORK.dm/dfu_cohort_commercial
   Part D:  Medicare passthrough w/ GROUP BY (7 claim tables -> D:\WPWatson)
   Part E:  Medicare final aggregation -> WORK.dm/dfu_cohort_medicare
   Part F:  Cross-validation with BEN_SUM_CC
   Part P1: Commercial procedure extraction (standalone, procedure-code-filtered)
   Part P2: Medicare procedure extraction (standalone, procedure-code-filtered)
   Part P3: Join procedures to DM + DFU cohorts (restricts to DM patients)
   Part T:  Tier 2 claim-level temporal matching (L97 + debridement ±30d)
   Part G:  Summary reports + cleanup

 ICD-10-CM Codes:
   Diabetes:  E10.x (T1D), E11.x (T2D)
   DFU:       L97.1xx-L97.9xx (non-pressure chronic ulcer of lower limb)
   DFU combo: E10.621, E10.622, E11.621, E11.622

 Procedure Codes (extracted from all claims, then restricted to DM patients in P3):
   Debridement CPT: 97597-97598 (selective), 11042-11047 (excisional), 97602
   Amputation CPT:  27590-27598 (above-knee), 28800-28825 (below-knee/toe)
   Amputation PCS:  0Y6x (ICD-10-PCS detachment of lower extremity)

 Procedure Fields:
   Commercial CPT/HCPCS:  mc055 (NOT mc024 which is NPI)
   Commercial ICD-10-PCS: mc058
   Medicare carrier line:  hcpcs_cd on APCD_MCR_PRTB_CAR_LIN (date: line_1st_expns_dt)
   Medicare outpatient rev: hcpcs_cd on APCD_MCR_OUT_REV (date: rev_cntr_dt)
   Medicare INP/SNF claim: icd_prcdr_cd1..25 on APCD_MCR_INP_CLM / SNF_CLM

 Output (4 WORK datasets consumed by step5):
   dm_cohort_commercial:  submitter, group_policy, person_code, dm_type,
                          first_dm_year, last_dm_year, n_dm_claims, first_dm_date,
                          has_debridement, first_debride_date, n_debride_claims,
                          has_amputation, first_amp_date, n_amp_claims,
                          tier2_temporal
   dm_cohort_medicare:    bene_id, dm_type, first_dm_year, last_dm_year,
                          n_dm_claims, first_dm_date,
                          has_debridement, first_debride_date, n_debride_claims,
                          has_amputation, first_amp_date, n_amp_claims,
                          tier2_temporal
   dfu_cohort_commercial: submitter, group_policy, person_code, first_dfu_year,
                          first_dfu_date, last_dfu_year, n_dfu_claims,
                          ever_l97, ever_dm_combo, max_severity_rank,
                          has_debridement, first_debride_date, n_debride_claims,
                          has_amputation, first_amp_date, n_amp_claims,
                          tier2_temporal
   dfu_cohort_medicare:   bene_id, (same procedure fields as commercial)

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
/*   DX-only — procedure codes extracted separately in Part P1.             */
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

/* C-2: DFU cohort — restricted to DM patients (no procedure fields yet) */
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
/*   DX-only — procedure codes extracted separately in Part P2.             */
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
/*   NO procedure fields — those come from Part P2.                         */
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

/* E-2: DFU cohort — restricted to DM patients (no procedure fields yet) */
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
/* PART P1: Commercial Procedure Extraction                                 */
/*   Scans commercial claims for debridement + amputation CPT/PCS.          */
/*   Filtered by procedure code WHERE clause (very selective, returns small */
/*   result set). Restriction to DM patients happens in Part P3 via join.   */
/*   mc055 = CPT/HCPCS, mc058 = ICD-10-PCS, mc017 = service date           */
/*   Produces 1 row per patient across all years.                           */
/* ======================================================================= */

%macro get_comm_proc(yr, first=0);
    proc sql;
        connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
        create table _comm_proc_yr as
        select * from connection to odbc (
            select mc001 as submitter,
                   mc006 as group_policy,
                   mc009 as person_code,
                   /* Debridement CPT (mc055) */
                   max(case when upper(mc055) in (
                       &q.97597&q.,&q.97598&q.,&q.11042&q.,&q.11043&q.,
                       &q.11044&q.,&q.11045&q.,&q.11046&q.,&q.11047&q.,&q.97602&q.
                   ) then 1 else 0 end) as has_debridement,
                   sum(case when upper(mc055) in (
                       &q.97597&q.,&q.97598&q.,&q.11042&q.,&q.11043&q.,
                       &q.11044&q.,&q.11045&q.,&q.11046&q.,&q.11047&q.,&q.97602&q.
                   ) then 1 else 0 end) as n_debride_claims,
                   min(case when upper(mc055) in (
                       &q.97597&q.,&q.97598&q.,&q.11042&q.,&q.11043&q.,
                       &q.11044&q.,&q.11045&q.,&q.11046&q.,&q.11047&q.,&q.97602&q.
                   ) then mc017 else null end) as first_debride_date,
                   /* Amputation CPT (mc055) */
                   max(case when upper(mc055) in (
                       &q.27590&q.,&q.27591&q.,&q.27592&q.,&q.27594&q.,&q.27596&q.,&q.27598&q.,
                       &q.28800&q.,&q.28805&q.,&q.28810&q.,&q.28820&q.,&q.28825&q.
                   ) then 1 else 0 end) as has_amp_cpt,
                   sum(case when upper(mc055) in (
                       &q.27590&q.,&q.27591&q.,&q.27592&q.,&q.27594&q.,&q.27596&q.,&q.27598&q.,
                       &q.28800&q.,&q.28805&q.,&q.28810&q.,&q.28820&q.,&q.28825&q.
                   ) then 1 else 0 end) as n_amp_cpt_claims,
                   min(case when upper(mc055) in (
                       &q.27590&q.,&q.27591&q.,&q.27592&q.,&q.27594&q.,&q.27596&q.,&q.27598&q.,
                       &q.28800&q.,&q.28805&q.,&q.28810&q.,&q.28820&q.,&q.28825&q.
                   ) then mc017 else null end) as first_amp_cpt_date,
                   /* Amputation ICD-10-PCS (mc058) */
                   max(case when upper(mc058) like &q.0Y6%&q. then 1 else 0 end) as has_amp_pcs,
                   sum(case when upper(mc058) like &q.0Y6%&q. then 1 else 0 end) as n_amp_pcs_claims,
                   min(case when upper(mc058) like &q.0Y6%&q. then mc017 else null end) as first_amp_pcs_date
            from public.CLAIM_SVC_DT_&yr.
            where upper(mc055) in (
                &q.97597&q.,&q.97598&q.,&q.11042&q.,&q.11043&q.,
                &q.11044&q.,&q.11045&q.,&q.11046&q.,&q.11047&q.,&q.97602&q.,
                &q.27590&q.,&q.27591&q.,&q.27592&q.,&q.27594&q.,&q.27596&q.,&q.27598&q.,
                &q.28800&q.,&q.28805&q.,&q.28810&q.,&q.28820&q.,&q.28825&q.
            )
            or upper(mc058) like &q.0Y6%&q.
            group by mc001, mc006, mc009
        );
        disconnect from odbc;
    quit;

    %if &first = 1 %then %do;
        data mylib.comm_proc_year; set _comm_proc_yr; run;
    %end;
    %else %do;
        proc append base=mylib.comm_proc_year data=_comm_proc_yr force; run;
    %end;
    proc datasets lib=work nolist; delete _comm_proc_yr; quit;
    %put NOTE: Commercial procedures &yr. complete.;
%mend;

%get_comm_proc(2017, first=1);
%get_comm_proc(2018);
%get_comm_proc(2019);
%get_comm_proc(2020);
%get_comm_proc(2021);
%get_comm_proc(2022);
%get_comm_proc(2023);
%get_comm_proc(2024);

/* Aggregate commercial procedures across years */
proc sql;
    create table mylib._comm_proc_agg as
    select submitter, group_policy, person_code,
           max(has_debridement) as has_debridement,
           sum(n_debride_claims) as n_debride_claims,
           min(first_debride_date) as first_debride_date format=yymmdd10.,
           /* Amputation: combine CPT + PCS */
           case when max(has_amp_cpt) = 1 or max(has_amp_pcs) = 1
                then 1 else 0 end as has_amputation,
           sum(n_amp_cpt_claims) + sum(n_amp_pcs_claims) as n_amp_claims,
           min(case when first_amp_cpt_date is not null then first_amp_cpt_date
                    else first_amp_pcs_date end) as first_amp_date format=yymmdd10.
    from mylib.comm_proc_year
    group by submitter, group_policy, person_code;
quit;
proc datasets lib=mylib nolist; delete comm_proc_year; quit;

%put NOTE: P1 Commercial procedure extraction complete.;

title "Step 3P1: Commercial Procedure Extraction";
proc sql;
    select 'Patients with debridement' as metric, count(*) as n format=comma12.
    from mylib._comm_proc_agg where has_debridement = 1
    union all
    select 'Patients with amputation', count(*)
    from mylib._comm_proc_agg where has_amputation = 1;
quit;

/* ======================================================================= */
/* PART P2: Medicare Procedure Extraction                                   */
/*   Queries 3 sources:                                                     */
/*     1. APCD_MCR_PRTB_CAR_LIN: hcpcs_cd (debridement + amputation CPT)   */
/*     2. APCD_MCR_OUT_REV: hcpcs_cd (debridement + amputation CPT)         */
/*     3. APCD_MCR_INP_CLM: icd_prcdr_cd1..25 (amputation PCS 0Y6x)        */
/*        APCD_MCR_SNF_CLM: icd_prcdr_cd1..25 (amputation PCS 0Y6x)        */
/*   Restriction to DM patients happens in Part P3 via INNER JOIN.          */
/* ======================================================================= */

/* P2-1: Carrier line HCPCS (debridement + amputation CPT) */
proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._mcr_proc_car as
    select * from connection to odbc (
        select bene_id,
               max(case when upper(hcpcs_cd) in (
                   &q.97597&q.,&q.97598&q.,&q.11042&q.,&q.11043&q.,
                   &q.11044&q.,&q.11045&q.,&q.11046&q.,&q.11047&q.,&q.97602&q.
               ) then 1 else 0 end) as has_debridement,
               sum(case when upper(hcpcs_cd) in (
                   &q.97597&q.,&q.97598&q.,&q.11042&q.,&q.11043&q.,
                   &q.11044&q.,&q.11045&q.,&q.11046&q.,&q.11047&q.,&q.97602&q.
               ) then 1 else 0 end) as n_debride_claims,
               min(case when upper(hcpcs_cd) in (
                   &q.97597&q.,&q.97598&q.,&q.11042&q.,&q.11043&q.,
                   &q.11044&q.,&q.11045&q.,&q.11046&q.,&q.11047&q.,&q.97602&q.
               ) then line_1st_expns_dt else null end) as first_debride_date,
               max(case when upper(hcpcs_cd) in (
                   &q.27590&q.,&q.27591&q.,&q.27592&q.,&q.27594&q.,&q.27596&q.,&q.27598&q.,
                   &q.28800&q.,&q.28805&q.,&q.28810&q.,&q.28820&q.,&q.28825&q.
               ) then 1 else 0 end) as has_amp_cpt,
               sum(case when upper(hcpcs_cd) in (
                   &q.27590&q.,&q.27591&q.,&q.27592&q.,&q.27594&q.,&q.27596&q.,&q.27598&q.,
                   &q.28800&q.,&q.28805&q.,&q.28810&q.,&q.28820&q.,&q.28825&q.
               ) then 1 else 0 end) as n_amp_cpt_claims,
               min(case when upper(hcpcs_cd) in (
                   &q.27590&q.,&q.27591&q.,&q.27592&q.,&q.27594&q.,&q.27596&q.,&q.27598&q.,
                   &q.28800&q.,&q.28805&q.,&q.28810&q.,&q.28820&q.,&q.28825&q.
               ) then line_1st_expns_dt else null end) as first_amp_cpt_date
        from public.APCD_MCR_PRTB_CAR_LIN
        where (upper(hcpcs_cd) in (
            &q.97597&q.,&q.97598&q.,&q.11042&q.,&q.11043&q.,
            &q.11044&q.,&q.11045&q.,&q.11046&q.,&q.11047&q.,&q.97602&q.,
            &q.27590&q.,&q.27591&q.,&q.27592&q.,&q.27594&q.,&q.27596&q.,&q.27598&q.,
            &q.28800&q.,&q.28805&q.,&q.28810&q.,&q.28820&q.,&q.28825&q.
        ))
        and line_1st_expns_dt < &q.2025-01-01&q.
        group by bene_id
    );
    disconnect from odbc;
quit;
%put NOTE: P2-1 Carrier line HCPCS complete.;

/* P2-2: Outpatient revenue center HCPCS (debridement + amputation CPT) */
proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._mcr_proc_out as
    select * from connection to odbc (
        select bene_id,
               max(case when upper(hcpcs_cd) in (
                   &q.97597&q.,&q.97598&q.,&q.11042&q.,&q.11043&q.,
                   &q.11044&q.,&q.11045&q.,&q.11046&q.,&q.11047&q.,&q.97602&q.
               ) then 1 else 0 end) as has_debridement,
               sum(case when upper(hcpcs_cd) in (
                   &q.97597&q.,&q.97598&q.,&q.11042&q.,&q.11043&q.,
                   &q.11044&q.,&q.11045&q.,&q.11046&q.,&q.11047&q.,&q.97602&q.
               ) then 1 else 0 end) as n_debride_claims,
               min(case when upper(hcpcs_cd) in (
                   &q.97597&q.,&q.97598&q.,&q.11042&q.,&q.11043&q.,
                   &q.11044&q.,&q.11045&q.,&q.11046&q.,&q.11047&q.,&q.97602&q.
               ) then rev_cntr_dt else null end) as first_debride_date,
               max(case when upper(hcpcs_cd) in (
                   &q.27590&q.,&q.27591&q.,&q.27592&q.,&q.27594&q.,&q.27596&q.,&q.27598&q.,
                   &q.28800&q.,&q.28805&q.,&q.28810&q.,&q.28820&q.,&q.28825&q.
               ) then 1 else 0 end) as has_amp_cpt,
               sum(case when upper(hcpcs_cd) in (
                   &q.27590&q.,&q.27591&q.,&q.27592&q.,&q.27594&q.,&q.27596&q.,&q.27598&q.,
                   &q.28800&q.,&q.28805&q.,&q.28810&q.,&q.28820&q.,&q.28825&q.
               ) then 1 else 0 end) as n_amp_cpt_claims,
               min(case when upper(hcpcs_cd) in (
                   &q.27590&q.,&q.27591&q.,&q.27592&q.,&q.27594&q.,&q.27596&q.,&q.27598&q.,
                   &q.28800&q.,&q.28805&q.,&q.28810&q.,&q.28820&q.,&q.28825&q.
               ) then rev_cntr_dt else null end) as first_amp_cpt_date
        from public.APCD_MCR_OUT_REV
        where (upper(hcpcs_cd) in (
            &q.97597&q.,&q.97598&q.,&q.11042&q.,&q.11043&q.,
            &q.11044&q.,&q.11045&q.,&q.11046&q.,&q.11047&q.,&q.97602&q.,
            &q.27590&q.,&q.27591&q.,&q.27592&q.,&q.27594&q.,&q.27596&q.,&q.27598&q.,
            &q.28800&q.,&q.28805&q.,&q.28810&q.,&q.28820&q.,&q.28825&q.
        ))
        and rev_cntr_dt < &q.2025-01-01&q.
        group by bene_id
    );
    disconnect from odbc;
quit;
%put NOTE: P2-2 Outpatient revenue HCPCS complete.;

/* P2-3: Inpatient + SNF ICD-10-PCS amputation (0Y6x on icd_prcdr_cd fields) */
%macro mcr_pcs_amp(max_prcdr);
    upper(icd_prcdr_cd1) like &q.0Y6%&q.
    %do i = 2 %to &max_prcdr;
        or upper(icd_prcdr_cd&i.) like &q.0Y6%&q.
    %end;
%mend;

%macro get_mcr_pcs(name, tbl, max_prcdr, first=0);
    proc sql;
        connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
        create table _mcr_pcs as
        select * from connection to odbc (
            select bene_id,
                   1 as has_amp_pcs,
                   count(*) as n_amp_pcs_claims,
                   min(clm_from_dt) as first_amp_pcs_date
            from public.APCD_MCR_&tbl._CLM
            where (%mcr_pcs_amp(&max_prcdr.))
              and clm_from_dt < &q.2025-01-01&q.
            group by bene_id
        );
        disconnect from odbc;
    quit;

    %if &first = 1 %then %do;
        data mylib._mcr_proc_pcs; set _mcr_pcs; run;
    %end;
    %else %do;
        proc append base=mylib._mcr_proc_pcs data=_mcr_pcs force; run;
    %end;
    proc datasets lib=work nolist; delete _mcr_pcs; quit;
    %put NOTE: P2-3 &name. PCS amputation complete.;
%mend;

%get_mcr_pcs(INP, INP, 25, first=1);
%get_mcr_pcs(SNF, SNF, 25);

/* P2-4: Combine all Medicare procedure sources */
proc sql;
    create table mylib._mcr_proc_agg as
    select coalesce(c.bene_id, o.bene_id, p.bene_id) as bene_id,
           /* Debridement: from carrier line + outpatient rev */
           case when max(coalesce(c.has_debridement, 0)) = 1
                  or max(coalesce(o.has_debridement, 0)) = 1
                then 1 else 0 end as has_debridement,
           sum(coalesce(c.n_debride_claims, 0))
               + sum(coalesce(o.n_debride_claims, 0)) as n_debride_claims,
           min(case when c.first_debride_date is not null
                         and o.first_debride_date is not null
                    then case when c.first_debride_date <= o.first_debride_date
                              then c.first_debride_date else o.first_debride_date end
                    when c.first_debride_date is not null then c.first_debride_date
                    else o.first_debride_date
               end) as first_debride_date format=yymmdd10.,
           /* Amputation: from carrier + outpatient CPT + INP/SNF PCS */
           case when max(coalesce(c.has_amp_cpt, 0)) = 1
                  or max(coalesce(o.has_amp_cpt, 0)) = 1
                  or max(coalesce(p.has_amp_pcs, 0)) = 1
                then 1 else 0 end as has_amputation,
           sum(coalesce(c.n_amp_cpt_claims, 0))
               + sum(coalesce(o.n_amp_cpt_claims, 0))
               + sum(coalesce(p.n_amp_pcs_claims, 0)) as n_amp_claims,
           min(c.first_amp_cpt_date, o.first_amp_cpt_date,
               p.first_amp_pcs_date) as first_amp_date format=yymmdd10.
    from mylib._mcr_proc_car c
    full outer join mylib._mcr_proc_out o on c.bene_id = o.bene_id
    full outer join mylib._mcr_proc_pcs p on coalesce(c.bene_id, o.bene_id) = p.bene_id
    group by calculated bene_id;
quit;
proc datasets lib=mylib nolist;
    delete _mcr_proc_car _mcr_proc_out _mcr_proc_pcs;
quit;

%put NOTE: P2 Medicare procedure extraction complete.;

title "Step 3P2: Medicare Procedure Extraction";
proc sql;
    select 'Patients with debridement' as metric, count(*) as n format=comma12.
    from mylib._mcr_proc_agg where has_debridement = 1
    union all
    select 'Patients with amputation', count(*)
    from mylib._mcr_proc_agg where has_amputation = 1;
quit;

/* ======================================================================= */
/* PART P3: Join Procedures to DM + DFU Cohorts                             */
/*   INNER JOIN to DM cohort restricts procedures to diabetic patients.     */
/*   This implements the "require DM, THEN look for procedures" logic.      */
/*   P1/P2 returned all patients with procedure codes; P3 keeps only those  */
/*   who are in the DM cohort (discarding non-diabetic procedure patients). */
/*                                                                          */
/*   DM cohorts get: amputation fields (for censoring)                      */
/*                   debridement fields (for Tier 2 on all DM, carried to   */
/*                   DFU via step5 merge)                                   */
/*   DFU cohorts get: debridement + amputation (for Tier 2 + censoring)     */
/* ======================================================================= */

/* P3-1: Commercial DM + procedures (LEFT JOIN — most DM patients have no procedures) */
proc sql;
    create table _dm_comm_proc as
    select a.*,
           coalesce(p.has_debridement, 0) as has_debridement,
           p.first_debride_date,
           coalesce(p.n_debride_claims, 0) as n_debride_claims,
           coalesce(p.has_amputation, 0) as has_amputation,
           p.first_amp_date,
           coalesce(p.n_amp_claims, 0) as n_amp_claims
    from dm_cohort_commercial a
    left join mylib._comm_proc_agg p
        on a.submitter = p.submitter
       and a.group_policy = p.group_policy
       and a.person_code = p.person_code;
quit;

proc sql; drop table dm_cohort_commercial; quit;
proc sql;
    create table dm_cohort_commercial as select * from _dm_comm_proc;
quit;
proc datasets lib=work nolist; delete _dm_comm_proc; quit;

/* P3-2: Commercial DFU + procedures */
proc sql;
    create table _dfu_comm_proc as
    select a.*,
           coalesce(p.has_debridement, 0) as has_debridement,
           p.first_debride_date,
           coalesce(p.n_debride_claims, 0) as n_debride_claims,
           coalesce(p.has_amputation, 0) as has_amputation,
           p.first_amp_date,
           coalesce(p.n_amp_claims, 0) as n_amp_claims
    from dfu_cohort_commercial a
    left join mylib._comm_proc_agg p
        on a.submitter = p.submitter
       and a.group_policy = p.group_policy
       and a.person_code = p.person_code;
quit;

proc sql; drop table dfu_cohort_commercial; quit;
proc sql;
    create table dfu_cohort_commercial as select * from _dfu_comm_proc;
quit;
proc datasets lib=work nolist; delete _dfu_comm_proc; quit;
proc datasets lib=mylib nolist; delete _comm_proc_agg; quit;

/* P3-3: Medicare DM + procedures */
proc sql;
    create table _dm_mcr_proc as
    select a.*,
           coalesce(p.has_debridement, 0) as has_debridement,
           p.first_debride_date,
           coalesce(p.n_debride_claims, 0) as n_debride_claims,
           coalesce(p.has_amputation, 0) as has_amputation,
           p.first_amp_date,
           coalesce(p.n_amp_claims, 0) as n_amp_claims
    from dm_cohort_medicare a
    left join mylib._mcr_proc_agg p
        on a.bene_id = p.bene_id;
quit;

proc sql; drop table dm_cohort_medicare; quit;
proc sql;
    create table dm_cohort_medicare as select * from _dm_mcr_proc;
quit;
proc datasets lib=work nolist; delete _dm_mcr_proc; quit;

/* P3-4: Medicare DFU + procedures */
proc sql;
    create table _dfu_mcr_proc as
    select a.*,
           coalesce(p.has_debridement, 0) as has_debridement,
           p.first_debride_date,
           coalesce(p.n_debride_claims, 0) as n_debride_claims,
           coalesce(p.has_amputation, 0) as has_amputation,
           p.first_amp_date,
           coalesce(p.n_amp_claims, 0) as n_amp_claims
    from dfu_cohort_medicare a
    left join mylib._mcr_proc_agg p
        on a.bene_id = p.bene_id;
quit;

proc sql; drop table dfu_cohort_medicare; quit;
proc sql;
    create table dfu_cohort_medicare as select * from _dfu_mcr_proc;
quit;
proc datasets lib=work nolist; delete _dfu_mcr_proc; quit;
proc datasets lib=mylib nolist; delete _mcr_proc_agg; quit;

%put NOTE: P3 Procedure join to DM + DFU cohorts complete.;

/* ======================================================================= */
/* PART T: Tier 2 Temporal Matching (claim-level L97 + debridement ±30d)    */
/*                                                                          */
/*   Extracts distinct (patient, claim_date) pairs for L97 diagnoses and    */
/*   for debridement procedures, then performs a claim-level temporal       */
/*   match in SAS SQL: a patient qualifies for Tier 2 temporal if any       */
/*   debridement occurs within [L97_date, L97_date + 30 days]. Forward-     */
/*   only window per Barshes et al. This addresses the limitation that     */
/*   P1/P2 return only first_debride_date per patient.                     */
/*                                                                          */
/*   Cross-year matches are handled automatically because L97 and           */
/*   debridement date tables are appended across all years before the       */
/*   SAS-side join.                                                         */
/*                                                                          */
/*   Output:                                                                */
/*     tier2_temporal flag added to dm_cohort_* and dfu_cohort_*            */
/* ======================================================================= */

/* ----------------------------------------------------------------------- */
/* T1: Commercial L97 claim dates (one row per patient per unique date)    */
/* ----------------------------------------------------------------------- */
%macro get_comm_l97_dates(yr, first=0);
    proc sql;
        connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
        create table _comm_l97_dt as
        select * from connection to odbc (
            select distinct mc001 as submitter,
                            mc006 as group_policy,
                            mc009 as person_code,
                            mc017 as claim_date
            from public.CLAIM_SVC_DT_&yr.
            where (%comm_like(L97))
        );
        disconnect from odbc;
    quit;

    %if &first = 1 %then %do;
        data mylib.comm_l97_dates; set _comm_l97_dt; run;
    %end;
    %else %do;
        proc append base=mylib.comm_l97_dates data=_comm_l97_dt force; run;
    %end;
    proc datasets lib=work nolist; delete _comm_l97_dt; quit;
    %put NOTE: T1 Commercial L97 dates &yr. complete.;
%mend;

%get_comm_l97_dates(2017, first=1);
%get_comm_l97_dates(2018);
%get_comm_l97_dates(2019);
%get_comm_l97_dates(2020);
%get_comm_l97_dates(2021);
%get_comm_l97_dates(2022);
%get_comm_l97_dates(2023);
%get_comm_l97_dates(2024);

/* ----------------------------------------------------------------------- */
/* T2: Commercial debridement claim dates (one row per patient per date)   */
/* ----------------------------------------------------------------------- */
%macro get_comm_dbr_dates(yr, first=0);
    proc sql;
        connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
        create table _comm_dbr_dt as
        select * from connection to odbc (
            select distinct mc001 as submitter,
                            mc006 as group_policy,
                            mc009 as person_code,
                            mc017 as claim_date
            from public.CLAIM_SVC_DT_&yr.
            where upper(mc055) in (
                &q.97597&q.,&q.97598&q.,&q.11042&q.,&q.11043&q.,
                &q.11044&q.,&q.11045&q.,&q.11046&q.,&q.11047&q.,&q.97602&q.
            )
        );
        disconnect from odbc;
    quit;

    %if &first = 1 %then %do;
        data mylib.comm_dbr_dates; set _comm_dbr_dt; run;
    %end;
    %else %do;
        proc append base=mylib.comm_dbr_dates data=_comm_dbr_dt force; run;
    %end;
    proc datasets lib=work nolist; delete _comm_dbr_dt; quit;
    %put NOTE: T2 Commercial debridement dates &yr. complete.;
%mend;

%get_comm_dbr_dates(2017, first=1);
%get_comm_dbr_dates(2018);
%get_comm_dbr_dates(2019);
%get_comm_dbr_dates(2020);
%get_comm_dbr_dates(2021);
%get_comm_dbr_dates(2022);
%get_comm_dbr_dates(2023);
%get_comm_dbr_dates(2024);

/* ----------------------------------------------------------------------- */
/* T3: Commercial temporal match (SAS SQL — handles cross-year)            */
/*     tier2_temporal = 1 if any debridement within [L97, L97+30d]         */
/* ----------------------------------------------------------------------- */
proc sql;
    create table mylib._tier2_temporal_comm as
    select distinct L.submitter, L.group_policy, L.person_code,
           1 as tier2_temporal
    from mylib.comm_l97_dates L
    inner join mylib.comm_dbr_dates D
        on L.submitter = D.submitter
       and L.group_policy = D.group_policy
       and L.person_code = D.person_code
    where D.claim_date - L.claim_date between 0 and 30;
quit;

proc datasets lib=mylib nolist;
    delete comm_l97_dates comm_dbr_dates;
quit;

%put NOTE: T3 Commercial Tier 2 temporal match complete.;

title "Step 3T3: Commercial Tier 2 Temporal Match";
proc sql;
    select 'Patients with L97 + debridement within 30d' as metric,
           count(*) as n format=comma12.
    from mylib._tier2_temporal_comm;
quit;

/* ----------------------------------------------------------------------- */
/* T4: Medicare L97 claim dates from 7 claim tables                        */
/* ----------------------------------------------------------------------- */
%macro get_mcr_l97_dates(name, tbl, max_dx, has_admtg, first=0);
    proc sql;
        connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
        create table _mcr_l97_dt as
        select * from connection to odbc (
            select distinct bene_id,
                            clm_from_dt as claim_date
            from public.APCD_MCR_&tbl._CLM
            where (%mcr_like(L97, &max_dx., &has_admtg.))
              and clm_from_dt < &q.2025-01-01&q.
        );
        disconnect from odbc;
    quit;

    %if &first = 1 %then %do;
        data mylib.mcr_l97_dates; set _mcr_l97_dt; run;
    %end;
    %else %do;
        proc append base=mylib.mcr_l97_dates data=_mcr_l97_dt force; run;
    %end;
    proc datasets lib=work nolist; delete _mcr_l97_dt; quit;
    %put NOTE: T4 Medicare L97 dates &name. complete.;
%mend;

%get_mcr_l97_dates(prtb, PRTB_CAR, 12, 0, first=1);
%get_mcr_l97_dates(out,  OUT,      25, 0);
%get_mcr_l97_dates(inp,  INP,      25, 1);
%get_mcr_l97_dates(snf,  SNF,      25, 1);
%get_mcr_l97_dates(hha,  HHA,      25, 0);
%get_mcr_l97_dates(hsp,  HSP,      25, 0);
%get_mcr_l97_dates(dme,  DME,      12, 0);

/* ----------------------------------------------------------------------- */
/* T5: Medicare debridement claim dates from 2 HCPCS tables                */
/* ----------------------------------------------------------------------- */
/* T5-1: Carrier line (line_1st_expns_dt) */
proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._mcr_dbr_car as
    select * from connection to odbc (
        select distinct bene_id,
                        line_1st_expns_dt as claim_date
        from public.APCD_MCR_PRTB_CAR_LIN
        where upper(hcpcs_cd) in (
            &q.97597&q.,&q.97598&q.,&q.11042&q.,&q.11043&q.,
            &q.11044&q.,&q.11045&q.,&q.11046&q.,&q.11047&q.,&q.97602&q.
        )
          and line_1st_expns_dt < &q.2025-01-01&q.
    );
    disconnect from odbc;
quit;
%put NOTE: T5-1 Medicare debridement dates (carrier) complete.;

/* T5-2: Outpatient revenue (rev_cntr_dt) */
proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._mcr_dbr_out as
    select * from connection to odbc (
        select distinct bene_id,
                        rev_cntr_dt as claim_date
        from public.APCD_MCR_OUT_REV
        where upper(hcpcs_cd) in (
            &q.97597&q.,&q.97598&q.,&q.11042&q.,&q.11043&q.,
            &q.11044&q.,&q.11045&q.,&q.11046&q.,&q.11047&q.,&q.97602&q.
        )
          and rev_cntr_dt < &q.2025-01-01&q.
    );
    disconnect from odbc;
quit;
%put NOTE: T5-2 Medicare debridement dates (outpatient rev) complete.;

/* T5-3: Union carrier + outpatient debridement dates */
proc sql;
    create table mylib.mcr_dbr_dates as
    select bene_id, claim_date from mylib._mcr_dbr_car
    union
    select bene_id, claim_date from mylib._mcr_dbr_out;
quit;
proc datasets lib=mylib nolist;
    delete _mcr_dbr_car _mcr_dbr_out;
quit;

/* ----------------------------------------------------------------------- */
/* T6: Medicare temporal match (SAS SQL)                                   */
/* ----------------------------------------------------------------------- */
proc sql;
    create table mylib._tier2_temporal_mcr as
    select distinct L.bene_id,
           1 as tier2_temporal
    from mylib.mcr_l97_dates L
    inner join mylib.mcr_dbr_dates D
        on L.bene_id = D.bene_id
    where D.claim_date - L.claim_date between 0 and 30;
quit;

proc datasets lib=mylib nolist;
    delete mcr_l97_dates mcr_dbr_dates;
quit;

%put NOTE: T6 Medicare Tier 2 temporal match complete.;

title "Step 3T6: Medicare Tier 2 Temporal Match";
proc sql;
    select 'Patients with L97 + debridement within 30d' as metric,
           count(*) as n format=comma12.
    from mylib._tier2_temporal_mcr;
quit;

/* ----------------------------------------------------------------------- */
/* T7: Join tier2_temporal flag to DM and DFU cohorts                      */
/* ----------------------------------------------------------------------- */

/* T7-1: Commercial DM cohort */
proc sql;
    create table _dm_comm_t2 as
    select a.*,
           coalesce(t.tier2_temporal, 0) as tier2_temporal
    from dm_cohort_commercial a
    left join mylib._tier2_temporal_comm t
        on a.submitter = t.submitter
       and a.group_policy = t.group_policy
       and a.person_code = t.person_code;
quit;
proc sql; drop table dm_cohort_commercial; quit;
proc sql; create table dm_cohort_commercial as select * from _dm_comm_t2; quit;
proc datasets lib=work nolist; delete _dm_comm_t2; quit;

/* T7-2: Commercial DFU cohort */
proc sql;
    create table _dfu_comm_t2 as
    select a.*,
           coalesce(t.tier2_temporal, 0) as tier2_temporal
    from dfu_cohort_commercial a
    left join mylib._tier2_temporal_comm t
        on a.submitter = t.submitter
       and a.group_policy = t.group_policy
       and a.person_code = t.person_code;
quit;
proc sql; drop table dfu_cohort_commercial; quit;
proc sql; create table dfu_cohort_commercial as select * from _dfu_comm_t2; quit;
proc datasets lib=work nolist; delete _dfu_comm_t2; quit;

proc datasets lib=mylib nolist; delete _tier2_temporal_comm; quit;

/* T7-3: Medicare DM cohort */
proc sql;
    create table _dm_mcr_t2 as
    select a.*,
           coalesce(t.tier2_temporal, 0) as tier2_temporal
    from dm_cohort_medicare a
    left join mylib._tier2_temporal_mcr t
        on a.bene_id = t.bene_id;
quit;
proc sql; drop table dm_cohort_medicare; quit;
proc sql; create table dm_cohort_medicare as select * from _dm_mcr_t2; quit;
proc datasets lib=work nolist; delete _dm_mcr_t2; quit;

/* T7-4: Medicare DFU cohort */
proc sql;
    create table _dfu_mcr_t2 as
    select a.*,
           coalesce(t.tier2_temporal, 0) as tier2_temporal
    from dfu_cohort_medicare a
    left join mylib._tier2_temporal_mcr t
        on a.bene_id = t.bene_id;
quit;
proc sql; drop table dfu_cohort_medicare; quit;
proc sql; create table dfu_cohort_medicare as select * from _dfu_mcr_t2; quit;
proc datasets lib=work nolist; delete _dfu_mcr_t2; quit;

proc datasets lib=mylib nolist; delete _tier2_temporal_mcr; quit;

%put NOTE: T7 tier2_temporal join to DM + DFU cohorts complete.;

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

title "Step 3G-5: Procedure Code Summary - DM Patients (all diabetics)";
proc sql;
    select 'COMMERCIAL' as source,
           count(*) as n_dm format=comma12.,
           sum(has_debridement) as with_debridement format=comma12.,
           sum(has_amputation) as with_amputation format=comma12.
    from dm_cohort_commercial
    union all
    select 'MEDICARE',
           count(*), sum(has_debridement), sum(has_amputation)
    from dm_cohort_medicare;
quit;

title "Step 3G-6: Procedure Code Summary - DFU Patients";
proc sql;
    select data_source,
           count(*) as n_dfu format=comma12.,
           sum(has_debridement) as with_debridement format=comma12.,
           sum(has_amputation) as with_amputation format=comma12.,
           sum(case when has_debridement = 1 or ever_dm_combo = 1
                    then 1 else 0 end) as tier2_eligible format=comma12.,
           sum(tier2_temporal) as tier2_temporal_match format=comma12.,
           sum(ever_dm_combo) as combo_only format=comma12.
    from (
        select data_source, has_debridement, has_amputation,
               ever_dm_combo, tier2_temporal
            from dfu_cohort_commercial
        union all
        select data_source, has_debridement, has_amputation,
               ever_dm_combo, tier2_temporal
            from dfu_cohort_medicare
    )
    group by data_source;
quit;

title "Step 3G-7: Tier 2 Temporal Match vs Any-Debridement Approximation";
proc sql;
    select data_source,
           sum(case when tier2_temporal = 1 then 1 else 0 end)
               as temporal_match format=comma12.,
           sum(case when has_debridement = 1 and ever_l97 = 1
                    and tier2_temporal = 0 then 1 else 0 end)
               as dbr_no_temporal_match format=comma12.,
           sum(case when ever_dm_combo = 1 and tier2_temporal = 0
                    then 1 else 0 end)
               as combo_only_no_temporal format=comma12.
    from (
        select data_source, ever_l97, ever_dm_combo,
               has_debridement, tier2_temporal
            from dfu_cohort_commercial
        union all
        select data_source, ever_l97, ever_dm_combo,
               has_debridement, tier2_temporal
            from dfu_cohort_medicare
    )
    group by data_source;
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

 3. PROCEDURE LOGIC — "REQUIRE DM, THEN LOOK FOR PROCEDURES":
    Parts P1/P2 scan all claims filtered by procedure code WHERE clause
    (very selective — returns only patients with debridement/amputation).
    Part P3 INNER JOINs (via LEFT JOIN + discard non-matches) these
    results to the DM and DFU cohorts from Parts A-E. Non-diabetic
    patients with procedures are discarded. Both DM cohorts (for
    amputation censoring) and DFU cohorts (for Tier 2 debridement
    confirmation) receive procedure fields.

 4. COMMERCIAL FIELD MAPPING: mc055 is the CPT/HCPCS field (NOT mc024,
    which contains NPI provider numbers). mc058 is the ICD-10-PCS field.
    Confirmed against 1% sample data.

 5. MEDICARE HCPCS SOURCES: Debridement and amputation CPT codes exist on
    two tables: APCD_MCR_PRTB_CAR_LIN (carrier line items, field hcpcs_cd,
    date: line_1st_expns_dt) and APCD_MCR_OUT_REV (outpatient revenue
    centers, field hcpcs_cd, date: rev_cntr_dt). ICD-10-PCS amputation
    (0Y6x) is on INP_CLM and SNF_CLM (icd_prcdr_cd1..25).

 6. TEMPORAL MATCHING (Part T): For each DM patient, Part T extracts
    distinct (patient, date) pairs for L97 diagnosis claims and for
    debridement procedure claims, then performs a claim-level temporal
    match in SAS SQL. A patient is flagged tier2_temporal = 1 if any
    debridement falls within [L97_date, L97_date + 30 days] — the
    forward-only window used by Barshes et al. This addresses the
    limitation that P1/P2 return only first_debride_date per patient.
    Cross-year matches are handled automatically because date tables
    are appended across all years before the SAS-side join.

 7. AMPUTATION CENSORING: The R pipeline applies amputation censoring
    (stop counting DFU after first amputation) using first_amp_date
    from the DM cohort.

 7. POSTGRESQL FUNCTIONS: This code uses length() and extract(year from ...).
    If the database is SQL Server, replace:
      length(col) -> len(col)
      extract(year from col) -> year(col)

 8. AMBIGUOUS DM TYPE: Patients with both E10.x and E11.x across their
    full claim history are flagged as AMBIGUOUS per study protocol.
*****************************************************************************/
