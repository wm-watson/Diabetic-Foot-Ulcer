/*****************************************************************************
 Step 3: Identify T1D and T2D Patients
 Purpose: Build patient-level diabetes cohort from all claims 2017–2024

 ICD-10-CM Codes:
   E10.x = Type 1 Diabetes Mellitus
   E11.x = Type 2 Diabetes Mellitus

 Data Sources:
   A. Commercial/Medicaid: CLAIM_SVC_DT_2017–2024
      14 diagnosis fields: mc039 (admitting dx), mc041 (principal dx),
      mc042–mc053 (dx2–dx13)
   B. Medicare Part B Carrier: APCD_MCR_PRTB_CAR_CLM (prncpal_dgns_cd,
      icd_dgns_cd1–12)
   C. Medicare Outpatient: APCD_MCR_OUT_CLM (prncpal_dgns_cd,
      icd_dgns_cd1–25)
   D. Medicare Inpatient: APCD_MCR_INP_CLM (similar multi-DX structure)
   E. Medicare SNF/HHA/HSP/DME claim tables

 Output:
   WORK.DM_COHORT_COMMERCIAL — commercial/Medicaid diabetic patients
   WORK.DM_COHORT_MEDICARE   — Medicare diabetic patients
   WORK.DM_COHORT_ALL        — combined, deduplicated, with flags

 Join Keys:
   Commercial: Composite key = mc001 (submitter) + mc006 (group/policy,
               hashed) + mc009 (person sequence).
               mc009 alone is just member sequence (1=subscriber, 2=spouse).
   Medicare:   BENE_ID = beneficiary ID

 NOTE: Patients with BOTH E10.x and E11.x codes are flagged as 'AMBIGUOUS'
       per instructions — ambiguity is NOT resolved here.
*****************************************************************************/

libname arapcd odbc
    noprompt="dsn=APCD-24D;Trusted_connection=yes"
    schema=public;

%let outdir = /Users/williamwatson/Claude_Code/Aim_3_Dissertation;

/* ======================================================================= */
/* PART A: Commercial/Medicaid — Scan CLAIM_SVC_DT tables                  */
/*         14 diagnosis fields: mc039, mc041–mc053                         */
/* ======================================================================= */

/*
   Commercial claims have 14 diagnosis fields:
     mc039 = admitting diagnosis
     mc041 = principal diagnosis
     mc042 = dx2, mc043 = dx3, ... mc053 = dx13
   We scan ALL 14 for E10.x / E11.x codes.

   Person-level key is the composite: mc001 (submitter) || mc006 (group/
   policy, hashed) || mc009 (member sequence number).
   mc009 alone is NOT unique — it is just 1=subscriber, 2=spouse, etc.

   Strategy: Use a macro to generate OR conditions across all 14 DX fields,
   then UNION ALL across all year tables.
*/

/* Macro: generate OR conditions for commercial DX fields matching a prefix */
%macro comm_dx_check(prefix);
    upcase(mc039) like "&prefix.%"
    or upcase(mc041) like "&prefix.%"
    %do i = 42 %to 53;
        or upcase(mc0&i.) like "&prefix.%"
    %end;
%mend comm_dx_check;

%macro scan_claim_year(yr);
    /* Extract diabetes-flagged claim lines from one year */
    select mc001 as submitter,
           mc006 as group_policy,
           mc009 as person_code,
           mc017 as service_date,
           &yr. as claim_year,
           case
               when (%comm_dx_check(E10))
                   then 1 else 0
           end as has_t1d,
           case
               when (%comm_dx_check(E11))
                   then 1 else 0
           end as has_t2d
    from arapcd.CLAIM_SVC_DT_&yr.
    where %comm_dx_check(E10)
       or %comm_dx_check(E11)
%mend scan_claim_year;

proc sql;
    create table dm_claims_commercial as
    %scan_claim_year(2017)
    union all
    %scan_claim_year(2018)
    union all
    %scan_claim_year(2019)
    union all
    %scan_claim_year(2020)
    union all
    %scan_claim_year(2021)
    union all
    %scan_claim_year(2022)
    union all
    %scan_claim_year(2023)
    union all
    %scan_claim_year(2024)
    ;
quit;

/* Aggregate to patient level using composite key */
proc sql;
    create table dm_cohort_commercial as
    select submitter,
           group_policy,
           person_code,
           max(has_t1d) as ever_t1d,
           max(has_t2d) as ever_t2d,
           min(service_date) as first_dm_date format=yymmdd10.,
           min(claim_year) as first_dm_year,
           max(claim_year) as last_dm_year,
           count(*) as n_dm_claims,
           /* Classify */
           case
               when max(has_t1d) = 1 and max(has_t2d) = 1 then 'AMBIGUOUS'
               when max(has_t1d) = 1 then 'T1D'
               when max(has_t2d) = 1 then 'T2D'
               else 'UNKNOWN'
           end as dm_type length=10,
           'COMMERCIAL' as data_source length=10
    from dm_claims_commercial
    group by submitter, group_policy, person_code;
quit;

title "Step 3A: Commercial/Medicaid Diabetes Cohort Summary";
proc sql;
    select dm_type,
           count(*) as n_patients format=comma12.,
           min(first_dm_year) as earliest_year,
           max(last_dm_year) as latest_year,
           sum(n_dm_claims) as total_claims format=comma15.
    from dm_cohort_commercial
    group by dm_type;
quit;

/* Year-level counts for commercial */
proc sql;
    create table dm_commercial_by_year as
    select claim_year,
           case
               when has_t1d = 1 and has_t2d = 1 then 'AMBIGUOUS'
               when has_t1d = 1 then 'T1D'
               when has_t2d = 1 then 'T2D'
               else 'UNKNOWN'
           end as dm_type length=10,
           count(distinct catx('|', submitter, group_policy, person_code)) as n_patients format=comma12.
    from dm_claims_commercial
    group by claim_year, calculated dm_type;
quit;

title "Step 3A: Commercial/Medicaid Diabetes Patients by Year and Type";
proc print data=dm_commercial_by_year noobs; run;

/* ======================================================================= */
/* PART B: Medicare — Scan multiple claim tables                           */
/* ======================================================================= */

/*
   Medicare tables have many more diagnosis fields than commercial.
   We scan: prncpal_dgns_cd + icd_dgns_cd1..N for each table.
   Strategy: Build macro for each Medicare claim table.
*/

/* --- Part B Carrier Claims (12 DX fields) ------------------------------ */
proc sql;
    create table dm_mcr_partb as
    select bene_id,
           clm_from_dt as service_date,
           year(clm_from_dt) as claim_year,
           case when (
               upcase(prncpal_dgns_cd) like 'E10%'
               or upcase(icd_dgns_cd1) like 'E10%'
               or upcase(icd_dgns_cd2) like 'E10%'
               or upcase(icd_dgns_cd3) like 'E10%'
               or upcase(icd_dgns_cd4) like 'E10%'
               or upcase(icd_dgns_cd5) like 'E10%'
               or upcase(icd_dgns_cd6) like 'E10%'
               or upcase(icd_dgns_cd7) like 'E10%'
               or upcase(icd_dgns_cd8) like 'E10%'
               or upcase(icd_dgns_cd9) like 'E10%'
               or upcase(icd_dgns_cd10) like 'E10%'
               or upcase(icd_dgns_cd11) like 'E10%'
               or upcase(icd_dgns_cd12) like 'E10%'
           ) then 1 else 0 end as has_t1d,
           case when (
               upcase(prncpal_dgns_cd) like 'E11%'
               or upcase(icd_dgns_cd1) like 'E11%'
               or upcase(icd_dgns_cd2) like 'E11%'
               or upcase(icd_dgns_cd3) like 'E11%'
               or upcase(icd_dgns_cd4) like 'E11%'
               or upcase(icd_dgns_cd5) like 'E11%'
               or upcase(icd_dgns_cd6) like 'E11%'
               or upcase(icd_dgns_cd7) like 'E11%'
               or upcase(icd_dgns_cd8) like 'E11%'
               or upcase(icd_dgns_cd9) like 'E11%'
               or upcase(icd_dgns_cd10) like 'E11%'
               or upcase(icd_dgns_cd11) like 'E11%'
               or upcase(icd_dgns_cd12) like 'E11%'
           ) then 1 else 0 end as has_t2d
    from arapcd.APCD_MCR_PRTB_CAR_CLM
    where (upcase(prncpal_dgns_cd) like 'E10%'
       or upcase(prncpal_dgns_cd) like 'E11%'
       or upcase(icd_dgns_cd1) like 'E10%' or upcase(icd_dgns_cd1) like 'E11%'
       or upcase(icd_dgns_cd2) like 'E10%' or upcase(icd_dgns_cd2) like 'E11%'
       or upcase(icd_dgns_cd3) like 'E10%' or upcase(icd_dgns_cd3) like 'E11%'
       or upcase(icd_dgns_cd4) like 'E10%' or upcase(icd_dgns_cd4) like 'E11%'
       or upcase(icd_dgns_cd5) like 'E10%' or upcase(icd_dgns_cd5) like 'E11%'
       or upcase(icd_dgns_cd6) like 'E10%' or upcase(icd_dgns_cd6) like 'E11%'
       or upcase(icd_dgns_cd7) like 'E10%' or upcase(icd_dgns_cd7) like 'E11%'
       or upcase(icd_dgns_cd8) like 'E10%' or upcase(icd_dgns_cd8) like 'E11%'
       or upcase(icd_dgns_cd9) like 'E10%' or upcase(icd_dgns_cd9) like 'E11%'
       or upcase(icd_dgns_cd10) like 'E10%' or upcase(icd_dgns_cd10) like 'E11%'
       or upcase(icd_dgns_cd11) like 'E10%' or upcase(icd_dgns_cd11) like 'E11%'
       or upcase(icd_dgns_cd12) like 'E10%' or upcase(icd_dgns_cd12) like 'E11%'
    )
    and year(clm_from_dt) <= 2024
    ;
quit;

/* --- Outpatient Claims (25 DX fields) ---------------------------------- */
proc sql;
    create table dm_mcr_outpt as
    select bene_id,
           clm_from_dt as service_date,
           year(clm_from_dt) as claim_year,
           case when (
               upcase(prncpal_dgns_cd) like 'E10%'
               or upcase(icd_dgns_cd1)  like 'E10%'
               or upcase(icd_dgns_cd2)  like 'E10%'
               or upcase(icd_dgns_cd3)  like 'E10%'
               or upcase(icd_dgns_cd4)  like 'E10%'
               or upcase(icd_dgns_cd5)  like 'E10%'
               or upcase(icd_dgns_cd6)  like 'E10%'
               or upcase(icd_dgns_cd7)  like 'E10%'
               or upcase(icd_dgns_cd8)  like 'E10%'
               or upcase(icd_dgns_cd9)  like 'E10%'
               or upcase(icd_dgns_cd10) like 'E10%'
               or upcase(icd_dgns_cd11) like 'E10%'
               or upcase(icd_dgns_cd12) like 'E10%'
               or upcase(icd_dgns_cd13) like 'E10%'
               or upcase(icd_dgns_cd14) like 'E10%'
               or upcase(icd_dgns_cd15) like 'E10%'
               or upcase(icd_dgns_cd16) like 'E10%'
               or upcase(icd_dgns_cd17) like 'E10%'
               or upcase(icd_dgns_cd18) like 'E10%'
               or upcase(icd_dgns_cd19) like 'E10%'
               or upcase(icd_dgns_cd20) like 'E10%'
               or upcase(icd_dgns_cd21) like 'E10%'
               or upcase(icd_dgns_cd22) like 'E10%'
               or upcase(icd_dgns_cd23) like 'E10%'
               or upcase(icd_dgns_cd24) like 'E10%'
               or upcase(icd_dgns_cd25) like 'E10%'
           ) then 1 else 0 end as has_t1d,
           case when (
               upcase(prncpal_dgns_cd) like 'E11%'
               or upcase(icd_dgns_cd1)  like 'E11%'
               or upcase(icd_dgns_cd2)  like 'E11%'
               or upcase(icd_dgns_cd3)  like 'E11%'
               or upcase(icd_dgns_cd4)  like 'E11%'
               or upcase(icd_dgns_cd5)  like 'E11%'
               or upcase(icd_dgns_cd6)  like 'E11%'
               or upcase(icd_dgns_cd7)  like 'E11%'
               or upcase(icd_dgns_cd8)  like 'E11%'
               or upcase(icd_dgns_cd9)  like 'E11%'
               or upcase(icd_dgns_cd10) like 'E11%'
               or upcase(icd_dgns_cd11) like 'E11%'
               or upcase(icd_dgns_cd12) like 'E11%'
               or upcase(icd_dgns_cd13) like 'E11%'
               or upcase(icd_dgns_cd14) like 'E11%'
               or upcase(icd_dgns_cd15) like 'E11%'
               or upcase(icd_dgns_cd16) like 'E11%'
               or upcase(icd_dgns_cd17) like 'E11%'
               or upcase(icd_dgns_cd18) like 'E11%'
               or upcase(icd_dgns_cd19) like 'E11%'
               or upcase(icd_dgns_cd20) like 'E11%'
               or upcase(icd_dgns_cd21) like 'E11%'
               or upcase(icd_dgns_cd22) like 'E11%'
               or upcase(icd_dgns_cd23) like 'E11%'
               or upcase(icd_dgns_cd24) like 'E11%'
               or upcase(icd_dgns_cd25) like 'E11%'
           ) then 1 else 0 end as has_t2d
    from arapcd.APCD_MCR_OUT_CLM
    where (upcase(prncpal_dgns_cd) like 'E10%' or upcase(prncpal_dgns_cd) like 'E11%'
       or upcase(icd_dgns_cd1)  like 'E10%' or upcase(icd_dgns_cd1)  like 'E11%'
       or upcase(icd_dgns_cd2)  like 'E10%' or upcase(icd_dgns_cd2)  like 'E11%'
       or upcase(icd_dgns_cd3)  like 'E10%' or upcase(icd_dgns_cd3)  like 'E11%'
       or upcase(icd_dgns_cd4)  like 'E10%' or upcase(icd_dgns_cd4)  like 'E11%'
       or upcase(icd_dgns_cd5)  like 'E10%' or upcase(icd_dgns_cd5)  like 'E11%'
       or upcase(icd_dgns_cd6)  like 'E10%' or upcase(icd_dgns_cd6)  like 'E11%'
       or upcase(icd_dgns_cd7)  like 'E10%' or upcase(icd_dgns_cd7)  like 'E11%'
       or upcase(icd_dgns_cd8)  like 'E10%' or upcase(icd_dgns_cd8)  like 'E11%'
       or upcase(icd_dgns_cd9)  like 'E10%' or upcase(icd_dgns_cd9)  like 'E11%'
       or upcase(icd_dgns_cd10) like 'E10%' or upcase(icd_dgns_cd10) like 'E11%'
       or upcase(icd_dgns_cd11) like 'E10%' or upcase(icd_dgns_cd11) like 'E11%'
       or upcase(icd_dgns_cd12) like 'E10%' or upcase(icd_dgns_cd12) like 'E11%'
       or upcase(icd_dgns_cd13) like 'E10%' or upcase(icd_dgns_cd13) like 'E11%'
       or upcase(icd_dgns_cd14) like 'E10%' or upcase(icd_dgns_cd14) like 'E11%'
       or upcase(icd_dgns_cd15) like 'E10%' or upcase(icd_dgns_cd15) like 'E11%'
       or upcase(icd_dgns_cd16) like 'E10%' or upcase(icd_dgns_cd16) like 'E11%'
       or upcase(icd_dgns_cd17) like 'E10%' or upcase(icd_dgns_cd17) like 'E11%'
       or upcase(icd_dgns_cd18) like 'E10%' or upcase(icd_dgns_cd18) like 'E11%'
       or upcase(icd_dgns_cd19) like 'E10%' or upcase(icd_dgns_cd19) like 'E11%'
       or upcase(icd_dgns_cd20) like 'E10%' or upcase(icd_dgns_cd20) like 'E11%'
       or upcase(icd_dgns_cd21) like 'E10%' or upcase(icd_dgns_cd21) like 'E11%'
       or upcase(icd_dgns_cd22) like 'E10%' or upcase(icd_dgns_cd22) like 'E11%'
       or upcase(icd_dgns_cd23) like 'E10%' or upcase(icd_dgns_cd23) like 'E11%'
       or upcase(icd_dgns_cd24) like 'E10%' or upcase(icd_dgns_cd24) like 'E11%'
       or upcase(icd_dgns_cd25) like 'E10%' or upcase(icd_dgns_cd25) like 'E11%'
    )
    and year(clm_from_dt) <= 2024
    ;
quit;

/* --- Inpatient Claims -------------------------------------------------- */
/* INP_CLM has admtg_dgns_cd + prncpal_dgns_cd + icd_dgns_cd1..25 (27 DX fields) */
proc sql;
    create table dm_mcr_inpt as
    select bene_id,
           clm_from_dt as service_date,
           year(clm_from_dt) as claim_year,
           case when (
               upcase(admtg_dgns_cd) like 'E10%'
               or upcase(prncpal_dgns_cd) like 'E10%'
               or upcase(icd_dgns_cd1)  like 'E10%'
               or upcase(icd_dgns_cd2)  like 'E10%'
               or upcase(icd_dgns_cd3)  like 'E10%'
               or upcase(icd_dgns_cd4)  like 'E10%'
               or upcase(icd_dgns_cd5)  like 'E10%'
               or upcase(icd_dgns_cd6)  like 'E10%'
               or upcase(icd_dgns_cd7)  like 'E10%'
               or upcase(icd_dgns_cd8)  like 'E10%'
               or upcase(icd_dgns_cd9)  like 'E10%'
               or upcase(icd_dgns_cd10) like 'E10%'
               or upcase(icd_dgns_cd11) like 'E10%'
               or upcase(icd_dgns_cd12) like 'E10%'
               or upcase(icd_dgns_cd13) like 'E10%'
               or upcase(icd_dgns_cd14) like 'E10%'
               or upcase(icd_dgns_cd15) like 'E10%'
               or upcase(icd_dgns_cd16) like 'E10%'
               or upcase(icd_dgns_cd17) like 'E10%'
               or upcase(icd_dgns_cd18) like 'E10%'
               or upcase(icd_dgns_cd19) like 'E10%'
               or upcase(icd_dgns_cd20) like 'E10%'
               or upcase(icd_dgns_cd21) like 'E10%'
               or upcase(icd_dgns_cd22) like 'E10%'
               or upcase(icd_dgns_cd23) like 'E10%'
               or upcase(icd_dgns_cd24) like 'E10%'
               or upcase(icd_dgns_cd25) like 'E10%'
           ) then 1 else 0 end as has_t1d,
           case when (
               upcase(admtg_dgns_cd) like 'E11%'
               or upcase(prncpal_dgns_cd) like 'E11%'
               or upcase(icd_dgns_cd1)  like 'E11%'
               or upcase(icd_dgns_cd2)  like 'E11%'
               or upcase(icd_dgns_cd3)  like 'E11%'
               or upcase(icd_dgns_cd4)  like 'E11%'
               or upcase(icd_dgns_cd5)  like 'E11%'
               or upcase(icd_dgns_cd6)  like 'E11%'
               or upcase(icd_dgns_cd7)  like 'E11%'
               or upcase(icd_dgns_cd8)  like 'E11%'
               or upcase(icd_dgns_cd9)  like 'E11%'
               or upcase(icd_dgns_cd10) like 'E11%'
               or upcase(icd_dgns_cd11) like 'E11%'
               or upcase(icd_dgns_cd12) like 'E11%'
               or upcase(icd_dgns_cd13) like 'E11%'
               or upcase(icd_dgns_cd14) like 'E11%'
               or upcase(icd_dgns_cd15) like 'E11%'
               or upcase(icd_dgns_cd16) like 'E11%'
               or upcase(icd_dgns_cd17) like 'E11%'
               or upcase(icd_dgns_cd18) like 'E11%'
               or upcase(icd_dgns_cd19) like 'E11%'
               or upcase(icd_dgns_cd20) like 'E11%'
               or upcase(icd_dgns_cd21) like 'E11%'
               or upcase(icd_dgns_cd22) like 'E11%'
               or upcase(icd_dgns_cd23) like 'E11%'
               or upcase(icd_dgns_cd24) like 'E11%'
               or upcase(icd_dgns_cd25) like 'E11%'
           ) then 1 else 0 end as has_t2d
    from arapcd.APCD_MCR_INP_CLM
    where (upcase(admtg_dgns_cd) like 'E10%' or upcase(admtg_dgns_cd) like 'E11%'
       or upcase(prncpal_dgns_cd) like 'E10%' or upcase(prncpal_dgns_cd) like 'E11%'
       or upcase(icd_dgns_cd1)  like 'E10%' or upcase(icd_dgns_cd1)  like 'E11%'
       or upcase(icd_dgns_cd2)  like 'E10%' or upcase(icd_dgns_cd2)  like 'E11%'
       or upcase(icd_dgns_cd3)  like 'E10%' or upcase(icd_dgns_cd3)  like 'E11%'
       or upcase(icd_dgns_cd4)  like 'E10%' or upcase(icd_dgns_cd4)  like 'E11%'
       or upcase(icd_dgns_cd5)  like 'E10%' or upcase(icd_dgns_cd5)  like 'E11%'
       or upcase(icd_dgns_cd6)  like 'E10%' or upcase(icd_dgns_cd6)  like 'E11%'
       or upcase(icd_dgns_cd7)  like 'E10%' or upcase(icd_dgns_cd7)  like 'E11%'
       or upcase(icd_dgns_cd8)  like 'E10%' or upcase(icd_dgns_cd8)  like 'E11%'
       or upcase(icd_dgns_cd9)  like 'E10%' or upcase(icd_dgns_cd9)  like 'E11%'
       or upcase(icd_dgns_cd10) like 'E10%' or upcase(icd_dgns_cd10) like 'E11%'
       or upcase(icd_dgns_cd11) like 'E10%' or upcase(icd_dgns_cd11) like 'E11%'
       or upcase(icd_dgns_cd12) like 'E10%' or upcase(icd_dgns_cd12) like 'E11%'
       or upcase(icd_dgns_cd13) like 'E10%' or upcase(icd_dgns_cd13) like 'E11%'
       or upcase(icd_dgns_cd14) like 'E10%' or upcase(icd_dgns_cd14) like 'E11%'
       or upcase(icd_dgns_cd15) like 'E10%' or upcase(icd_dgns_cd15) like 'E11%'
       or upcase(icd_dgns_cd16) like 'E10%' or upcase(icd_dgns_cd16) like 'E11%'
       or upcase(icd_dgns_cd17) like 'E10%' or upcase(icd_dgns_cd17) like 'E11%'
       or upcase(icd_dgns_cd18) like 'E10%' or upcase(icd_dgns_cd18) like 'E11%'
       or upcase(icd_dgns_cd19) like 'E10%' or upcase(icd_dgns_cd19) like 'E11%'
       or upcase(icd_dgns_cd20) like 'E10%' or upcase(icd_dgns_cd20) like 'E11%'
       or upcase(icd_dgns_cd21) like 'E10%' or upcase(icd_dgns_cd21) like 'E11%'
       or upcase(icd_dgns_cd22) like 'E10%' or upcase(icd_dgns_cd22) like 'E11%'
       or upcase(icd_dgns_cd23) like 'E10%' or upcase(icd_dgns_cd23) like 'E11%'
       or upcase(icd_dgns_cd24) like 'E10%' or upcase(icd_dgns_cd24) like 'E11%'
       or upcase(icd_dgns_cd25) like 'E10%' or upcase(icd_dgns_cd25) like 'E11%'
    )
    and year(clm_from_dt) <= 2024
    ;
quit;

/* --- SNF, HHA, HSP, DME Claims (all use similar DX structure) ---------- */
/* These are lower-volume tables; using a macro for efficiency */

%macro scan_mcr_facility(tbl, max_dx, has_admtg=0);
    proc sql;
        create table dm_mcr_&tbl. as
        select bene_id,
               clm_from_dt as service_date,
               year(clm_from_dt) as claim_year,
               case when (
                   %if &has_admtg. = 1 %then %do;
                       upcase(admtg_dgns_cd) like 'E10%' or
                   %end;
                   upcase(prncpal_dgns_cd) like 'E10%'
                   %do i = 1 %to &max_dx.;
                       or upcase(icd_dgns_cd&i.) like 'E10%'
                   %end;
               ) then 1 else 0 end as has_t1d,
               case when (
                   %if &has_admtg. = 1 %then %do;
                       upcase(admtg_dgns_cd) like 'E11%' or
                   %end;
                   upcase(prncpal_dgns_cd) like 'E11%'
                   %do i = 1 %to &max_dx.;
                       or upcase(icd_dgns_cd&i.) like 'E11%'
                   %end;
               ) then 1 else 0 end as has_t2d
        from arapcd.APCD_MCR_&tbl._CLM
        where (
           %if &has_admtg. = 1 %then %do;
               upcase(admtg_dgns_cd) like 'E10%' or upcase(admtg_dgns_cd) like 'E11%' or
           %end;
           upcase(prncpal_dgns_cd) like 'E10%'
           or upcase(prncpal_dgns_cd) like 'E11%'
           %do i = 1 %to &max_dx.;
               or upcase(icd_dgns_cd&i.) like 'E10%'
               or upcase(icd_dgns_cd&i.) like 'E11%'
           %end;
        )
        and year(clm_from_dt) <= 2024
        ;
    quit;
%mend scan_mcr_facility;

%scan_mcr_facility(SNF, 25, has_admtg=1);  /* SNF has admtg_dgns_cd per element list */
%scan_mcr_facility(HHA, 25);
%scan_mcr_facility(HSP, 25);
%scan_mcr_facility(DME, 12);

/* ======================================================================= */
/* PART C: Combine all Medicare sources into one patient-level table        */
/* ======================================================================= */

/* Stack all Medicare diabetes claims */
data dm_claims_medicare;
    set dm_mcr_partb
        dm_mcr_outpt
        dm_mcr_inpt
        dm_mcr_SNF
        dm_mcr_HHA
        dm_mcr_HSP
        dm_mcr_DME;
run;

/* Aggregate to patient level */
proc sql;
    create table dm_cohort_medicare as
    select bene_id,
           max(has_t1d) as ever_t1d,
           max(has_t2d) as ever_t2d,
           min(service_date) as first_dm_date format=yymmdd10.,
           min(claim_year) as first_dm_year,
           max(claim_year) as last_dm_year,
           count(*) as n_dm_claims,
           case
               when max(has_t1d) = 1 and max(has_t2d) = 1 then 'AMBIGUOUS'
               when max(has_t1d) = 1 then 'T1D'
               when max(has_t2d) = 1 then 'T2D'
               else 'UNKNOWN'
           end as dm_type length=10,
           'MEDICARE' as data_source length=10
    from dm_claims_medicare
    group by bene_id;
quit;

title "Step 3B: Medicare Diabetes Cohort Summary";
proc sql;
    select dm_type,
           count(*) as n_patients format=comma12.,
           min(first_dm_year) as earliest_year,
           max(last_dm_year) as latest_year,
           sum(n_dm_claims) as total_claims format=comma15.
    from dm_cohort_medicare
    group by dm_type;
quit;

/* Medicare year-level counts */
proc sql;
    create table dm_medicare_by_year as
    select claim_year,
           case
               when has_t1d = 1 and has_t2d = 1 then 'AMBIGUOUS'
               when has_t1d = 1 then 'T1D'
               when has_t2d = 1 then 'T2D'
               else 'UNKNOWN'
           end as dm_type length=10,
           count(distinct bene_id) as n_patients format=comma12.
    from dm_claims_medicare
    group by claim_year, calculated dm_type;
quit;

title "Step 3B: Medicare Diabetes Patients by Year and Type";
proc print data=dm_medicare_by_year noobs; run;

/* ======================================================================= */
/* PART D: Cross-validate Medicare with BEN_SUM_CC diabetes flags          */
/* ======================================================================= */

title "Step 3D: Medicare BEN_SUM_CC Diabetes Flags vs Claims-Based Cohort";
proc sql;
    /* How many of our claims-identified diabetics are flagged in BEN_SUM_CC? */
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
/* PART E: Combined summary across all data sources                        */
/* ======================================================================= */

title "Step 3 FINAL: Combined Diabetes Cohort Summary";
proc sql;
    select data_source, dm_type,
           count(*) as n_patients format=comma12.,
           sum(n_dm_claims) as total_claims format=comma15.
    from (
        select dm_type, data_source, n_dm_claims from dm_cohort_commercial
        union all
        select dm_type, data_source, n_dm_claims from dm_cohort_medicare
    )
    group by data_source, dm_type
    order by data_source, dm_type;
quit;

title;

/*****************************************************************************
 NOTES AND CAVEATS:

 1. Commercial/Medicaid claims have 14 diagnosis fields: mc039 (admitting dx),
    mc041 (principal dx), mc042–mc053 (dx2–dx13). All 14 are scanned via the
    %comm_dx_check macro. This provides substantially better coverage than the
    earlier incorrect approach that only checked 2 fields.

 2. The person-level key for commercial claims is the composite of mc001
    (submitter) + mc006 (group/policy, hashed) + mc009 (member sequence).
    mc009 alone is NOT a person identifier — it is just a sequence number
    (1=subscriber, 2=spouse, etc.) within a group.

 3. The AMBIGUOUS flag identifies patients with BOTH E10.x and E11.x codes
    across their entire claim history. Per instructions, this is NOT resolved.
    Common reasons: coding errors, T1D patients with insulin resistance coded
    as T2D, or actual diagnostic uncertainty.

 4. Medicare BEN_SUM_CC diabetes flags are checked for cross-validation but
    do NOT distinguish T1D vs T2D, so they cannot replace claims-based typing.

 5. Person linkage across commercial and Medicare is NOT attempted here.
    A patient could appear in both dm_cohort_commercial and dm_cohort_medicare
    if they have both coverage types. Deduplication would require a crosswalk
    (e.g., AR_APCD_24B_MEST.apcd_unique_id) which is deferred.
*****************************************************************************/
