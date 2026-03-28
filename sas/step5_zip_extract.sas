/*****************************************************************************
 Step 5: Extract Patient ZIP Codes and Export CSV
 Purpose: Pull ZIP codes for all diabetes/DFU patients, assess data quality,
          and export a clean patient-level analytic dataset as CSV.

 Person Identification:
   Commercial/Medicaid person composite key is 3 fields:
     mc001 (submitter) + mc006 (group/policy, hashed) + mc009 (member sequence)
   In MEMBER table the equivalent fields are:
     me001 (submitter) + me006 (group/policy, hashed) + me010 (member sequence)
   patient_id for export = catx('|', mc001, mc006, mc009)

 ZIP Sources:
   Commercial/Medicaid:
     - MEMBER.ME017 = member residential ZIP (5-digit in practice)
     - CLAIM.MC016 = member ZIP at time of service (5-digit in practice)
     Preference: Use MEMBER.ME017 as primary (enrollment record),
       fall back to MC016 from most recent claim if ME017 is missing.

   Medicare:
     - APCD_MCR_BEN_SUM.ZIP_CD = 5-digit ZIP (confirmed)
     - Joined on BENE_ID
     - NOTE: BEN_SUM has ~100% ZIP coverage on the full server.
       The 92% missing rate seen in earlier sample analysis was an artifact
       of independent sample draws (sample BENE_IDs did not overlap between
       claims and BEN_SUM). Full-server joins resolve correctly.

 Diagnosis fields: 14 DX fields (mc039, mc041-mc053) in CLAIM tables.

 Output: CSV file at /Users/williamwatson/Claude_Code/Aim_3_Dissertation/
         with one row per patient, columns for DM type, DFU flag,
         ZIP code, and data quality indicators.

 Prerequisite: Steps 3 and 4 must be run first.
*****************************************************************************/

libname arapcd odbc
    noprompt="dsn=APCD-24D;Trusted_connection=yes"
    schema=public;

%let outdir = /Users/williamwatson/Claude_Code/Aim_3_Dissertation;

/* ======================================================================= */
/* PART A: Commercial/Medicaid — Get ZIP from MEMBER table                 */
/* ======================================================================= */

/*
   Person composite key: mc001 (submitter) + mc006 (group/policy) + mc009 (member seq)
   MEMBER equivalent:    me001 (submitter) + me006 (group/policy) + me010 (member seq)

   MEMBER has multiple rows per person (coverage segments).
   Strategy: For each person (3-field key), take the most recent enrollment
   record (max reckey) to get current ZIP.
*/
proc sql;
    create table dm_zip_commercial as
    select d.submitter,
           d.group_policy,
           d.person_code,
           d.dm_type,
           d.first_dm_year,
           d.last_dm_year,
           d.n_dm_claims,
           d.first_dm_date,
           /* Get ZIP from most recent MEMBER record */
           m.me017 as zip_member length=10,
           m.me016 as state_member length=5,
           m.me028 as gender length=2,
           m.me014_curr_age as age,
           /* DFU flag */
           case when f.person_code is not null then 1 else 0 end as has_dfu,
           f.first_dfu_year,
           f.first_dfu_date,
           f.last_dfu_year,
           f.n_dfu_claims,
           f.ever_l97,
           f.ever_dm_combo,
           f.max_severity_rank,
           'COMMERCIAL' as data_source length=10
    from dm_cohort_commercial d
    left join (
        /* Most recent MEMBER record per person (3-field composite key) */
        select me001, me006, me010, me016, me017, me028, me014_curr_age
        from arapcd.MEMBER
        where (me001, me006, me010, reckey) in (
            select me001, me006, me010, max(reckey)
            from arapcd.MEMBER
            group by me001, me006, me010
        )
    ) m
        on d.submitter = m.me001
       and d.group_policy = m.me006
       and d.person_code = m.me010
    left join dfu_cohort_commercial f
        on d.submitter = f.submitter
       and d.group_policy = f.group_policy
       and d.person_code = f.person_code
    ;
quit;

/* Also get ZIP from most recent CLAIM as fallback */
proc sql;
    create table _claim_zip_fallback as
    select mc001 as submitter,
           mc006 as group_policy,
           mc009 as person_code,
           mc016 as zip_claim length=10
    from (
        /* Most recent claim per person (3-field key) from latest year */
        select mc001, mc006, mc009, mc016, mc017,
               row_number() over (
                   partition by mc001, mc006, mc009
                   order by mc017 desc
               ) as rn
        from arapcd.CLAIM_SVC_DT_2024
        where mc016 is not null and mc016 ne ''
    )
    where rn = 1;
quit;

/* Merge fallback ZIP using 3-field composite key */
proc sql;
    create table dm_zip_commercial2 as
    select a.*,
           b.zip_claim,
           /* Use MEMBER ZIP if available, else CLAIM ZIP */
           case
               when a.zip_member is not null and a.zip_member ne '' then a.zip_member
               when b.zip_claim is not null and b.zip_claim ne '' then b.zip_claim
               else ''
           end as zip_final length=10,
           case
               when a.zip_member is not null and a.zip_member ne '' then 'MEMBER'
               when b.zip_claim is not null and b.zip_claim ne '' then 'CLAIM'
               else 'MISSING'
           end as zip_source length=10
    from dm_zip_commercial a
    left join _claim_zip_fallback b
        on a.submitter = b.submitter
       and a.group_policy = b.group_policy
       and a.person_code = b.person_code;
quit;

/* ======================================================================= */
/* PART B: Medicare — Get ZIP from BEN_SUM                                 */
/* ======================================================================= */

/*
   BEN_SUM has ~100% ZIP coverage on the full server. The 92% missing rate
   observed in earlier sample analysis was an artifact of independent sample
   draws (claim-sample BENE_IDs did not overlap with BEN_SUM-sample BENE_IDs).
   Full-server joins resolve correctly.
*/
proc sql;
    create table dm_zip_medicare as
    select d.bene_id,
           d.dm_type,
           d.first_dm_year,
           d.last_dm_year,
           d.n_dm_claims,
           d.first_dm_date,
           /* Demographics from BEN_SUM */
           b.zip_cd as zip_final length=10,
           b.state_code as state_member length=5,
           b.county_cd as county length=5,
           b.sex_ident_cd as gender length=2,
           b.bene_race_cd as race_cd length=2,
           b.rti_race_cd as rti_race_cd length=2,
           b.age_at_end_ref_yr as age,
           /* DFU flag */
           case when f.bene_id is not null then 1 else 0 end as has_dfu,
           f.first_dfu_year,
           f.first_dfu_date,
           f.last_dfu_year,
           f.n_dfu_claims,
           f.ever_l97,
           f.ever_dm_combo,
           f.max_severity_rank,
           'MEDICARE' as data_source length=10,
           'BEN_SUM' as zip_source length=10
    from dm_cohort_medicare d
    left join (
        /* Most recent BEN_SUM record per beneficiary */
        select *
        from arapcd.APCD_MCR_BEN_SUM
        where (bene_id, bene_enrollmt_ref_yr) in (
            select bene_id, max(bene_enrollmt_ref_yr)
            from arapcd.APCD_MCR_BEN_SUM
            group by bene_id
        )
    ) b
        on d.bene_id = b.bene_id
    left join dfu_cohort_medicare f
        on d.bene_id = f.bene_id
    ;
quit;

/* ======================================================================= */
/* PART C: ZIP Code Quality Assessment                                     */
/* ======================================================================= */

title "Step 5: ZIP Code Completeness — Commercial";
proc sql;
    select zip_source,
           count(*) as n_patients format=comma12.,
           calculated n_patients / (select count(*) from dm_zip_commercial2) * 100
               as pct format=5.1
    from dm_zip_commercial2
    group by zip_source;
quit;

title "Step 5: ZIP Code Completeness — Medicare";
proc sql;
    select case
               when zip_final is not null and zip_final ne '' then 'HAS_ZIP'
               else 'MISSING'
           end as zip_status,
           count(*) as n_patients format=comma12.,
           calculated n_patients / (select count(*) from dm_zip_medicare) * 100
               as pct format=5.1
    from dm_zip_medicare
    group by calculated zip_status;
quit;

/* ZIP length distribution */
title "Step 5: ZIP Code Length Distribution — Commercial";
proc sql;
    select length(strip(zip_final)) as zip_length,
           count(*) as n format=comma12.
    from dm_zip_commercial2
    where zip_final ne ''
    group by calculated zip_length
    order by zip_length;
quit;

title "Step 5: ZIP Code Length Distribution — Medicare";
proc sql;
    select length(strip(zip_final)) as zip_length,
           count(*) as n format=comma12.
    from dm_zip_medicare
    where zip_final is not null and zip_final ne ''
    group by calculated zip_length
    order by zip_length;
quit;

/* Check for non-Arkansas ZIPs (Arkansas ZIPs: 71601–72959) */
title "Step 5: Non-Arkansas ZIP Codes — Commercial (first 2 digits not 71 or 72)";
proc sql;
    select case
               when substr(strip(zip_final), 1, 2) in ('71','72') then 'ARKANSAS'
               when zip_final = '' or zip_final is null then 'MISSING'
               else 'NON-ARKANSAS'
           end as zip_state_cat,
           count(*) as n_patients format=comma12.,
           calculated n_patients / (select count(*) from dm_zip_commercial2) * 100
               as pct format=5.1
    from dm_zip_commercial2
    group by calculated zip_state_cat;
quit;

title "Step 5: Non-Arkansas ZIP Codes — Medicare";
proc sql;
    select case
               when state_member = '05' then 'ARKANSAS'     /* FIPS 05 = Arkansas */
               when zip_final = '' or zip_final is null then 'MISSING'
               else 'NON-ARKANSAS'
           end as state_cat,
           count(*) as n_patients format=comma12.,
           calculated n_patients / (select count(*) from dm_zip_medicare) * 100
               as pct format=5.1
    from dm_zip_medicare
    group by calculated state_cat;
quit;

/* ======================================================================= */
/* PART D: Build final export dataset                                      */
/* ======================================================================= */

/*
   Standardize and stack commercial + Medicare.
   Commercial patient_id = catx('|', submitter, group_policy, person_code)
     i.e., the 3-field composite key: mc001 | mc006 | mc009
   Medicare patient_id = bene_id
*/
proc sql;
    create table dm_dfu_analytic as

    /* Commercial patients */
    select 'COMMERCIAL' as data_source length=10,
           catx('|', submitter, group_policy, person_code) as patient_id length=80,
           dm_type as diabetes_type length=10,
           first_dm_year,
           last_dm_year,
           n_dm_claims,
           first_dm_date,
           has_dfu,
           first_dfu_year,
           first_dfu_date,
           last_dfu_year,
           n_dfu_claims,
           coalesce(ever_l97, 0) as ever_l97,
           coalesce(ever_dm_combo, 0) as ever_dm_combo,
           case when coalesce(ever_l97,0)=1 and coalesce(ever_dm_combo,0)=1 then 'BOTH'
                when coalesce(ever_l97,0)=1 then 'L97_ONLY'
                when coalesce(ever_dm_combo,0)=1 then 'COMBO_ONLY'
                else '' end as dfu_source length=12,
           coalesce(max_severity_rank, 0) as severity_rank,
           substr(strip(zip_final), 1, 5) as ar_zip length=5,
           zip_source length=10,
           state_member as state length=5,
           '' as county length=5,
           /* Harmonize sex: commercial me028 is already M/F */
           gender as sex length=2,
           age,
           '' as race_cd length=2,
           '' as rti_race_cd length=2,
           case when has_dfu = 1 and first_dfu_date is not null and first_dm_date is not null
                then intck('day', first_dm_date, first_dfu_date)
                else . end as days_dm_to_dfu
    from dm_zip_commercial2
    where zip_final ne ''
      and substr(strip(zip_final), 1, 2) in ('71','72')

    union all

    /* Medicare patients */
    select 'MEDICARE' as data_source,
           bene_id as patient_id,
           dm_type as diabetes_type,
           first_dm_year,
           last_dm_year,
           n_dm_claims,
           first_dm_date,
           has_dfu,
           first_dfu_year,
           first_dfu_date,
           last_dfu_year,
           n_dfu_claims,
           coalesce(ever_l97, 0),
           coalesce(ever_dm_combo, 0),
           case when coalesce(ever_l97,0)=1 and coalesce(ever_dm_combo,0)=1 then 'BOTH'
                when coalesce(ever_l97,0)=1 then 'L97_ONLY'
                when coalesce(ever_dm_combo,0)=1 then 'COMBO_ONLY'
                else '' end as dfu_source,
           coalesce(max_severity_rank, 0),
           substr(strip(zip_final), 1, 5) as ar_zip length=5,
           zip_source,
           state_member as state,
           county,
           /* Harmonize sex: Medicare sex_ident_cd 1→M, 2→F, else→U */
           case when gender = '1' then 'M'
                when gender = '2' then 'F'
                else 'U' end as sex length=2,
           age,
           race_cd,
           rti_race_cd,
           case when has_dfu = 1 and first_dfu_date is not null and first_dm_date is not null
                then intck('day', first_dm_date, first_dfu_date)
                else . end as days_dm_to_dfu
    from dm_zip_medicare
    where zip_final is not null and zip_final ne ''
      and substr(strip(zip_final), 1, 2) in ('71','72')

    order by data_source, diabetes_type, patient_id;
quit;

/* ======================================================================= */
/* PART E: Export to CSV                                                    */
/* ======================================================================= */

/*
   Export 4 separate CSVs per spec:
     cohort_commercial.csv — all commercial DM patients
     cohort_medicare.csv   — all Medicare DM patients
     dfu_commercial.csv    — commercial DFU patients only
     dfu_medicare.csv      — Medicare DFU patients only
*/

/* --- cohort_commercial.csv --- */
proc export data=dm_dfu_analytic(
    where=(data_source='COMMERCIAL')
    keep=patient_id study_id diabetes_type first_dm_date ar_zip first_dm_year sex age
)   outfile="&outdir./cohort_commercial.csv"
    dbms=csv replace;
run;

/* --- cohort_medicare.csv --- */
proc export data=dm_dfu_analytic(
    where=(data_source='MEDICARE')
    keep=patient_id study_id diabetes_type first_dm_date ar_zip first_dm_year sex age
)   outfile="&outdir./cohort_medicare.csv"
    dbms=csv replace;
run;

/* --- dfu_commercial.csv --- */
proc export data=dm_dfu_analytic(
    where=(data_source='COMMERCIAL' and has_dfu=1)
    keep=patient_id study_id first_dfu_date dfu_source severity_rank days_dm_to_dfu
)   outfile="&outdir./dfu_commercial.csv"
    dbms=csv replace;
run;

/* --- dfu_medicare.csv --- */
proc export data=dm_dfu_analytic(
    where=(data_source='MEDICARE' and has_dfu=1)
    keep=patient_id study_id first_dfu_date dfu_source severity_rank days_dm_to_dfu
)   outfile="&outdir./dfu_medicare.csv"
    dbms=csv replace;
run;

/* Also keep a combined analytic file for reference */
proc export data=dm_dfu_analytic
    outfile="&outdir./dm_dfu_analytic.csv"
    dbms=csv replace;
run;

title "Step 5: Final Analytic Dataset Summary";
proc sql;
    select data_source, diabetes_type, has_dfu,
           count(*) as n_patients format=comma12.
    from dm_dfu_analytic
    group by data_source, diabetes_type, has_dfu
    order by data_source, diabetes_type, has_dfu;
quit;

title "Step 5: Final Dataset Dimensions";
proc sql;
    select count(*) as total_rows format=comma12.,
           count(distinct patient_id) as unique_patients format=comma12.,
           sum(case when ar_zip ne '' then 1 else 0 end) as has_zip format=comma12.,
           sum(case when ar_zip = '' or ar_zip is null then 1 else 0 end) as missing_zip format=comma12.,
           sum(case when ar_zip ne '' then 1 else 0 end) / count(*) * 100 as zip_completeness format=5.1
    from dm_dfu_analytic;
quit;

title "Step 5: Time from First DM to First DFU (DFU patients only)";
proc sql;
    select data_source,
           count(*) as n_dfu_patients format=comma12.,
           sum(case when days_dm_to_dfu is not null then 1 else 0 end) as has_both_dates format=comma12.,
           avg(days_dm_to_dfu) as mean_days format=8.0,
           min(days_dm_to_dfu) as min_days format=8.0,
           max(days_dm_to_dfu) as max_days format=8.0
    from dm_dfu_analytic
    where has_dfu = 1
    group by data_source;
quit;

title;

/* ======================================================================= */
/* PART F: MEST Linkage for Cross-Source Deduplication                      */
/* ======================================================================= */

/*
   MEST LINKAGE (from ACHI "Guide to the APCD Member Enrollment Selection Table"
                 + MCR APCD Element List FINAL.xlsx):

   Join path (commercial): MEMBER.me001+me107 = MEST.submitter+member_id → apcd_unique_id
   Join path (Medicare FFS): BEN_SUM.APCD_UNIQUE_ID directly available (Text, 512)!

   Study_ID = apcd_unique_id + gender — the cross-payer person identifier.

   KEY FINDING: MCR_BEN_SUM has APCD_UNIQUE_ID. This means BOTH commercial and
   Medicare FFS patients can be linked via the same cross-payer identifier, enabling
   full cross-source deduplication without proxy matching.

   APPROACH:
     F-1: Join commercial patients to MEST via MEMBER(me001+me107) to get study_id
     F-2: Get Medicare FFS study_id directly from BEN_SUM.APCD_UNIQUE_ID
     F-3: Merge study_ids into the analytic dataset
     F-4: Report cross-source overlap (same study_id in both commercial and Medicare)
     F-5: Export dedup-enriched dataset
*/

/* F-1: Get MEST study_id for commercial patients */
/*
   Join: dm_cohort_commercial → MEMBER (on me001+me006+me010) → MEST (on me001+me107)
   Takes the most recent MEST record per person to get a stable apcd_unique_id.
*/
proc sql;
    create table _commercial_mest_link as
    select d.submitter,
           d.group_policy,
           d.person_code,
           m.me107,
           t.apcd_unique_id,
           t.gender as mest_gender,
           catx('', t.apcd_unique_id, t.gender) as study_id length=50
    from dm_cohort_commercial d
    /* Step 1: MEMBER lookup to get me107 */
    left join (
        select me001, me006, me010, me107
        from arapcd.MEMBER
        where (me001, me006, me010, reckey) in (
            select me001, me006, me010, max(reckey)
            from arapcd.MEMBER
            group by me001, me006, me010
        )
    ) m
        on d.submitter = m.me001
       and d.group_policy = m.me006
       and d.person_code = m.me010
    /* Step 2: MEST lookup to get apcd_unique_id */
    left join (
        select submitter, member_id, apcd_unique_id, gender
        from arapcd.AR_APCD_24B_MEST
        where (submitter, member_id, year) in (
            select submitter, member_id, max(year)
            from arapcd.AR_APCD_24B_MEST
            group by submitter, member_id
        )
    ) t
        on m.me001 = t.submitter
       and m.me107 = t.member_id;
quit;

/* F-1 Report: MEST linkage success rate */
title "Step 5F-1: Commercial → MEST Linkage Success";
proc sql;
    select case when study_id ne '' then 'LINKED' else 'NOT_LINKED' end as mest_status,
           count(*) as n_patients format=comma12.,
           calculated n_patients / (select count(*) from _commercial_mest_link) * 100
               as pct format=5.1
    from _commercial_mest_link
    group by calculated mest_status;
quit;

/* F-2: Get Medicare FFS study_id from BEN_SUM.APCD_UNIQUE_ID */
/*
   MCR_BEN_SUM has APCD_UNIQUE_ID (Text, 512) per the Medicare APCD Element List.
   Join dm_cohort_medicare → BEN_SUM to get apcd_unique_id + gender = study_id.
*/
proc sql;
    create table _medicare_study_link as
    select d.bene_id,
           b.apcd_unique_id,
           b.sex_ident_cd as mcr_gender_raw,
           /* Harmonize: BEN_SUM sex_ident_cd 1=M, 2=F → match MEST gender M/F */
           case when b.sex_ident_cd = '1' then 'M'
                when b.sex_ident_cd = '2' then 'F'
                else 'U' end as mcr_gender length=1,
           catx('', b.apcd_unique_id,
                case when b.sex_ident_cd = '1' then 'M'
                     when b.sex_ident_cd = '2' then 'F'
                     else 'U' end) as study_id length=520
    from dm_cohort_medicare d
    left join (
        /* Most recent BEN_SUM per BENE_ID */
        select bene_id, apcd_unique_id, sex_ident_cd
        from arapcd.APCD_MCR_BEN_SUM
        where (bene_id, bene_enrollmt_ref_yr) in (
            select bene_id, max(bene_enrollmt_ref_yr)
            from arapcd.APCD_MCR_BEN_SUM
            group by bene_id
        )
    ) b
        on d.bene_id = b.bene_id;
quit;

/* F-2 Report: Medicare BEN_SUM APCD_UNIQUE_ID coverage */
title "Step 5F-2: Medicare → BEN_SUM APCD_UNIQUE_ID Linkage";
proc sql;
    select case when study_id ne '' then 'LINKED' else 'NOT_LINKED' end as link_status,
           count(*) as n_patients format=comma12.,
           calculated n_patients / (select count(*) from _medicare_study_link) * 100
               as pct format=5.1
    from _medicare_study_link
    group by calculated link_status;
quit;

/* F-3: Add study_id to the export dataset (BOTH commercial and Medicare) */
proc sql;
    create table dm_dfu_analytic_dedup as
    select a.*,
           case
               when a.data_source = 'COMMERCIAL' then c.study_id
               when a.data_source = 'MEDICARE' then m.study_id
               else ''
           end as study_id length=520
    from dm_dfu_analytic a
    left join _commercial_mest_link c
        on a.data_source = 'COMMERCIAL'
       and a.patient_id = catx('|', c.submitter, c.group_policy, c.person_code)
    left join _medicare_study_link m
        on a.data_source = 'MEDICARE'
       and a.patient_id = m.bene_id;
quit;

/* F-3 Report: Study_ID coverage in final dataset */
title "Step 5F-3: Study_ID Coverage in Final Dataset";
proc sql;
    select data_source,
           count(*) as n_patients format=comma12.,
           sum(case when study_id ne '' then 1 else 0 end) as has_study_id format=comma12.,
           sum(case when study_id = '' or study_id is null then 1 else 0 end) as no_study_id format=comma12.
    from dm_dfu_analytic_dedup
    group by data_source;
quit;

/* F-4: Cross-source deduplication — identify patients appearing in both sources */
title "Step 5F-4: Cross-Source Duplicates (same Study_ID in Commercial AND Medicare)";
proc sql;
    select count(*) as cross_source_duplicates format=comma12.
    from (
        select study_id
        from dm_dfu_analytic_dedup
        where study_id ne '' and study_id is not null
        group by study_id
        having count(distinct data_source) > 1
    );
quit;

/* Detailed cross-source overlap by DM type */
title "Step 5F-4b: Cross-Source Duplicates by DM Type";
proc sql;
    select a.diabetes_type as commercial_dm_type,
           b.diabetes_type as medicare_dm_type,
           count(*) as n_duplicates format=comma12.
    from dm_dfu_analytic_dedup a
    inner join dm_dfu_analytic_dedup b
        on a.study_id = b.study_id
       and a.data_source = 'COMMERCIAL'
       and b.data_source = 'MEDICARE'
       and a.study_id ne ''
    group by a.diabetes_type, b.diabetes_type
    order by n_duplicates desc;
quit;

/* F-5: Overwrite exports with dedup-enriched version (study_id now populated) */

/* --- cohort_commercial.csv (with study_id) --- */
proc export data=dm_dfu_analytic_dedup(
    where=(data_source='COMMERCIAL')
    keep=patient_id study_id diabetes_type first_dm_date ar_zip first_dm_year sex age
)   outfile="&outdir./cohort_commercial.csv"
    dbms=csv replace;
run;

/* --- cohort_medicare.csv (with study_id) --- */
proc export data=dm_dfu_analytic_dedup(
    where=(data_source='MEDICARE')
    keep=patient_id study_id diabetes_type first_dm_date ar_zip first_dm_year sex age
)   outfile="&outdir./cohort_medicare.csv"
    dbms=csv replace;
run;

/* --- dfu_commercial.csv (with study_id) --- */
proc export data=dm_dfu_analytic_dedup(
    where=(data_source='COMMERCIAL' and has_dfu=1)
    keep=patient_id study_id first_dfu_date dfu_source severity_rank days_dm_to_dfu
)   outfile="&outdir./dfu_commercial.csv"
    dbms=csv replace;
run;

/* --- dfu_medicare.csv (with study_id) --- */
proc export data=dm_dfu_analytic_dedup(
    where=(data_source='MEDICARE' and has_dfu=1)
    keep=patient_id study_id first_dfu_date dfu_source severity_rank days_dm_to_dfu
)   outfile="&outdir./dfu_medicare.csv"
    dbms=csv replace;
run;

/* Also keep combined analytic file */
proc export data=dm_dfu_analytic_dedup
    outfile="&outdir./dm_dfu_analytic.csv"
    dbms=csv replace;
run;

title "Step 5F-5: Final Dedup-Enriched Dataset Dimensions";
proc sql;
    select count(*) as total_rows format=comma12.,
           count(distinct patient_id) as unique_patient_ids format=comma12.,
           count(distinct case when study_id ne '' then study_id end) as unique_study_ids format=comma12.
    from dm_dfu_analytic_dedup;
quit;

title;

/*****************************************************************************
 ZIP CODE DATA QUALITY NOTES:

 1. PERSON IDENTIFICATION — COMPOSITE KEY:
    - Commercial/Medicaid persons are identified by a 3-field composite key:
        mc001 (submitter) + mc006 (group/policy, hashed) + mc009 (member seq)
    - MEMBER table equivalents: me001 + me006 + me010
    - patient_id in export = catx('|', mc001, mc006, mc009)
    - Using fewer fields risks collapsing distinct individuals.

 2. ZIP FIELD REALITY (5-DIGIT):
    - ME017 (MEMBER) and MC016 (CLAIM) contain 5-digit ZIP codes in practice,
      despite the data dictionary labeling them as "ZIP3".
    - These are suitable for ZCTA crosswalk at the 5-digit level.
    - Confirmed via field-length analysis in Step 2.

 3. ZIP SOURCE HIERARCHY:
    - MEMBER.ME017 (enrollment record) is preferred over CLAIM.MC016
    - CLAIM.MC016 is used as fallback when MEMBER ZIP is missing
    - Medicare uses BEN_SUM.ZIP_CD (always 5-digit, ~100% coverage on full server)

 4. MEDICARE ZIP COVERAGE NOTE:
    - BEN_SUM has ~100% ZIP coverage when joined on the full server.
    - The 92% missing rate seen in earlier sample exploration was a sample
      artifact: independent 1% draws from claims and BEN_SUM produced
      non-overlapping BENE_ID sets. This is NOT a data quality issue.

 5. DIAGNOSIS FIELDS:
    - Commercial claims have 14 DX fields: mc039, mc041 through mc053.
    - Steps 3-4 search all 14 fields for DM and DFU codes.

 6. NON-ARKANSAS ZIPS:
    - Some patients may have out-of-state ZIPs (border residents,
      snowbirds, military). These need a decision: include or exclude
      from ZCTA-level analysis.

 7. ANTICIPATED ZCTA CROSSWALK ISSUES:
    - Not all ZIPs map 1:1 to ZCTAs (PO Box ZIPs, military ZIPs)
    - Census ZCTA boundaries change between decennial censuses
    - 2020 Census ZCTAs should be used for consistency
    - HUD ZIP-to-ZCTA crosswalk or Census relationship file needed
*****************************************************************************/
