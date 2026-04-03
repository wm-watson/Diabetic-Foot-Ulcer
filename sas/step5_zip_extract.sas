/*****************************************************************************
 Step 5: Extract Patient ZIP Codes, Link Study IDs, and Export CSV

 Fixes from prior version:
   - Replaced (col1,col2) IN (SELECT...) with JOINs (SAS SQL limitation)
   - Replaced ROW_NUMBER() OVER with GROUP BY (SAS SQL limitation)
   - Output to D:\WPWatson instead of Mac path
   - Database-heavy lookups use passthrough with GROUP BY
   - Added procedure fields (debridement, amputation) from step3

 Architecture:
   Part A: Passthrough extractions (MEMBER, BEN_SUM, MEST, claim ZIP)
   Part B: Commercial ZIP + demographics + MEST study_id
   Part C: Medicare ZIP + demographics + study_id from BEN_SUM
   Part D: ZIP quality reports
   Part E: Build final analytic dataset
   Part F: Export to CSV
   Part G: Cross-source dedup report + final summary
   Part H: Cleanup

 Prerequisite: Step 3 must be run first (dm_cohort_*, dfu_cohort_* in WORK).
*****************************************************************************/

libname arapcd odbc
    noprompt="dsn=APCD-24D;Trusted_connection=yes"
    schema=public;

libname mylib 'D:\WPWatson';
%let outdir = D:\WPWatson;

/* ======================================================================= */
/* PART A: Passthrough Extractions from Database Tables                     */
/*   All use GROUP BY + self-join on the database to get the most recent    */
/*   record per person. Results stream to D:\WPWatson (not WORK).           */
/* ======================================================================= */

/* A-1: Latest MEMBER record per person (most recent reckey)               */
/*      Includes me107 for MEST linkage later.                             */
proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._member_latest as
    select * from connection to odbc (
        select a.me001, a.me006, a.me010, a.me016, a.me017,
               a.me028, a.me014_curr_age, a.me107
        from public.MEMBER a
        inner join (
            select me001, me006, me010, max(reckey) as max_rk
            from public.MEMBER
            group by me001, me006, me010
        ) b on a.me001 = b.me001 and a.me006 = b.me006
           and a.me010 = b.me010 and a.reckey = b.max_rk
    );
    disconnect from odbc;
quit;
%put NOTE: A-1 MEMBER extraction complete.;

/* A-2: Latest BEN_SUM record per beneficiary (most recent enrollment yr)  */
/*      Includes apcd_unique_id for Medicare study_id.                     */
proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._bensym_latest as
    select * from connection to odbc (
        select a.bene_id, a.zip_cd, a.state_code, a.county_cd,
               a.sex_ident_cd, a.bene_race_cd, a.rti_race_cd,
               a.age_at_end_ref_yr, a.apcd_unique_id
        from public.APCD_MCR_BEN_SUM a
        inner join (
            select bene_id, max(bene_enrollmt_ref_yr) as max_yr
            from public.APCD_MCR_BEN_SUM
            group by bene_id
        ) b on a.bene_id = b.bene_id
           and a.bene_enrollmt_ref_yr = b.max_yr
    );
    disconnect from odbc;
quit;
%put NOTE: A-2 BEN_SUM extraction complete.;

/* A-3: Latest MEST record per submitter + member_id                       */
proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._mest_latest as
    select * from connection to odbc (
        select a.submitter, a.member_id, a.apcd_unique_id, a.gender
        from public.AR_APCD_24B_MEST a
        inner join (
            select submitter, member_id, max(year) as max_yr
            from public.AR_APCD_24B_MEST
            group by submitter, member_id
        ) b on a.submitter = b.submitter and a.member_id = b.member_id
           and a.year = b.max_yr
    );
    disconnect from odbc;
quit;
%put NOTE: A-3 MEST extraction complete.;

/* A-4: Claim ZIP fallback — one ZIP per person from 2024 claims           */
/*      Uses MAX(mc016) grouped by person (simple, avoids ROW_NUMBER).     */
proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._claim_zip_2024 as
    select * from connection to odbc (
        select mc001 as submitter, mc006 as group_policy, mc009 as person_code,
               max(mc016) as zip_claim
        from public.CLAIM_SVC_DT_2024
        where mc016 is not null and mc016 <> ''
        group by mc001, mc006, mc009
    );
    disconnect from odbc;
quit;
%put NOTE: A-4 Claim ZIP extraction complete.;

/* ======================================================================= */
/* PART B: Commercial ZIP + Demographics + Study ID                         */
/*   All joins are local (WORK + mylib). No ODBC overhead.                 */
/* ======================================================================= */

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
           /* ZIP: prefer MEMBER, fallback to CLAIM */
           case
               when m.me017 is not null and m.me017 ne '' then m.me017
               when cz.zip_claim is not null and cz.zip_claim ne '' then cz.zip_claim
               else ''
           end as zip_final length=10,
           case
               when m.me017 is not null and m.me017 ne '' then 'MEMBER'
               when cz.zip_claim is not null and cz.zip_claim ne '' then 'CLAIM'
               else 'MISSING'
           end as zip_source length=10,
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
           /* Procedure flags (from DM cohort — covers ALL diabetics, not just DFU) */
           d.has_debridement,
           d.first_debride_date,
           d.n_debride_claims,
           d.has_amputation,
           d.first_amp_date,
           d.n_amp_claims,
           /* MEST linkage: MEMBER.me107 -> MEST -> study_id */
           case when t.apcd_unique_id is not null and t.apcd_unique_id ne ''
                then catx('', t.apcd_unique_id, t.gender)
                else '' end as study_id length=150,
           'COMMERCIAL' as data_source length=10
    from dm_cohort_commercial d
    left join mylib._member_latest m
        on d.submitter = m.me001
       and d.group_policy = m.me006
       and d.person_code = m.me010
    left join mylib._claim_zip_2024 cz
        on d.submitter = cz.submitter
       and d.group_policy = cz.group_policy
       and d.person_code = cz.person_code
    left join dfu_cohort_commercial f
        on d.submitter = f.submitter
       and d.group_policy = f.group_policy
       and d.person_code = f.person_code
    /* MEST: use me107 from MEMBER to look up MEST */
    left join mylib._mest_latest t
        on m.me001 = t.submitter
       and m.me107 = t.member_id
    ;
quit;
%put NOTE: Part B commercial assembly complete.;

/* ======================================================================= */
/* PART C: Medicare ZIP + Demographics + Study ID                           */
/*   BEN_SUM has both zip_cd and apcd_unique_id.                           */
/* ======================================================================= */

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
           /* Procedure flags (from DM cohort — covers ALL diabetics, not just DFU) */
           d.has_debridement,
           d.first_debride_date,
           d.n_debride_claims,
           d.has_amputation,
           d.first_amp_date,
           d.n_amp_claims,
           /* Study ID from BEN_SUM.apcd_unique_id + harmonized gender */
           case when b.apcd_unique_id is not null and b.apcd_unique_id ne ''
                then catx('', b.apcd_unique_id,
                     case when b.sex_ident_cd = '1' then 'M'
                          when b.sex_ident_cd = '2' then 'F'
                          else 'U' end)
                else '' end as study_id length=150,
           'MEDICARE' as data_source length=10,
           case
               when b.zip_cd is not null and b.zip_cd ne '' then 'BEN_SUM'
               else 'MISSING'
           end as zip_source length=10
    from dm_cohort_medicare d
    left join mylib._bensym_latest b
        on d.bene_id = b.bene_id
    left join dfu_cohort_medicare f
        on d.bene_id = f.bene_id
    ;
quit;
%put NOTE: Part C Medicare assembly complete.;

/* ======================================================================= */
/* PART D: ZIP Code Quality Reports                                         */
/* ======================================================================= */

title "Step 5D-1: ZIP Code Completeness - Commercial";
proc sql;
    select zip_source,
           count(*) as n_patients format=comma12.,
           calculated n_patients / (select count(*) from dm_zip_commercial) * 100
               as pct format=5.1
    from dm_zip_commercial
    group by zip_source;
quit;

title "Step 5D-2: ZIP Code Completeness - Medicare";
proc sql;
    select zip_source,
           count(*) as n_patients format=comma12.,
           calculated n_patients / (select count(*) from dm_zip_medicare) * 100
               as pct format=5.1
    from dm_zip_medicare
    group by zip_source;
quit;

title "Step 5D-3: ZIP Length Distribution - Commercial";
proc sql;
    select length(strip(zip_final)) as zip_length,
           count(*) as n format=comma12.
    from dm_zip_commercial
    where zip_final ne ''
    group by calculated zip_length
    order by zip_length;
quit;

title "Step 5D-4: Non-Arkansas ZIPs - Commercial";
proc sql;
    select case
               when substr(strip(zip_final), 1, 2) in ('71','72') then 'ARKANSAS'
               when zip_final = '' or zip_final is null then 'MISSING'
               else 'NON-ARKANSAS'
           end as zip_state_cat,
           count(*) as n_patients format=comma12.,
           calculated n_patients / (select count(*) from dm_zip_commercial) * 100
               as pct format=5.1
    from dm_zip_commercial
    group by calculated zip_state_cat;
quit;

title "Step 5D-5: Non-Arkansas ZIPs - Medicare";
proc sql;
    select case
               when state_member = '05' then 'ARKANSAS'
               when zip_final = '' or zip_final is null then 'MISSING'
               else 'NON-ARKANSAS'
           end as state_cat,
           count(*) as n_patients format=comma12.,
           calculated n_patients / (select count(*) from dm_zip_medicare) * 100
               as pct format=5.1
    from dm_zip_medicare
    group by calculated state_cat;
quit;

title "Step 5D-6: MEST/Study_ID Linkage - Commercial";
proc sql;
    select case when study_id ne '' then 'LINKED' else 'NOT_LINKED' end as status,
           count(*) as n format=comma12.,
           calculated n / (select count(*) from dm_zip_commercial) * 100 as pct format=5.1
    from dm_zip_commercial
    group by calculated status;
quit;

title "Step 5D-7: Study_ID Linkage - Medicare";
proc sql;
    select case when study_id ne '' then 'LINKED' else 'NOT_LINKED' end as status,
           count(*) as n format=comma12.,
           calculated n / (select count(*) from dm_zip_medicare) * 100 as pct format=5.1
    from dm_zip_medicare
    group by calculated status;
quit;

/* ======================================================================= */
/* PART E: Build Final Analytic Dataset                                     */
/*   Stack commercial + Medicare with harmonized columns.                   */
/*   Filter to Arkansas ZIPs only.                                          */
/* ======================================================================= */

proc sql;
    create table dm_dfu_analytic as

    /* Commercial patients */
    select 'COMMERCIAL' as data_source length=10,
           catx('|', submitter, group_policy, person_code) as patient_id length=80,
           study_id,
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
           /* Procedure flags */
           has_debridement,
           first_debride_date,
           n_debride_claims,
           has_amputation,
           first_amp_date,
           n_amp_claims,
           substr(strip(zip_final), 1, 5) as ar_zip length=5,
           zip_source length=10,
           state_member as state length=5,
           '' as county length=5,
           /* Commercial me028 is already M/F */
           gender as sex length=2,
           age,
           '' as race_cd length=2,
           '' as rti_race_cd length=2,
           case when has_dfu = 1 and first_dfu_date is not null and first_dm_date is not null
                then intck('day', first_dm_date, first_dfu_date)
                else . end as days_dm_to_dfu
    from dm_zip_commercial
    where zip_final ne ''
      and substr(strip(zip_final), 1, 2) in ('71','72')

    union all

    /* Medicare patients */
    select 'MEDICARE' as data_source,
           bene_id as patient_id,
           study_id,
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
           /* Procedure flags */
           has_debridement,
           first_debride_date,
           n_debride_claims,
           has_amputation,
           first_amp_date,
           n_amp_claims,
           substr(strip(zip_final), 1, 5) as ar_zip length=5,
           zip_source,
           state_member as state,
           county,
           /* Medicare: 1=M, 2=F, else=U */
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
%put NOTE: Part E analytic dataset complete.;

/* ======================================================================= */
/* PART F: Export to CSV                                                     */
/* ======================================================================= */

/* --- cohort_commercial.csv --- */
proc export data=dm_dfu_analytic(
    where=(data_source='COMMERCIAL')
    keep=patient_id study_id diabetes_type first_dm_date ar_zip first_dm_year sex age
)   outfile="&outdir.\cohort_commercial.csv"
    dbms=csv replace;
run;

/* --- cohort_medicare.csv --- */
proc export data=dm_dfu_analytic(
    where=(data_source='MEDICARE')
    keep=patient_id study_id diabetes_type first_dm_date ar_zip first_dm_year sex age
)   outfile="&outdir.\cohort_medicare.csv"
    dbms=csv replace;
run;

/* --- dfu_commercial.csv --- */
proc export data=dm_dfu_analytic(
    where=(data_source='COMMERCIAL' and has_dfu=1)
    keep=patient_id study_id first_dfu_date dfu_source severity_rank
         has_debridement first_debride_date n_debride_claims
         has_amputation first_amp_date n_amp_claims
         days_dm_to_dfu
)   outfile="&outdir.\dfu_commercial.csv"
    dbms=csv replace;
run;

/* --- dfu_medicare.csv --- */
proc export data=dm_dfu_analytic(
    where=(data_source='MEDICARE' and has_dfu=1)
    keep=patient_id study_id first_dfu_date dfu_source severity_rank
         has_debridement first_debride_date n_debride_claims
         has_amputation first_amp_date n_amp_claims
         days_dm_to_dfu
)   outfile="&outdir.\dfu_medicare.csv"
    dbms=csv replace;
run;

/* --- Combined analytic file --- */
proc export data=dm_dfu_analytic
    outfile="&outdir.\dm_dfu_analytic.csv"
    dbms=csv replace;
run;

/* ======================================================================= */
/* PART G: Cross-Source Dedup Report + Final Summary                        */
/* ======================================================================= */

title "Step 5G-1: Final Analytic Dataset Summary";
proc sql;
    select data_source, diabetes_type, has_dfu,
           count(*) as n_patients format=comma12.
    from dm_dfu_analytic
    group by data_source, diabetes_type, has_dfu
    order by data_source, diabetes_type, has_dfu;
quit;

title "Step 5G-2: Final Dataset Dimensions";
proc sql;
    select count(*) as total_rows format=comma12.,
           count(distinct patient_id) as unique_patients format=comma12.,
           count(distinct case when study_id ne '' then study_id end)
               as unique_study_ids format=comma12.,
           sum(case when ar_zip ne '' then 1 else 0 end) / count(*) * 100
               as zip_completeness format=5.1
    from dm_dfu_analytic;
quit;

title "Step 5G-3: Cross-Source Duplicates (same Study_ID in Commercial AND Medicare)";
proc sql;
    select count(*) as cross_source_duplicates format=comma12.
    from (
        select study_id
        from dm_dfu_analytic
        where study_id ne '' and study_id is not null
        group by study_id
        having count(distinct data_source) > 1
    );
quit;

title "Step 5G-4: Cross-Source Duplicates by DM Type";
proc sql;
    select a.diabetes_type as commercial_dm_type,
           b.diabetes_type as medicare_dm_type,
           count(*) as n_duplicates format=comma12.
    from dm_dfu_analytic a
    inner join dm_dfu_analytic b
        on a.study_id = b.study_id
       and a.data_source = 'COMMERCIAL'
       and b.data_source = 'MEDICARE'
       and a.study_id ne ''
    group by a.diabetes_type, b.diabetes_type
    order by n_duplicates desc;
quit;

title "Step 5G-5: Time from First DM to First DFU (DFU patients only)";
proc sql;
    select data_source,
           count(*) as n_dfu_patients format=comma12.,
           sum(case when days_dm_to_dfu is not null then 1 else 0 end)
               as has_both_dates format=comma12.,
           avg(days_dm_to_dfu) as mean_days format=8.0,
           min(days_dm_to_dfu) as min_days format=8.0,
           max(days_dm_to_dfu) as max_days format=8.0
    from dm_dfu_analytic
    where has_dfu = 1
    group by data_source;
quit;

title "Step 5G-6: Procedure Code Summary - DFU Patients by Source";
proc sql;
    select data_source,
           count(*) as n_dfu format=comma12.,
           sum(has_debridement) as with_debridement format=comma12.,
           sum(has_amputation) as with_amputation format=comma12.,
           sum(case when has_debridement = 1 or ever_dm_combo = 1
                    then 1 else 0 end) as tier2_eligible format=comma12.,
           sum(ever_dm_combo) as with_combo_code format=comma12.
    from dm_dfu_analytic
    where has_dfu = 1
    group by data_source;
quit;

title "Step 5G-7: DFU Case Definition Tier Distribution (All DFU Patients)";
proc sql;
    select data_source,
           case when ever_dm_combo = 1 and has_debridement = 1 then 'COMBO+DEBRIDE'
                when ever_dm_combo = 1 then 'COMBO_ONLY'
                when has_debridement = 1 then 'L97+DEBRIDE'
                else 'L97_ONLY'
           end as dfu_evidence length=20,
           count(*) as n_patients format=comma12.
    from dm_dfu_analytic
    where has_dfu = 1
    group by data_source, calculated dfu_evidence
    order by data_source, dfu_evidence;
quit;

title "Step 5G-8: Amputation Summary - DFU Patients";
proc sql;
    select data_source,
           sum(has_amputation) as n_with_amp format=comma12.,
           sum(case when has_amputation = 1 and first_amp_date is not null
                     and first_dfu_date is not null
                then case when first_amp_date >= first_dfu_date then 1 else 0 end
                else 0 end) as amp_after_dfu format=comma12.,
           sum(case when has_amputation = 1 and first_amp_date is not null
                     and first_dfu_date is not null
                then case when first_amp_date < first_dfu_date then 1 else 0 end
                else 0 end) as amp_before_dfu format=comma12.
    from dm_dfu_analytic
    where has_dfu = 1
    group by data_source;
quit;

title;

/* ======================================================================= */
/* PART H: Cleanup                                                          */
/* ======================================================================= */

/* Delete passthrough extractions from D:\WPWatson */
proc datasets lib=mylib nolist;
    delete _member_latest _bensym_latest _mest_latest _claim_zip_2024;
quit;

/* Delete intermediate WORK datasets */
proc datasets lib=work nolist;
    delete dm_zip_commercial dm_zip_medicare;
quit;

%put NOTE: Step 5 complete. CSVs written to &outdir.;

/*****************************************************************************
 NOTES:

 1. SAS SQL COMPATIBILITY: All queries avoid multi-column IN subqueries
    and window functions (ROW_NUMBER, RANK). Instead:
    - "Most recent record" uses passthrough self-join with GROUP BY + MAX
    - "Claim ZIP fallback" uses GROUP BY with MAX(mc016)
    These patterns work in SAS SQL and push heavy computation to the database.

 2. MEMORY STRATEGY: Database-side extractions (MEMBER, BEN_SUM, MEST,
    claim ZIP) are stored on D:\WPWatson via passthrough GROUP BY. All
    subsequent joins are local (WORK + mylib). No large ODBC fetches
    during the join phase.

 3. STUDY_ID CONSTRUCTION:
    Commercial: MEMBER.me107 -> MEST(submitter+member_id) -> apcd_unique_id
                study_id = catx('', apcd_unique_id, mest_gender)
    Medicare:   BEN_SUM.apcd_unique_id directly
                study_id = catx('', apcd_unique_id, harmonized_gender)
    Cross-source duplicates = same study_id in both COMMERCIAL and MEDICARE.

 4. ZIP HIERARCHY:
    Commercial: MEMBER.me017 (preferred) -> CLAIM.mc016 (fallback)
    Medicare:   BEN_SUM.zip_cd (~100% coverage on full server)
    Arkansas filter: first 2 digits in ('71','72')

 5. SEX HARMONIZATION:
    Commercial me028: already M/F
    Medicare sex_ident_cd: 1->M, 2->F, else->U

 6. PROCEDURE FIELDS: Debridement and amputation flags flow from step3
    DFU cohort datasets. For non-DFU patients, these are 0/null.
    - has_debridement, first_debride_date, n_debride_claims
    - has_amputation, first_amp_date, n_amp_claims

 7. DFU TIER REPORTING: Step 5G-7 shows the tier-eligible distribution
    but does NOT assign a tier column. Tier assignment (Tier 1/2/3)
    happens in the R pipeline where temporal windows can be applied
    (e.g., debridement within +/-30 days of DFU claim).
*****************************************************************************/
