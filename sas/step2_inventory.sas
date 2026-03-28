/*****************************************************************************
 Step 2: Inventory AAPCD Tables
 Purpose: Document table names, dimensions, key variables, and join structure
 Uses PROC CONTENTS and obs= sampling — does NOT load full tables

 Data Dictionary Reference:
   - MEMBER: ME010 (person code) = primary member ID
   - CLAIM:  MC009 (person code) = joins to ME010
   - CLAIM_SVC_DT_2017–2025: year-partitioned subsets of CLAIM
   - MCR tables: BENE_ID = universal Medicare join key
   - AR_APCD_24B_MEST: enrollment selection for denominators
*****************************************************************************/

/* --- ODBC connection to AAPCD database --------------------------------- */
libname arapcd odbc
    noprompt="dsn=APCD-24D;Trusted_connection=yes"
    schema=public;

/* --- Output location --------------------------------------------------- */
%let outdir = /Users/williamwatson/Claude_Code/Aim_3_Dissertation;

/* ======================================================================= */
/* PART A: PROC CONTENTS on all key tables                                 */
/* ======================================================================= */

/* Macro to run PROC CONTENTS and capture metadata for each table */
%macro inventory_table(tbl);
    proc contents data=arapcd.&tbl. noprint
        out=_contents_&tbl.(keep=memname name type length format label nobs);
    run;

    /* Add table name and nobs to tracking dataset */
    data _inv_&tbl.;
        set _contents_&tbl.(obs=1 keep=memname nobs);
        table = "&tbl.";
    run;
%mend inventory_table;

/* --- Commercial / Medicaid core tables --------------------------------- */
%inventory_table(MEMBER);
%inventory_table(CLAIM_SVC_DT_2017);
%inventory_table(CLAIM_SVC_DT_2018);
%inventory_table(CLAIM_SVC_DT_2019);
%inventory_table(CLAIM_SVC_DT_2020);
%inventory_table(CLAIM_SVC_DT_2021);
%inventory_table(CLAIM_SVC_DT_2022);
%inventory_table(CLAIM_SVC_DT_2023);
%inventory_table(CLAIM_SVC_DT_2024);
%inventory_table(CLAIM_SVC_DT_2025);
%inventory_table(DENTAL);
%inventory_table(PHARMACY);
%inventory_table(PROVIDER);
%inventory_table(AR_APCD_24B_MEST);
%inventory_table(MEMBER_CARE);

/* --- Medicare tables --------------------------------------------------- */
%inventory_table(APCD_MCR_BEN_SUM);
%inventory_table(APCD_MCR_BEN_SUM_CC);
%inventory_table(APCD_MCR_PRTB_CAR_CLM);
%inventory_table(APCD_MCR_PRTB_CAR_LIN);
%inventory_table(APCD_MCR_OUT_CLM);
%inventory_table(APCD_MCR_INP_CLM);
%inventory_table(APCD_MCR_SNF_CLM);
%inventory_table(APCD_MCR_HHA_CLM);
%inventory_table(APCD_MCR_HSP_CLM);
%inventory_table(APCD_MCR_DME_CLM);
%inventory_table(APCD_MCR_PRTD_PHM_CLM);

/* --- Reference tables -------------------------------------------------- */
%inventory_table(LOOKUP);
%inventory_table(DISTINCT_REUSED_NDCS);

/* ======================================================================= */
/* PART B: Combine inventory and print summary                             */
/* ======================================================================= */

/* Stack all inventory rows */
data inventory_summary;
    set _inv_:;
run;

proc sql;
    title "AAPCD TABLE INVENTORY — Row Counts";
    select table, nobs format=comma20.
    from inventory_summary
    order by table;
quit;

/* ======================================================================= */
/* PART C: Verify key column names on critical tables                      */
/* ======================================================================= */

/* Print column details for tables we will use in Steps 3–5 */
title "MEMBER Table — Column Details";
proc print data=_contents_MEMBER noobs;
    var name type length format label;
run;

title "CLAIM_SVC_DT_2023 — Column Details (representative year)";
proc print data=_contents_CLAIM_SVC_DT_2023 noobs;
    var name type length format label;
run;

title "MCR BEN_SUM — Column Details";
proc print data=_contents_APCD_MCR_BEN_SUM noobs;
    var name type length format label;
run;

title "MCR BEN_SUM_CC — Column Details (Chronic Condition Flags)";
proc print data=_contents_APCD_MCR_BEN_SUM_CC noobs;
    var name type length format label;
run;

title "MCR PRTB_CAR_CLM — Column Details (Part B Carrier)";
proc print data=_contents_APCD_MCR_PRTB_CAR_CLM noobs;
    var name type length format label;
run;

/* ======================================================================= */
/* PART D: Sample rows from key tables to verify data formats              */
/* ======================================================================= */

/* Verify diagnosis code format in CLAIM */
/*
   NOTE: Data dictionary maps mc022=principal dx, mc023=dx2 — this is WRONG.
   Actual DX columns (confirmed from sample data):
     mc039 = admitting dx
     mc041 = principal dx (ICD-10-CM)
     mc042–mc053 = dx2–dx13
   Total: 14 diagnosis fields per claim.
   mc022 and mc023 contain numeric category codes, NOT ICD-10 codes.
*/
title "CLAIM_SVC_DT_2023 — Sample Diagnosis Codes (first 20 rows)";
proc sql outobs=20;
    select mc009, mc001, mc006, mc015, mc016,
           mc039, mc041, mc042, mc043,
           mc022 as mc022_NOT_dx
    from arapcd.CLAIM_SVC_DT_2023;
quit;

/* Verify ZIP code format: data dictionary says ZIP3, actual data may differ */
title "MEMBER — Sample ZIP and State (first 20 rows)";
proc sql outobs=20;
    select me010, me016, me017, me028
    from arapcd.MEMBER;
quit;

/* Verify ZIP length distribution in MEMBER */
title "MEMBER ME017 — Distribution of ZIP Code Lengths";
proc sql outobs=20;
    select length(strip(put(me017, $10.))) as zip_length,
           count(*) as n format=comma12.
    from arapcd.MEMBER(obs=100000)
    group by calculated zip_length
    order by zip_length;
quit;

/* Verify ZIP length distribution in CLAIM */
title "CLAIM MC016 — Distribution of ZIP Code Lengths";
proc sql outobs=20;
    select length(strip(put(mc016, $10.))) as zip_length,
           count(*) as n format=comma12.
    from arapcd.CLAIM_SVC_DT_2023(obs=100000)
    group by calculated zip_length
    order by zip_length;
quit;

/* Medicare BEN_SUM ZIP verification */
title "MCR BEN_SUM — Sample ZIP, State, County (first 20 rows)";
proc sql outobs=20;
    select bene_id, zip_cd, state_code, county_cd, sex_ident_cd,
           bene_race_cd, age_at_end_ref_yr
    from arapcd.APCD_MCR_BEN_SUM;
quit;

/* Medicare BEN_SUM_CC — check diabetes flag structure */
title "MCR BEN_SUM_CC — Diabetes Flag Distribution";
proc sql;
    select diabetes, count(*) as n format=comma12.
    from arapcd.APCD_MCR_BEN_SUM_CC
    group by diabetes;
quit;

/* ======================================================================= */
/* PART E: Year coverage verification                                      */
/* ======================================================================= */

/* Count rows per year-partitioned CLAIM table */
title "CLAIM_SVC_DT — Row Counts by Year Table";
proc sql;
    select 'CLAIM_SVC_DT_2017' as table_year, count(*) as n format=comma15. from arapcd.CLAIM_SVC_DT_2017
    union all
    select 'CLAIM_SVC_DT_2018', count(*) from arapcd.CLAIM_SVC_DT_2018
    union all
    select 'CLAIM_SVC_DT_2019', count(*) from arapcd.CLAIM_SVC_DT_2019
    union all
    select 'CLAIM_SVC_DT_2020', count(*) from arapcd.CLAIM_SVC_DT_2020
    union all
    select 'CLAIM_SVC_DT_2021', count(*) from arapcd.CLAIM_SVC_DT_2021
    union all
    select 'CLAIM_SVC_DT_2022', count(*) from arapcd.CLAIM_SVC_DT_2022
    union all
    select 'CLAIM_SVC_DT_2023', count(*) from arapcd.CLAIM_SVC_DT_2023
    union all
    select 'CLAIM_SVC_DT_2024', count(*) from arapcd.CLAIM_SVC_DT_2024
    union all
    select 'CLAIM_SVC_DT_2025', count(*) from arapcd.CLAIM_SVC_DT_2025;
quit;

/* MEST enrollment coverage */
title "AR_APCD_24B_MEST — Year Distribution";
proc sql;
    select year, count(*) as n_members format=comma12.
    from arapcd.AR_APCD_24B_MEST
    group by year
    order by year;
quit;

/* MCR BEN_SUM reference year distribution */
title "MCR BEN_SUM — Reference Year Distribution";
proc sql;
    select bene_enrollmt_ref_yr, count(*) as n_benes format=comma12.
    from arapcd.APCD_MCR_BEN_SUM
    group by bene_enrollmt_ref_yr
    order by bene_enrollmt_ref_yr;
quit;

title;

/* ======================================================================= */
/* PART F: Document join relationship validation                           */
/* ======================================================================= */

/* Verify MEMBER.ME010 = CLAIM.MC009 join works (small sample) */
title "Join Validation: MEMBER.ME010 = CLAIM_SVC_DT_2023.MC009 (sample)";
proc sql outobs=10;
    select m.me010, m.me001, m.me006, m.me016, m.me017, m.me028,
           c.mc009, c.mc001, c.mc006, c.mc015, c.mc016, c.mc041
    from arapcd.MEMBER(obs=1000) m
    inner join arapcd.CLAIM_SVC_DT_2023(obs=10000) c
        on m.me010 = c.mc009
           and m.me001 = c.mc001
           and m.me006 = c.mc006;
quit;

title;

/* ======================================================================= */
/* PART G: MEST Table Exploration — Person Linkage for Deduplication        */
/* ======================================================================= */

/*
   MEST LINKAGE (from ACHI "Guide to the APCD Member Enrollment Selection Table"):

   MEST columns: submitter (ME001), member_id (ME107), apcd_unique_id (ME998),
                 gender (ME013), payer_type, year, enrollment_string,
                 months_covered_in_year

   Two person identifiers:
     SE_ID    = Submitter (ME001) + Member_ID (ME107) — unique within submitter
     Study_ID = APCD_Unique_ID (ME998) + Gender (ME013) — unique across submitters

   APCD_Unique_ID = hash of member's last name + date of birth (Char 44).
   Use Study_ID (apcd_unique_id + gender) for cross-payer deduplication.

   JOIN PATH (MEMBER → MEST):
     MEMBER.me001 = MEST.submitter  AND  MEMBER.me107 = MEST.member_id
   MEST.member_id is ME107 (submitter-specific unique member ID), NOT ME006.
   This explains why sample MEST.member_id had only 964 overlaps with MEMBER.me006.

   CLAIM → MEST goes through MEMBER:
     CLAIM (mc001+mc006+mc009) → MEMBER (me001+me006+me010) → MEST (me001+me107)

   IMPORTANT: MEST covers commercial, Medicaid, MCR_ADV, QHP, PBM, etc.
   Medicare FFS (APCD_MCR_* tables from CMS) uses BENE_ID and is NOT in MEST.
   Cross-source dedup between commercial and Medicare FFS requires proxy matching.
*/

/* G-1: MEST column listing */
title "AR_APCD_24B_MEST — All Columns";
proc contents data=arapcd.AR_APCD_24B_MEST noprint
    out=_contents_MEST(keep=memname name type length format label);
run;
proc print data=_contents_MEST noobs; var name type length format label; run;

/* G-2: Check if MEMBER table has ME107 (submitter-specific unique member ID) */
title "MEST Linkage: Does MEMBER have ME107 or similar unique member ID?";
proc sql;
    select name, type, length, label
    from dictionary.columns
    where libname = 'ARAPCD'
      and memname = 'MEMBER'
      and (upcase(name) like '%ME107%'
           or upcase(name) like '%MEMBER_ID%'
           or upcase(name) like '%APCD%'
           or upcase(name) like '%UNIQUE%'
           or upcase(name) like '%HASH%'
           or upcase(name) like '%ME998%');
quit;

/* G-3: Check if CLAIM has ME107-equivalent or apcd_unique_id */
title "MEST Linkage: Does CLAIM_SVC_DT_2023 have ME107 or apcd_unique_id?";
proc sql;
    select name, type, length, label
    from dictionary.columns
    where libname = 'ARAPCD'
      and memname = 'CLAIM_SVC_DT_2023'
      and (upcase(name) like '%ME107%'
           or upcase(name) like '%MEMBER_ID%'
           or upcase(name) like '%APCD%'
           or upcase(name) like '%UNIQUE%'
           or upcase(name) like '%HASH%'
           or upcase(name) like '%ME998%');
quit;

/* G-4: MEST payer type distribution */
title "AR_APCD_24B_MEST — Payer Type Distribution";
proc sql;
    select payer_type,
           count(*) as n_rows format=comma12.,
           count(distinct apcd_unique_id) as n_unique_persons format=comma12.
    from arapcd.AR_APCD_24B_MEST
    group by payer_type
    order by n_unique_persons desc;
quit;

/* G-5: Total unique persons (cross-payer) */
title "AR_APCD_24B_MEST — Total Unique Study_IDs (apcd_unique_id + gender)";
proc sql;
    select count(distinct catx('', apcd_unique_id, gender)) as n_unique_study_ids format=comma12.,
           count(distinct apcd_unique_id) as n_unique_apcd_ids format=comma12.
    from arapcd.AR_APCD_24B_MEST;
quit;

/* G-6: Test MEMBER → MEST join via ME001 + ME107 */
/*
   Per the MEST guide, SE_ID = Submitter (ME001) + Member_ID (ME107).
   If ME107 exists in MEMBER, test the join:
*/
title "MEST Linkage Test: MEMBER.me001+me107 → MEST.submitter+member_id (sample)";
proc sql outobs=10;
    select m.me001, m.me107, m.me010, m.me006,
           t.submitter, t.member_id, t.apcd_unique_id, t.payer_type, t.year
    from arapcd.MEMBER(obs=5000) m
    inner join arapcd.AR_APCD_24B_MEST t
        on m.me001 = t.submitter
       and m.me107 = t.member_id;
quit;

/* G-7: Quantify join success rate */
title "MEST Linkage Test: Join hit rate — MEMBER(sample 100K) → MEST";
proc sql;
    select count(distinct catx('|', m.me001, m.me107)) as member_keys format=comma12.,
           count(distinct catx('|', m.me001, m.me107)) -
               count(distinct case when t.apcd_unique_id is not null
                             then catx('|', m.me001, m.me107) end) as no_mest_match format=comma12.,
           count(distinct case when t.apcd_unique_id is not null
                         then catx('|', m.me001, m.me107) end) as has_mest_match format=comma12.
    from arapcd.MEMBER(obs=100000) m
    left join arapcd.AR_APCD_24B_MEST t
        on m.me001 = t.submitter
       and m.me107 = t.member_id;
quit;

/* G-8: Count persons appearing in multiple payer types (the people we'd dedup) */
title "MEST: Persons with Multiple Payer Types (cross-payer overlap)";
proc sql;
    select n_payer_types,
           count(*) as n_persons format=comma12.
    from (
        select catx('', apcd_unique_id, gender) as study_id,
               count(distinct payer_type) as n_payer_types
        from arapcd.AR_APCD_24B_MEST
        group by calculated study_id
    )
    group by n_payer_types
    order by n_payer_types;
quit;

/* G-9: MCR_BEN_SUM has APCD_UNIQUE_ID per Medicare element list */
/*
   The Medicare APCD Element List shows APCD_UNIQUE_ID (Text, 512) on MCR_BEN_SUM.
   This means Medicare FFS beneficiaries CAN be linked to the same cross-payer
   identifier used in MEST, enabling full cross-source deduplication.
*/
title "MCR BEN_SUM: Verify APCD_UNIQUE_ID column exists";
proc sql;
    select name, type, length, label
    from dictionary.columns
    where libname = 'ARAPCD'
      and memname = 'APCD_MCR_BEN_SUM'
      and (upcase(name) like '%APCD%'
           or upcase(name) like '%UNIQUE%'
           or upcase(name) like '%HASH%');
quit;

/* G-10: Sample APCD_UNIQUE_ID values from BEN_SUM */
title "MCR BEN_SUM: Sample APCD_UNIQUE_ID values (first 10)";
proc sql outobs=10;
    select bene_id, apcd_unique_id, sex_ident_cd, zip_cd, state_code
    from arapcd.APCD_MCR_BEN_SUM;
quit;

/* G-11: APCD_UNIQUE_ID coverage on BEN_SUM */
title "MCR BEN_SUM: APCD_UNIQUE_ID completeness";
proc sql;
    select count(*) as total_benes format=comma12.,
           sum(case when apcd_unique_id is not null and apcd_unique_id ne ''
                    then 1 else 0 end) as has_apcd_uid format=comma12.,
           sum(case when apcd_unique_id is null or apcd_unique_id = ''
                    then 1 else 0 end) as missing_apcd_uid format=comma12.
    from arapcd.APCD_MCR_BEN_SUM;
quit;

/* G-12: Test cross-source linkage — do any BEN_SUM APCD_UNIQUE_IDs appear in MEST? */
title "Cross-Source Linkage: BEN_SUM.APCD_UNIQUE_ID overlap with MEST";
proc sql;
    select count(distinct b.apcd_unique_id) as ben_sum_apcd_ids format=comma12.,
           (select count(distinct apcd_unique_id) from arapcd.AR_APCD_24B_MEST)
               as mest_apcd_ids format=comma12.,
           count(distinct case when t.apcd_unique_id is not null
                         then b.apcd_unique_id end) as overlap format=comma12.
    from (select distinct apcd_unique_id
          from arapcd.APCD_MCR_BEN_SUM
          where apcd_unique_id is not null and apcd_unique_id ne '') b
    left join (select distinct apcd_unique_id
               from arapcd.AR_APCD_24B_MEST) t
        on b.apcd_unique_id = t.apcd_unique_id;
quit;

/* G-13: Verify ADMTG_DGNS_CD on INP_CLM (per Medicare element list) */
title "MCR INP_CLM: Verify ADMTG_DGNS_CD column";
proc sql outobs=5;
    select name, type, length
    from dictionary.columns
    where libname = 'ARAPCD'
      and memname = 'APCD_MCR_INP_CLM'
      and upcase(name) like '%ADMTG%';
quit;

title;

/*****************************************************************************
 DATA MODEL SUMMARY (from data dictionary + verification above):

 COMMERCIAL/MEDICAID PATH:
   Person key: 3-field composite — MC001 (submitter) + MC006 (group/policy) + MC009 (member seq)
   MEMBER equivalent: ME001 + ME006 + ME010
   Diagnosis codes: 14 fields per claim:
     mc039 (admitting), mc041 (principal), mc042–mc053 (dx2–dx13)
     ** NOTE: Data dictionary says mc022=principal dx — this is WRONG.
        mc022 contains a numeric category code, NOT an ICD-10 code. **
   ZIP: ME017 on MEMBER, MC016 on CLAIM
     ** DATA DICTIONARY says ZIP3 but actual data has 5-digit ZIP **
   Service dates: MC015 (from date), MC017 (to date)

 MEDICARE PATH (from MCR APCD Element List FINAL.xlsx):
   APCD_MCR_BEN_SUM.BENE_ID = all MCR claim tables
   BEN_SUM also has APCD_UNIQUE_ID (Text, 512) — enables cross-source dedup!
   BEN_SUM_CC has diabetes/diabetes_mid/diabetes_ever flags (no T1D/T2D split)
   PRTB_CAR_CLM: prncpal_dgns_cd + icd_dgns_cd1..12 (13 DX fields)
   OUT_CLM: prncpal_dgns_cd + icd_dgns_cd1..25 (26 DX fields)
   INP_CLM: admtg_dgns_cd + prncpal_dgns_cd + icd_dgns_cd1..25 (27 DX fields)
   SNF_CLM: admtg_dgns_cd + prncpal_dgns_cd + icd_dgns_cd1..25 (27 DX fields)
   HHA/HSP_CLM: prncpal_dgns_cd + icd_dgns_cd1..25 (26 DX fields)
   DME_CLM: prncpal_dgns_cd + icd_dgns_cd1..12 (13 DX fields)
   ZIP: zip_cd on BEN_SUM (5-digit confirmed, ~100% coverage on full server)
   Claim-level ZIP: bene_mlg_cntct_zip_cd (noted as ZIP3 in element list)

 MEST LINKAGE (from ACHI Guide):
   - SE_ID (within-submitter) = Submitter (ME001) + Member_ID (ME107)
   - Study_ID (cross-submitter) = APCD_Unique_ID (ME998) + Gender (ME013)
   - APCD_Unique_ID = hash of last name + DOB (Char 44)
   - Join: MEMBER.me001+me107 = MEST.submitter+member_id → gets apcd_unique_id
   - CLAIM → MEMBER → MEST (no direct CLAIM → MEST join)
   - Medicare FFS: BEN_SUM has APCD_UNIQUE_ID (Text, 512) — enables cross-source dedup!
   - Study_ID = APCD_UNIQUE_ID + Gender works for BOTH commercial and Medicare FFS

 KNOWN ISSUES:
   1. Data dictionary DX column mapping is WRONG (mc022/mc023 are NOT diagnosis codes)
   2. ZIP field labeling discrepancy between dictionary (ZIP3) and data (5-digit)
   3. CLAIM master table (~11 GB) too large to scan; use year-partitioned tables
   4. BEN_SUM_CC diabetes flags do not distinguish T1D vs T2D
   5. BEN_SUM ZIP joins on sample data show ~92% missing — this is a SAMPLE ARTIFACT
      (independent sample draws). Full-server joins resolve correctly (~100% coverage).
   6. MEST.member_id (ME107) != MEMBER.me006 — they are different fields
*****************************************************************************/
