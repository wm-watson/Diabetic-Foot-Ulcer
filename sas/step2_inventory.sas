/*****************************************************************************
 Step 2: Lightweight Server Verification
 Purpose: Confirm critical column existence and data formats before running
          Steps 3-5. Uses dictionary.columns and tiny samples only.
          Does NOT scan full tables or compute aggregates.

 What we already know (from 1% sample + documentation):
   - DX fields: mc039, mc041-mc053 (NOT mc022/mc023)
   - Dates: mc017 is service date (NOT mc015)
   - Person key: mc001 + mc006 + mc009 (3-field composite)
   - ICD codes stored without dots (e.g., L97522 not L97.522)
   - ZIP fields contain 5-digit values despite dictionary saying "ZIP3"

 What we need to VERIFY on the full server:
   1. ME107 exists on MEMBER (MEST linkage depends on it)
   2. APCD_UNIQUE_ID exists on BEN_SUM (cross-source dedup depends on it)
   3. ADMTG_DGNS_CD exists on INP_CLM and SNF_CLM
   4. ZIP format confirmation (spot check)
   5. ICD code format confirmation (spot check)
*****************************************************************************/

libname arapcd odbc
    noprompt="dsn=APCD-24D;Trusted_connection=yes"
    schema=public;

/* ======================================================================= */
/* CHECK 1: Does MEMBER have ME107? (required for MEST linkage)            */
/* ======================================================================= */

title "CHECK 1: ME107 on MEMBER table";
proc sql;
    select name, type, length, label
    from dictionary.columns
    where libname = 'ARAPCD'
      and memname = 'MEMBER'
      and upcase(name) in ('ME107','MEMBER_ID');
quit;
/* EXPECTED: One row showing ME107, VarChar 128 */
/* IF EMPTY: MEST linkage will fail — halt and report to ACHI */

/* ======================================================================= */
/* CHECK 2: Does BEN_SUM have APCD_UNIQUE_ID? (cross-source dedup)        */
/* ======================================================================= */

title "CHECK 2: APCD_UNIQUE_ID on APCD_MCR_BEN_SUM";
proc sql;
    select name, type, length, label
    from dictionary.columns
    where libname = 'ARAPCD'
      and memname = 'APCD_MCR_BEN_SUM'
      and upcase(name) like '%APCD_UNIQUE%';
quit;
/* EXPECTED: One row showing APCD_UNIQUE_ID, Text 512 */
/* IF EMPTY: Cross-source dedup will fail — fall back to within-source only */

/* ======================================================================= */
/* CHECK 3: Does INP_CLM / SNF_CLM have ADMTG_DGNS_CD?                    */
/* ======================================================================= */

title "CHECK 3a: ADMTG_DGNS_CD on APCD_MCR_INP_CLM";
proc sql;
    select name, type, length
    from dictionary.columns
    where libname = 'ARAPCD'
      and memname = 'APCD_MCR_INP_CLM'
      and upcase(name) like '%ADMTG%';
quit;

title "CHECK 3b: ADMTG_DGNS_CD on APCD_MCR_SNF_CLM";
proc sql;
    select name, type, length
    from dictionary.columns
    where libname = 'ARAPCD'
      and memname = 'APCD_MCR_SNF_CLM'
      and upcase(name) like '%ADMTG%';
quit;

/* ======================================================================= */
/* CHECK 4: ZIP format spot check (20 rows only)                           */
/* ======================================================================= */

title "CHECK 4a: MEMBER ME017 — ZIP format (20 rows)";
proc sql outobs=20;
    select me017, length(strip(me017)) as zip_len
    from arapcd.MEMBER
    where me017 is not null and me017 ne '';
quit;

title "CHECK 4b: BEN_SUM ZIP_CD — format (20 rows)";
proc sql outobs=20;
    select zip_cd, length(strip(zip_cd)) as zip_len
    from arapcd.APCD_MCR_BEN_SUM
    where zip_cd is not null and zip_cd ne '';
quit;

/* ======================================================================= */
/* CHECK 5: ICD code format — dots or no dots? (20 rows)                   */
/* ======================================================================= */

title "CHECK 5a: Commercial DX format (20 rows with E10/E11/L97)";
proc sql outobs=20;
    select mc041, mc042, mc039
    from arapcd.CLAIM_SVC_DT_2023
    where upcase(mc041) like 'E1%' or upcase(mc041) like 'L97%';
quit;

title "CHECK 5b: Medicare DX format (20 rows with E10/E11/L97)";
proc sql outobs=20;
    select prncpal_dgns_cd, icd_dgns_cd1, icd_dgns_cd2
    from arapcd.APCD_MCR_PRTB_CAR_CLM
    where upcase(prncpal_dgns_cd) like 'E1%'
       or upcase(prncpal_dgns_cd) like 'L97%';
quit;

/* ======================================================================= */
/* CHECK 6: Quick table existence verification                             */
/* ======================================================================= */

title "CHECK 6: Key tables exist in ARAPCD schema";
proc sql;
    select memname as table_name
    from dictionary.tables
    where libname = 'ARAPCD'
      and memname in (
          'MEMBER',
          'CLAIM_SVC_DT_2017','CLAIM_SVC_DT_2018','CLAIM_SVC_DT_2019',
          'CLAIM_SVC_DT_2020','CLAIM_SVC_DT_2021','CLAIM_SVC_DT_2022',
          'CLAIM_SVC_DT_2023','CLAIM_SVC_DT_2024',
          'AR_APCD_24B_MEST',
          'APCD_MCR_BEN_SUM',
          'APCD_MCR_PRTB_CAR_CLM','APCD_MCR_OUT_CLM',
          'APCD_MCR_INP_CLM','APCD_MCR_SNF_CLM',
          'APCD_MCR_HHA_CLM','APCD_MCR_HSP_CLM','APCD_MCR_DME_CLM'
      )
    order by memname;
quit;
/* EXPECTED: 17 rows (8 CLAIM years + MEMBER + MEST + 7 MCR tables + BEN_SUM) */

title;

/*****************************************************************************
 INTERPRETATION GUIDE:

 CHECK 1 (ME107):
   - If ME107 found → proceed to Step 3
   - If not found → MEST linkage impossible; halt pipeline

 CHECK 2 (APCD_UNIQUE_ID on BEN_SUM):
   - If found → cross-source dedup works for both commercial + Medicare
   - If not found → can only dedup within-source; report limitation

 CHECK 3 (ADMTG_DGNS_CD):
   - If found → INP/SNF queries in Steps 3-4 will scan admitting dx
   - If not found → remove admtg_dgns_cd from INP/SNF WHERE clauses

 CHECK 4 (ZIP format):
   - If 5-digit → AR filter (prefix 71/72) works as written in Step 5
   - If 3-digit → need to adjust AR filter to just '71' and '72'

 CHECK 5 (ICD format):
   - If no dots (E1052, L97522) → current LIKE patterns work
   - If dots (E10.52, L97.522) → need to add dotted variants to combo code lists

 CHECK 6 (table existence):
   - All 17+ tables should be present
   - If any missing → adjust Steps 3-5 to skip that source
*****************************************************************************/
