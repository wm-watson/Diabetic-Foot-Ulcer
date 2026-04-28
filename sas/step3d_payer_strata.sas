/*****************************************************************************
 Step 3d: Payer Stratification — Assign Primary Payer Category per Patient

 Purpose
 -------
 The base analysis (scripts 04 -> 05/07/08) collapses all medical payer
 types into a single cohort. Payer-stratified spatial analysis is needed
 to investigate whether hot/cold spots differ by payer mix -- particularly
 the Delta cold-spot pattern, which may be driven by Medicare-FFS dual
 eligibility and Memphis VA utilization rather than disease distribution.

 This script tags each apcd_unique_id with their PRIMARY payer category
 based on majority-month enrollment across 2017-2022. Output is a single
 CSV that the R pipeline (script 04) joins onto the cohort to filter.

 Payer Categories
 ----------------
 We collapse the granular MEST payer_type taxonomy into four mutually
 exclusive analytic strata:

   "MEDICARE"   = MEST.MCR_ADV (Medicare Advantage) + BEN_SUM presence
                  (Medicare FFS). Combines all Medicare in one stratum
                  because identifying FFS vs MA is unstable in the data.

   "MEDICAID"   = MCD, MCD_QHP, HCIP, PASSE
                  (Traditional Medicaid + Medicaid Expansion via QHP +
                  Health Care Independence Program + Provider-led ARK
                  Shared Savings Entity)

   "COMMERCIAL" = COM, QHP (non-Medicaid QHP), EBD (Employee Benefits
                  Division -- AR state employees)

   "MIXED"      = patient has substantial enrollment in >1 of the above
                  categories (>=20% of total enrolled months in two or
                  more strata). Reported separately rather than forced
                  into a single bucket.

 Assignment Rule
 ---------------
 1. For each apcd_unique_id, sum enrolled months 2017-2022 in each of
    MEDICARE, MEDICAID, COMMERCIAL.
 2. If max-share category >= 80% of total enrolled months -> assign
    that category.
 3. Else -> "MIXED".

 80% threshold matches CMS Chronic Conditions Data Warehouse convention
 for primary payer assignment.

 Prerequisites
 -------------
 step3c_enrollment.sas must have been run (we need the same MEST + BEN_SUM
 extraction logic). step3d is independent of step3c outputs since we
 re-pull the source tables here -- this keeps it self-contained.

 Outputs (D:\WPWatson)
 ---------------------
   payer_strata.csv      -- one row per apcd_unique_id with:
                              apcd_unique_id, gender,
                              months_medicare, months_medicaid,
                              months_commercial, months_total,
                              primary_payer_category

   payer_strata_summary.txt  -- counts per category for sanity check
*****************************************************************************/

libname arapcd odbc
    noprompt="dsn=APCD-24D;Trusted_connection=yes"
    schema=public;

libname mylib 'D:\WPWatson';
%let outdir = D:\WPWatson;

%let medicare_payers   = %str('MCR_ADV');
%let medicaid_payers   = %str('MCD','MCD_QHP','HCIP','PASSE');
%let commercial_payers = %str('COM','QHP','EBD');

%let yr_start  = 2017;
%let yr_end    = 2022;
%let year_list = %str('2017','2018','2019','2020','2021','2022');


/* ======================================================================= */
/* PART A: Pull MEST with payer_type retained, count enrolled months in    */
/*         each payer-category stratum per person.                          */
/* ======================================================================= */

proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._mest_strata as
    select * from connection to odbc (
        select apcd_unique_id,
               gender,
               case
                   when payer_type in (&medicare_payers)   then 'MEDICARE'
                   when payer_type in (&medicaid_payers)   then 'MEDICAID'
                   when payer_type in (&commercial_payers) then 'COMMERCIAL'
                   else 'OTHER'
               end as payer_cat,
               cast(year as integer) as year,
               sum(cast(months_covered_in_year as integer)) as months
        from public.AR_APCD_24B_MEST
        where payer_type in (&medicare_payers, &medicaid_payers, &commercial_payers)
          and year in (&year_list)
          and apcd_unique_id is not null
          and apcd_unique_id <> ''
        group by apcd_unique_id, gender,
                 case
                     when payer_type in (&medicare_payers)   then 'MEDICARE'
                     when payer_type in (&medicaid_payers)   then 'MEDICAID'
                     when payer_type in (&commercial_payers) then 'COMMERCIAL'
                     else 'OTHER'
                 end,
                 cast(year as integer)
    );
    disconnect from odbc;
quit;
%put NOTE: A-1 MEST strata extracted.;


/* ======================================================================= */
/* PART B: Pull Medicare FFS presence (BEN_SUM). Each year of presence     */
/*         counts as 12 months of MEDICARE coverage.                        */
/* ======================================================================= */

proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._ben_strata as
    select * from connection to odbc (
        select apcd_unique_id,
               'MEDICARE' as payer_cat,
               bene_enrollmt_ref_yr as year,
               12 as months
        from public.APCD_MCR_BEN_SUM
        where bene_enrollmt_ref_yr between &yr_start and &yr_end
          and apcd_unique_id is not null
          and apcd_unique_id <> ''
        group by apcd_unique_id, bene_enrollmt_ref_yr
    );
    disconnect from odbc;
quit;
%put NOTE: B-1 BEN_SUM strata extracted.;


/* ======================================================================= */
/* PART C: Combine, take MAX months per (person, year, payer_cat) so a     */
/*         person with both MA + FFS Medicare doesn't double-count.        */
/* ======================================================================= */

data mylib._strata_combined;
    length apcd_unique_id $90 payer_cat $10 gender $1;
    set mylib._mest_strata
        mylib._ben_strata (in=from_ffs);
    if from_ffs and missing(gender) then gender = 'U';
    if missing(months) then months = 0;
run;

proc sql;
    create table mylib._person_year_payer as
    select apcd_unique_id,
           max(gender) as gender length=1,
           year,
           payer_cat,
           max(months) as months   /* dedupe MA+FFS overlap within year */
    from mylib._strata_combined
    group by apcd_unique_id, year, payer_cat;
quit;
%put NOTE: C-1 Person-year-payer combined.;


/* ======================================================================= */
/* PART D: Aggregate to person-level: total enrolled months per payer cat  */
/*         across the full 2017-2022 window.                                */
/* ======================================================================= */

proc sql;
    create table mylib._person_payer as
    select apcd_unique_id,
           max(gender) as gender length=1,
           sum(case when payer_cat='MEDICARE'   then months else 0 end) as months_medicare,
           sum(case when payer_cat='MEDICAID'   then months else 0 end) as months_medicaid,
           sum(case when payer_cat='COMMERCIAL' then months else 0 end) as months_commercial
    from mylib._person_year_payer
    group by apcd_unique_id;
quit;

data mylib._person_payer;
    set mylib._person_payer;
    months_total = months_medicare + months_medicaid + months_commercial;

    /* Compute share per category */
    if months_total > 0 then do;
        share_medicare   = months_medicare   / months_total;
        share_medicaid   = months_medicaid   / months_total;
        share_commercial = months_commercial / months_total;
    end;
    else do;
        share_medicare = 0; share_medicaid = 0; share_commercial = 0;
    end;

    /* Primary payer rule: max share >= 80% -> that category, else MIXED */
    length primary_payer_category $10;
    if months_total = 0 then primary_payer_category = 'NONE';
    else if share_medicare >= 0.80 then primary_payer_category = 'MEDICARE';
    else if share_medicaid >= 0.80 then primary_payer_category = 'MEDICAID';
    else if share_commercial >= 0.80 then primary_payer_category = 'COMMERCIAL';
    else primary_payer_category = 'MIXED';

    drop share_:;
run;
%put NOTE: D-1 Person-level payer assignment complete.;


/* ======================================================================= */
/* PART E: Subset to DM cohort (load from analytic CSV like step3c does)   */
/* ======================================================================= */

proc import
    datafile="&outdir.\dm_dfu_analytic.csv"
    out=work._analytic_raw
    dbms=csv
    replace;
    getnames=yes;
    guessingrows=max;
run;

data work.dm_ids;
    length apcd_unique_id $90;
    set work._analytic_raw;
    if missing(study_id) or length(study_id) < 2 then apcd_unique_id = '';
    else apcd_unique_id = substr(study_id, 1, length(study_id) - 1);
    if apcd_unique_id ne '' then output;
    keep apcd_unique_id;
run;

proc sql;
    create table work.dm_ids_unique as
    select distinct apcd_unique_id from work.dm_ids;
quit;

proc sql;
    create table work.payer_strata_dm as
    select p.*
    from mylib._person_payer p
    inner join work.dm_ids_unique d
       on p.apcd_unique_id = d.apcd_unique_id;
quit;
%put NOTE: E-1 DM cohort joined to payer strata.;


/* ======================================================================= */
/* PART F: Export                                                           */
/* ======================================================================= */

proc export data=work.payer_strata_dm
    outfile="&outdir.\payer_strata.csv"
    dbms=csv replace;
run;

proc sql;
    create table work._summary as
    select primary_payer_category,
           count(*) as n,
           round(mean(months_medicare),   1) as avg_mo_mcr,
           round(mean(months_medicaid),   1) as avg_mo_mcd,
           round(mean(months_commercial), 1) as avg_mo_com,
           round(mean(months_total),      1) as avg_mo_tot
    from work.payer_strata_dm
    group by primary_payer_category;
quit;

filename sumrep "&outdir.\payer_strata_summary.txt";
data _null_;
    file sumrep;
    put "PRIMARY PAYER ASSIGNMENT (step3d)";
    put "==================================";
    put " ";
    put "Window: &yr_start - &yr_end (72 months max)";
    put "Rule: primary = max-share category if >=80%; else MIXED";
    put " ";
    put "Category   N           Avg months by source";
    put "-------------------------------------------------";
run;

data _null_;
    set work._summary;
    file sumrep mod;
    put primary_payer_category $-12 n 8. "    Mcr=" avg_mo_mcr 5.1
        "  Mcd=" avg_mo_mcd 5.1 "  Com=" avg_mo_com 5.1
        "  Tot=" avg_mo_tot 5.1;
run;

%put NOTE: Step 3d complete. Summary at &outdir.\payer_strata_summary.txt;


/* ======================================================================= */
/* PART G: Cleanup                                                          */
/* ======================================================================= */

proc datasets library=mylib nolist;
    delete _mest_strata _ben_strata _strata_combined _person_year_payer _person_payer;
quit;
