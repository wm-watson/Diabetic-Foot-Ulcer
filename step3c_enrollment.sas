/*****************************************************************************
 Step 3c: Enrollment-Based Denominator Construction from MEST + BEN_SUM

 Purpose
 -------
 Replace the claims-observed denominator (count of distinct patient_ids with
 any claim in a bin) with an enrollment-weighted denominator derived from
 the Member Enrollment Selection Table (MEST). This addresses:

   - Insurance churn: patients who lose coverage mid-window drop out of
     the claims-observed denominator entirely, inflating ZCTA rates in
     high-churn (typically rural, Medicaid-heavy) areas.
   - Healthy-user invisibility: well-managed diabetics with few visits
     are undercounted relative to high-utilizers.
   - Payer transitions: a patient switching Medicaid -> Commercial
     vanishes from the claims-observed denominator during the
     transition gap.

 Data Sources
 ------------
 1. AR_APCD_24B_MEST — monthly enrollment flags per person per payer per
    year. Covers Commercial, Medicaid, Medicare Advantage, QHP, PBM, etc.
    Does NOT cover Medicare FFS.
    Key fields: apcd_unique_id, gender, payer_type, year, enrollment_string
    (12-char binary, Jan=char1 ... Dec=char12), months_covered_in_year.

 2. APCD_MCR_BEN_SUM — Medicare FFS beneficiary records, year-level only.
    bene_enrollmt_ref_yr gives year-of-presence; no monthly granularity.
    Assumption: if a bene_id has any BEN_SUM record for year Y, treat
    them as enrolled all 12 months of year Y. Standard assumption given
    that FFS disenrollment mid-year is rare (death or MA switch, either
    of which removes them from further claims).

 Cohort Definitions (two cohorts exported)
 -----------------------------------------
   Cohort 1 (Primary):      cohort_continuous.csv
     Patient must have >=1 month of medical coverage in EVERY month
     of the 72-month window (2017-01 through 2022-12). Coverage can
     switch across payers — we take the bitwise OR of all medical
     payer_types for each person-year before checking continuity.

   Cohort 2 (Sensitivity):  cohort_fractional.csv
     All DM patients with >=1 month of medical coverage anywhere in
     2017-2022. Each person contributes fractional person-halfyears
     proportional to enrolled months in each bin. Per-bin floor of
     3/6 months is applied at the analysis stage in R (not filtered
     here) so the CSV keeps the raw monthly counts.

 Medical Payer Type Definition
 -----------------------------
 Included (can generate medical claims with DX codes):
   COM, MCD, MCR_ADV, QHP, HCIP, EBD, PASSE, MCD_QHP
 Excluded (no medical claims):
   DNT (dental only), PBM (pharmacy only), MCRAdvPhrm (MA pharmacy Part D
   carve-out), EBD_PBM, EBD_TPA, EBD_RET

 Prerequisites
 -------------
 Step 3 (step3_cohort.sas) must have been run — requires dm_cohort_commercial
 and dm_cohort_medicare in WORK, both carrying apcd_unique_id (from MEST for
 commercial, from BEN_SUM for Medicare).

 Outputs (D:\WPWatson)
 ---------------------
   cohort_continuous.csv   — DM patients continuously enrolled 2017-2022
   cohort_fractional.csv   — all DM patients with enrolled months per bin
                             (12 columns: m_h1_2017 ... m_h2_2022)
   enrollment_summary.txt  — sample sizes at each cohort definition

 Authors: William P. Watson, PhD Candidate (UAMS)
 Last updated: 2026-04-21
*****************************************************************************/

libname arapcd odbc
    noprompt="dsn=APCD-24D;Trusted_connection=yes"
    schema=public;

libname mylib 'D:\WPWatson';
%let outdir = D:\WPWatson;

/* Medical payer types (WHERE clause fragment) */
%let medical_payers = %str('COM','MCD','MCR_ADV','QHP','HCIP','EBD','PASSE','MCD_QHP');

/* Study window */
%let yr_start = 2017;
%let yr_end   = 2022;


/* ======================================================================= */
/* PART A: MEST passthrough — one row per apcd_unique_id per year,          */
/*         merging (bitwise OR) enrollment_string across medical payers.    */
/* ======================================================================= */

/*
 The MEST can have multiple rows for the same person in the same year
 (e.g., MCR_ADV + COM simultaneously, or transitioning COM -> MCD). We
 need the UNION of their monthly enrollment. The bitwise OR is done via
 per-character MAX() across rows within person-year.

 SAS SQL (and most ODBC backends) lack a bitwise OR aggregate, but we
 can implement it by SUBSTRING'ing each month position and taking MAX:
   month_1 = MAX(SUBSTRING(enrollment_string, 1, 1))
   ...
   month_12 = MAX(SUBSTRING(enrollment_string, 12, 1))
 Then CONCAT back into a 12-char string.

 For Postgres/SQL Server ODBC the syntax is SUBSTRING(col FROM pos FOR 1)
 or SUBSTRING(col, pos, 1). We use the ANSI SUBSTRING(col, pos, 1) form.
*/

proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._mest_enrollment as
    select * from connection to odbc (
        select apcd_unique_id,
               gender,
               year,
               max(substring(enrollment_string, 1,  1)) as m01,
               max(substring(enrollment_string, 2,  1)) as m02,
               max(substring(enrollment_string, 3,  1)) as m03,
               max(substring(enrollment_string, 4,  1)) as m04,
               max(substring(enrollment_string, 5,  1)) as m05,
               max(substring(enrollment_string, 6,  1)) as m06,
               max(substring(enrollment_string, 7,  1)) as m07,
               max(substring(enrollment_string, 8,  1)) as m08,
               max(substring(enrollment_string, 9,  1)) as m09,
               max(substring(enrollment_string, 10, 1)) as m10,
               max(substring(enrollment_string, 11, 1)) as m11,
               max(substring(enrollment_string, 12, 1)) as m12
        from public.AR_APCD_24B_MEST
        where payer_type in (&medical_payers)
          and year between &yr_start and &yr_end
          and apcd_unique_id is not null
          and apcd_unique_id <> ''
        group by apcd_unique_id, gender, year
    );
    disconnect from odbc;
quit;
%put NOTE: A-1 MEST medical-coverage enrollment extracted.;


/* ======================================================================= */
/* PART B: BEN_SUM passthrough — Medicare FFS presence per beneficiary      */
/*         per year (year-level only; monthly flags not in APCD extract).   */
/* ======================================================================= */

proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._bensum_years as
    select * from connection to odbc (
        select bene_id,
               apcd_unique_id,
               sex_ident_cd,
               bene_enrollmt_ref_yr as year
        from public.APCD_MCR_BEN_SUM
        where bene_enrollmt_ref_yr between &yr_start and &yr_end
          and apcd_unique_id is not null
          and apcd_unique_id <> ''
    );
    disconnect from odbc;
quit;
%put NOTE: B-1 BEN_SUM year-level presence extracted.;


/* ======================================================================= */
/* PART C: Unify MEST + BEN_SUM into a single person-year enrollment table  */
/*         keyed on apcd_unique_id. Medicare FFS gets full 12-month strings */
/*         for each year of BEN_SUM presence.                               */
/* ======================================================================= */

data mylib._enrollment_unified;
    length source $4 apcd_unique_id $90 gender $1;
    set
        mylib._mest_enrollment (in=from_mest)
        mylib._bensum_years    (in=from_bensum
                                rename=(sex_ident_cd=_sex_code));

    if from_mest then source = 'MEST';
    else if from_bensum then do;
        source = 'FFS';
        /* Harmonize sex_ident_cd (1/2) -> M/F to match MEST */
        if _sex_code = '1' then gender = 'M';
        else if _sex_code = '2' then gender = 'F';
        else gender = 'U';
        /* Full 12-month enrollment assumption for Medicare FFS */
        m01='1'; m02='1'; m03='1'; m04='1'; m05='1'; m06='1';
        m07='1'; m08='1'; m09='1'; m10='1'; m11='1'; m12='1';
    end;

    drop _sex_code;
run;
%put NOTE: C-1 Unified enrollment table built.;

/*
 Within an apcd_unique_id + year, a person might appear in BOTH MEST
 (MA) and BEN_SUM (FFS). That's unusual but possible (e.g., MA
 enrollment for part of the year then switch to FFS). Take bitwise OR
 across both sources for the final monthly flags.
*/

proc sql;
    create table mylib._enrollment_by_year as
    select apcd_unique_id,
           /* Gender: prefer non-U if conflict */
           case when max(gender) = 'U' then min(gender) else max(gender) end as gender,
           year,
           /* Bitwise OR across source rows within person-year */
           max(m01) as m01, max(m02) as m02, max(m03) as m03,
           max(m04) as m04, max(m05) as m05, max(m06) as m06,
           max(m07) as m07, max(m08) as m08, max(m09) as m09,
           max(m10) as m10, max(m11) as m11, max(m12) as m12
    from mylib._enrollment_unified
    group by apcd_unique_id, year;
quit;
%put NOTE: C-2 Source-level bitwise OR complete.;


/* ======================================================================= */
/* PART D: Half-year bin rollup                                              */
/*   Compute months enrolled in each of 12 bins (H1/H2 × 6 years).          */
/* ======================================================================= */

data mylib._enrollment_bins;
    set mylib._enrollment_by_year;

    /* Monthly flags are '0'/'1' characters — convert to numeric 0/1 */
    array mchar {12} $1 m01-m12;
    array mnum  {12}    n01-n12;
    do i = 1 to 12;
        mnum[i] = input(mchar[i], 1.);
        if missing(mnum[i]) then mnum[i] = 0;
    end;

    h1_months = sum(of n01-n06);
    h2_months = sum(of n07-n12);

    drop i n01-n12;
run;

/* Pivot to wide: one row per apcd_unique_id, 12 columns of bin-months */
proc sql;
    create table mylib._enrollment_wide as
    select apcd_unique_id,
           max(gender) as gender length=1,
           sum(case when year=2017 then h1_months else 0 end) as m_h1_2017,
           sum(case when year=2017 then h2_months else 0 end) as m_h2_2017,
           sum(case when year=2018 then h1_months else 0 end) as m_h1_2018,
           sum(case when year=2018 then h2_months else 0 end) as m_h2_2018,
           sum(case when year=2019 then h1_months else 0 end) as m_h1_2019,
           sum(case when year=2019 then h2_months else 0 end) as m_h2_2019,
           sum(case when year=2020 then h1_months else 0 end) as m_h1_2020,
           sum(case when year=2020 then h2_months else 0 end) as m_h2_2020,
           sum(case when year=2021 then h1_months else 0 end) as m_h1_2021,
           sum(case when year=2021 then h2_months else 0 end) as m_h2_2021,
           sum(case when year=2022 then h1_months else 0 end) as m_h1_2022,
           sum(case when year=2022 then h2_months else 0 end) as m_h2_2022
    from mylib._enrollment_bins
    group by apcd_unique_id;
quit;

/* Add derived fields for cohort selection */
data mylib._enrollment_wide;
    set mylib._enrollment_wide;
    array bins {12} m_h1_2017 m_h2_2017 m_h1_2018 m_h2_2018
                    m_h1_2019 m_h2_2019 m_h1_2020 m_h2_2020
                    m_h1_2021 m_h2_2021 m_h1_2022 m_h2_2022;

    total_months_enrolled = sum(of bins[*]);
    n_bins_ge_3_of_6      = 0;
    n_bins_with_any       = 0;
    all_bins_full         = 1;
    do i = 1 to 12;
        if bins[i] >= 3 then n_bins_ge_3_of_6 + 1;
        if bins[i] >= 1 then n_bins_with_any + 1;
        if bins[i] <  6 then all_bins_full = 0;
    end;

    /* Continuous enrollment flag: all 72 months covered (every bin = 6) */
    continuous_enrolled = all_bins_full;

    drop i all_bins_full;
run;
%put NOTE: D-1 Bin-level enrollment rollup complete.;


/* ======================================================================= */
/* PART E: Join to DM cohort (keep only DM patients in the enrollment file) */
/* ======================================================================= */

/*
 dm_cohort_commercial and dm_cohort_medicare (from step3) both carry
 apcd_unique_id. A patient in both sources (cross-payer) has a single
 apcd_unique_id and we want a single enrollment row — the MEST/BEN_SUM
 union above already handles this at the apcd_unique_id level.
*/

proc sql;
    create table work.dm_study_ids as
    select distinct
        coalescec(apcd_unique_id, '') as apcd_unique_id length=90,
        max(diabetes_type) as diabetes_type length=10,
        min(first_dm_date) as first_dm_date format=mmddyy10.,
        'COMMERCIAL' as primary_source length=10
    from dm_cohort_commercial
    where apcd_unique_id is not null and apcd_unique_id <> ''
    group by apcd_unique_id

    union

    select distinct
        coalescec(apcd_unique_id, '') as apcd_unique_id length=90,
        max(diabetes_type) as diabetes_type length=10,
        min(first_dm_date) as first_dm_date format=mmddyy10.,
        'MEDICARE' as primary_source length=10
    from dm_cohort_medicare
    where apcd_unique_id is not null and apcd_unique_id <> ''
    group by apcd_unique_id;
quit;

proc sql;
    create table work.dm_enrollment as
    select d.apcd_unique_id,
           d.diabetes_type,
           d.first_dm_date,
           d.primary_source,
           e.gender,
           e.m_h1_2017, e.m_h2_2017,
           e.m_h1_2018, e.m_h2_2018,
           e.m_h1_2019, e.m_h2_2019,
           e.m_h1_2020, e.m_h2_2020,
           e.m_h1_2021, e.m_h2_2021,
           e.m_h1_2022, e.m_h2_2022,
           e.total_months_enrolled,
           e.n_bins_ge_3_of_6,
           e.n_bins_with_any,
           e.continuous_enrolled
    from work.dm_study_ids d
    left join mylib._enrollment_wide e
        on d.apcd_unique_id = e.apcd_unique_id;
quit;
%put NOTE: E-1 DM cohort joined to enrollment.;


/* ======================================================================= */
/* PART F: Export CSVs                                                      */
/* ======================================================================= */

/* Cohort 1: Continuously enrolled (all 72 months) */
data work.cohort_continuous;
    set work.dm_enrollment;
    where continuous_enrolled = 1;
run;

proc export data=work.cohort_continuous
    outfile="&outdir.\cohort_continuous.csv"
    dbms=csv replace;
run;

/* Cohort 2: Fractional (anyone with >=1 month anywhere in the window) */
data work.cohort_fractional;
    set work.dm_enrollment;
    where total_months_enrolled >= 1;
run;

proc export data=work.cohort_fractional
    outfile="&outdir.\cohort_fractional.csv"
    dbms=csv replace;
run;

%put NOTE: F-1 CSV exports complete.;


/* ======================================================================= */
/* PART G: Summary report                                                   */
/* ======================================================================= */

proc sql;
    select count(*) into :n_dm_total          from work.dm_enrollment;
    select count(*) into :n_with_enrollment   from work.dm_enrollment
           where total_months_enrolled >= 1;
    select count(*) into :n_continuous        from work.cohort_continuous;
    select count(*) into :n_fractional        from work.cohort_fractional;
    select mean(total_months_enrolled) into :mean_months
           from work.dm_enrollment where total_months_enrolled >= 1;
    select median(total_months_enrolled) into :med_months
           from work.dm_enrollment where total_months_enrolled >= 1;
quit;

filename sumrep "&outdir.\enrollment_summary.txt";
data _null_;
    file sumrep;
    put "ENROLLMENT-BASED DENOMINATOR CONSTRUCTION (step3c)";
    put "==================================================";
    put " ";
    put "Study window: &yr_start-01 through &yr_end-12 (72 months, 12 bins)";
    put "Medical payer types: COM, MCD, MCR_ADV, QHP, HCIP, EBD, PASSE, MCD_QHP";
    put "Medicare FFS: 12-month presence assumed per BEN_SUM year";
    put " ";
    put "DM patients total (from step3 cohort):        &n_dm_total";
    put "  With >=1 month of medical enrollment:       &n_with_enrollment";
    put "  With ZERO matched enrollment (dropped):     %eval(&n_dm_total - &n_with_enrollment)";
    put " ";
    put "COHORT 1 (PRIMARY, continuous 72 months):     &n_continuous";
    put "COHORT 2 (SENSITIVITY, fractional PT):        &n_fractional";
    put " ";
    put "Among enrolled patients:";
    put "  Mean months enrolled:    %sysfunc(putn(&mean_months, 6.1))";
    put "  Median months enrolled:  &med_months";
run;

%put NOTE: Step 3c complete. Summary at &outdir.\enrollment_summary.txt;


/* ======================================================================= */
/* PART H: Cleanup                                                          */
/* ======================================================================= */

proc datasets library=mylib nolist;
    delete _mest_enrollment _bensum_years _enrollment_unified
           _enrollment_by_year _enrollment_bins;
quit;
