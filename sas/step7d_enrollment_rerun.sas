/*****************************************************************************
 Step 7d: Enrollment-build rerun (space-safe)

 Purpose
 -------
 The initial step7 run (2026-08-06) exhausted D:\WPWatson mid-passthrough
 during the all-population MEST enrollment build. The output all_enrollment
 came back with 0 rows and a mangled schema (apcd_unique_id + gender missing).
 The PCP-visit extract (Parts A/B/C) and the ZIP lookup (Part E) succeeded
 and are still on disk -- no need to redo them.

 This script reruns ONLY the fixed Part D (per-year MEST passthrough with
 immediate binning + inline cleanup + row-count checkpoints) and exports
 the corrected all_enrollment.csv. Server work is bounded by one year of
 MEST at a time, so D:\WPWatson never holds more than ~1M rows worth of
 monthly enrollment strings.

 Runtime estimate: 20-40 min on the APCD server (one MEST passthrough per
 year, small BEN_SUM, then two aggregate SQL steps and a data step).

 Prerequisites
 -------------
   D:\WPWatson\step7_summary.txt      (from initial step7 run, will be updated)
   D:\WPWatson\pcp_visits_bin.csv     (from initial step7 run, unchanged)
   D:\WPWatson\all_zip_lookup.csv     (from initial step7 run, unchanged)

 Output
 ------
   D:\WPWatson\all_enrollment.csv     (rewritten, with apcd_unique_id + gender)
   D:\WPWatson\step7d_summary.txt     (rerun row counts)

 Author: William P. Watson (UAMS)
 Created: 2026-08-06 (rerun after space exhaustion)
*****************************************************************************/

libname arapcd odbc
    noprompt="dsn=APCD-24D;Trusted_connection=yes"
    schema=public;

libname mylib 'D:\WPWatson';
%let outdir = D:\WPWatson;

%let q = %str(%');

%let yr_start  = 2017;
%let yr_end    = 2022;
%let medical_payers = %str('COM','MCD','MCR_ADV','QHP','HCIP','EBD','PASSE','MCD_QHP');


/* ======================================================================= */
/* D-1: Per-year MEST passthrough + immediate SAS-side binning.             */
/* ======================================================================= */

%macro get_mest_year(yr, first=0);
    proc sql;
        connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
        create table _mest_yr as
        select * from connection to odbc (
            select apcd_unique_id,
                   gender,
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
              and year = &q.&yr.&q.
              and apcd_unique_id is not null
              and apcd_unique_id <> ''
            group by apcd_unique_id, gender
        );
        disconnect from odbc;
    quit;

    data _mest_yr_binned;
        length apcd_unique_id $90 gender $1 source $4;
        set _mest_yr;
        array mchar {12} $1 m01-m12;
        array mnum  {12}    n01-n12;
        do i = 1 to 12;
            mnum[i] = input(mchar[i], 1.);
            if missing(mnum[i]) then mnum[i] = 0;
        end;
        year      = &yr.;
        h1_months = sum(of n01-n06);
        h2_months = sum(of n07-n12);
        source    = 'MEST';
        keep apcd_unique_id gender year h1_months h2_months source;
    run;

    %if &first = 1 %then %do;
        data mylib.enroll_year_bins; set _mest_yr_binned; run;
    %end;
    %else %do;
        proc append base=mylib.enroll_year_bins data=_mest_yr_binned force; run;
    %end;

    proc sql;
        select count(*) into :_n_yr from _mest_yr_binned;
    quit;
    %put NOTE: D-1 MEST &yr. binned rows appended: &_n_yr.;

    proc datasets lib=work nolist; delete _mest_yr _mest_yr_binned; quit;
%mend;

%get_mest_year(2017, first=1);
%get_mest_year(2018);
%get_mest_year(2019);
%get_mest_year(2020);
%get_mest_year(2021);
%get_mest_year(2022);

proc sql;
    select count(*) into :n_mest_bins from mylib.enroll_year_bins;
quit;
%put NOTE: D-1 MEST total binned person-years: &n_mest_bins.;


/* ======================================================================= */
/* D-2: BEN_SUM Medicare FFS presence.                                       */
/* ======================================================================= */

proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table _bensum_years as
    select * from connection to odbc (
        select apcd_unique_id,
               sex_ident_cd,
               bene_enrollmt_ref_yr as year
        from public.APCD_MCR_BEN_SUM
        where bene_enrollmt_ref_yr between &yr_start and &yr_end
          and apcd_unique_id is not null
          and apcd_unique_id <> ''
        group by apcd_unique_id, sex_ident_cd, bene_enrollmt_ref_yr
    );
    disconnect from odbc;
quit;

data _bensum_binned;
    length apcd_unique_id $90 gender $1 source $4;
    set _bensum_years;
    if      sex_ident_cd = '1' then gender = 'M';
    else if sex_ident_cd = '2' then gender = 'F';
    else                            gender = 'U';
    h1_months = 6;
    h2_months = 6;
    source    = 'FFS';
    keep apcd_unique_id gender year h1_months h2_months source;
run;

proc append base=mylib.enroll_year_bins data=_bensum_binned force; run;

proc sql;
    select count(*) into :n_bensum from _bensum_binned;
    select count(*) into :n_after_ffs from mylib.enroll_year_bins;
quit;
%put NOTE: D-2 BEN_SUM binned rows appended: &n_bensum. (total now &n_after_ffs.);

proc datasets lib=work nolist; delete _bensum_years _bensum_binned; quit;


/* ======================================================================= */
/* D-3: Collapse to one row per (apcd_unique_id, year).                      */
/* ======================================================================= */

proc sql;
    create table mylib.enroll_person_year as
    select apcd_unique_id,
           year,
           case when max(gender) = 'U' then min(gender) else max(gender) end
               as gender length=1,
           max(h1_months) as h1_months,
           max(h2_months) as h2_months
    from mylib.enroll_year_bins
    where apcd_unique_id is not null and apcd_unique_id <> ''
    group by apcd_unique_id, year;
quit;

proc sql;
    select count(*) into :n_person_year from mylib.enroll_person_year;
    select count(distinct apcd_unique_id) into :n_person
      from mylib.enroll_person_year;
quit;
%put NOTE: D-3 person-year rows: &n_person_year. distinct persons: &n_person.;

proc datasets lib=mylib nolist; delete enroll_year_bins; quit;


/* ======================================================================= */
/* D-4: Wide pivot -- one row per apcd_unique_id, 12 half-year bin cols.    */
/* ======================================================================= */

proc sql;
    create table mylib.all_enrollment_raw as
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
    from mylib.enroll_person_year
    group by apcd_unique_id;
quit;

proc sql;
    select count(*) into :n_raw from mylib.all_enrollment_raw;
quit;
%put NOTE: D-4 wide pivot rows: &n_raw.;

proc datasets lib=mylib nolist; delete enroll_person_year; quit;


/* ======================================================================= */
/* D-5: Derived fields + final table.                                        */
/* ======================================================================= */

data mylib.all_enrollment;
    retain apcd_unique_id gender
           m_h1_2017 m_h2_2017 m_h1_2018 m_h2_2018
           m_h1_2019 m_h2_2019 m_h1_2020 m_h2_2020
           m_h1_2021 m_h2_2021 m_h1_2022 m_h2_2022;
    set mylib.all_enrollment_raw;
    array bins {12} m_h1_2017 m_h2_2017 m_h1_2018 m_h2_2018
                    m_h1_2019 m_h2_2019 m_h1_2020 m_h2_2020
                    m_h1_2021 m_h2_2021 m_h1_2022 m_h2_2022;
    total_months_enrolled = sum(of bins[*]);
    n_bins_ge_3_of_6 = 0;
    n_bins_with_any  = 0;
    _all_bins_full   = 1;
    do i = 1 to 12;
        if bins[i] >= 3 then n_bins_ge_3_of_6 + 1;
        if bins[i] >= 1 then n_bins_with_any  + 1;
        if bins[i] <  6 then _all_bins_full = 0;
    end;
    continuous_enrolled = _all_bins_full;
    drop i _all_bins_full;
run;

proc sql;
    select count(*) into :n_final from mylib.all_enrollment;
    select count(*) into :n_cont  from mylib.all_enrollment
        where continuous_enrolled = 1;
quit;
%put NOTE: D-5 all_enrollment rows: &n_final. continuous: &n_cont.;

proc datasets lib=mylib nolist; delete all_enrollment_raw; quit;


/* ======================================================================= */
/* Export corrected CSV + rerun summary                                     */
/* ======================================================================= */

proc export data=mylib.all_enrollment
    outfile="&outdir.\all_enrollment.csv"
    dbms=csv replace;
run;

filename sumrep "&outdir.\step7d_summary.txt";
data _null_;
    file sumrep;
    put "STEP 7d: ENROLLMENT-BUILD RERUN (space-safe)";
    put "=============================================";
    put " ";
    put "MEST binned person-years (2017-2022):  &n_mest_bins.";
    put "BEN_SUM FFS person-years appended:     &n_bensum.";
    put "After FFS union:                       &n_after_ffs.";
    put "Person-year (MEST or FFS OR):          &n_person_year.";
    put "Distinct persons:                      &n_person.";
    put "Wide-pivot rows:                       &n_raw.";
    put "all_enrollment rows:                   &n_final.";
    put "  of which continuous_enrolled=1:      &n_cont.";
    put " ";
    put "Output: &outdir.\all_enrollment.csv";
run;

%put NOTE: Step 7d complete. Summary at &outdir.\step7d_summary.txt;
