/* ----------------------------------------------------------------------
   Adapted from step3c_enrollment.sas (Part D: Half-year bin rollup).

   Converts the 12 character monthly-coverage flags to numeric, sums them
   into half-year months-enrolled (h1/h2), pivots to one row per person
   with 12 bin-month columns (H1/H2 x 6 years), then derives the cohort-
   selection flags (total months, bins >= 3 of 6, bins with any coverage,
   and the all-bins-full continuous-enrollment flag). DATA-step and SQL
   logic preserved verbatim; only the mylib.* libref is dropped so the
   tables resolve to WORK.
   ---------------------------------------------------------------------- */

data _enrollment_bins;
    set _enrollment_by_year;

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
    create table _enrollment_wide as
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
    from _enrollment_bins
    group by apcd_unique_id;
quit;

/* Add derived fields for cohort selection */
data _enrollment_wide;
    set _enrollment_wide;
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

title "Step 3c-D: Enrollment Bin Rollup";
proc print data=_enrollment_wide noobs;
    var apcd_unique_id gender total_months_enrolled
        n_bins_ge_3_of_6 n_bins_with_any continuous_enrolled;
run;
title;
