/* ----------------------------------------------------------------------
   Adapted from step3b_bin_activity.sas (Part F: Summary reports).

   Four PROC SQL summaries describing bin activity by year (commercial and
   Medicare separately), by half-year, and by meteorological season, the
   latter two combining both sources with UNION ALL over the 2017-2022
   analytic window. The SQL is preserved verbatim from upstream; only the
   mylib.* libref is dropped so the tables resolve to the WORK stand-ins.
   ---------------------------------------------------------------------- */

title "Step 3b-F1: Bin Activity by Year - Commercial";
proc sql;
    select bin_year,
           count(distinct catx('|', submitter, group_policy, person_code))
               as n_active_patients format=comma12.,
           sum(had_dm)  as dm_bins  format=comma15.,
           sum(had_dfu) as dfu_bins format=comma15.
    from bin_activity_commercial
    group by bin_year
    order by bin_year;
quit;

title "Step 3b-F2: Bin Activity by Year - Medicare";
proc sql;
    select bin_year,
           count(distinct bene_id) as n_active_patients format=comma12.,
           sum(had_dm)  as dm_bins  format=comma15.,
           sum(had_dfu) as dfu_bins format=comma15.
    from bin_activity_medicare
    group by bin_year
    order by bin_year;
quit;

title "Step 3b-F3: Half-Year Bin Distribution (Combined)";
proc sql;
    select bin_year, half,
           sum(had_dm)  as dm_bins  format=comma15.,
           sum(had_dfu) as dfu_bins format=comma15.
    from (
        select bin_year, half, had_dm, had_dfu
            from bin_activity_commercial
            where bin_year between 2017 and 2022
        union all
        select bin_year, half, had_dm, had_dfu
            from bin_activity_medicare
            where bin_year between 2017 and 2022
    )
    group by bin_year, half
    order by bin_year, half;
quit;

title "Step 3b-F4: Seasonal Bin Distribution (Combined)";
proc sql;
    select season_year, season,
           sum(had_dm)  as dm_bins  format=comma15.,
           sum(had_dfu) as dfu_bins format=comma15.
    from (
        select season_year, season, had_dm, had_dfu
            from bin_activity_commercial
            where season_year between 2017 and 2022
        union all
        select season_year, season, had_dm, had_dfu
            from bin_activity_medicare
            where season_year between 2017 and 2022
    )
    group by season_year, season
    order by season_year, season;
quit;
title;

%put NOTE: step3b summary reports complete.;
