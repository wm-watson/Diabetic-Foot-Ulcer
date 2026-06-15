/* ----------------------------------------------------------------------
   Adapted from step3b_medicare_rerun.sas (collapse + cohort join + summary).

   Collapses the seven-table accumulator across (bene_id x bin), taking
   max() of activity flags and sum() of claim counts, then inner-joins to
   the DM cohort as a safety net and prints the study summary. SQL is
   preserved verbatim from upstream; only the mylib.* librefs are dropped
   so the tables resolve to WORK.
   ---------------------------------------------------------------------- */

/* Collapse across the 7 tables */
proc sql;
    create table bin_activity_medicare as
    select bene_id,
           bin_year, half, season, season_year,
           max(had_dm)       as had_dm,
           max(had_l97)      as had_l97,
           max(had_combo)    as had_combo,
           max(had_dfu)      as had_dfu,
           sum(n_dm_claims)  as n_dm_claims,
           sum(n_dfu_claims) as n_dfu_claims
    from _bin_mcr_raw
    group by bene_id, bin_year, half, season, season_year;
quit;

/* Inner-join to DM cohort for safety */
proc sql;
    create table _bin_mcr_final as
    select a.*
    from bin_activity_medicare a
    inner join dm_cohort_medicare b
        on a.bene_id = b.bene_id;
quit;
proc sql;
    create table bin_activity_medicare as
    select * from _bin_mcr_final;
quit;

/* Summary */
title "Medicare Bin Activity Summary";
proc sql;
    select count(*) as n_rows format=comma15.,
           count(distinct bene_id) as n_patients format=comma12.,
           min(bin_year) as min_year,
           max(bin_year) as max_year,
           sum(had_dm)  as dm_active_bins  format=comma15.,
           sum(had_dfu) as dfu_active_bins format=comma15.
    from bin_activity_medicare;
quit;
title;

%put NOTE: Medicare bin activity re-run complete.;
