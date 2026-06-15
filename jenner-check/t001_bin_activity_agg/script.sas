/* ----------------------------------------------------------------------
   Adapted from step3b_bin_activity.sas (Part B collapse + summary).

   The original collapses any duplicate (patient x bin) rows introduced by
   per-year appends, taking max() of the activity flags and sum() of the
   claim counts, then prints a one-row study summary. The aggregation and
   summary SQL below are kept exactly as written upstream; only the
   mylib.* libref is dropped so the table resolves to the WORK stand-in
   created in autoexec.sas.
   ---------------------------------------------------------------------- */

/* Collapse any duplicate bins introduced by the append (shouldn't happen
   within a single year table but safety net if year tables overlap). */
proc sql;
    create table _bin_comm_agg as
    select submitter, group_policy, person_code,
           bin_year, half, season, season_year,
           max(had_dm)       as had_dm,
           max(had_l97)      as had_l97,
           max(had_combo)    as had_combo,
           max(had_dfu)      as had_dfu,
           sum(n_dm_claims)  as n_dm_claims,
           sum(n_dfu_claims) as n_dfu_claims
    from bin_activity_commercial
    group by submitter, group_policy, person_code,
             bin_year, half, season, season_year;
quit;

proc sql;
    create table bin_activity_commercial as
    select * from _bin_comm_agg;
quit;

%put NOTE: Part B Commercial bin activity complete.;

title "Step 3b-B: Commercial Bin Activity Summary";
proc sql;
    select count(*) as n_rows format=comma15.,
           count(distinct catx('|', submitter, group_policy, person_code))
               as n_patients format=comma12.,
           min(bin_year) as min_year,
           max(bin_year) as max_year,
           sum(had_dm)  as dm_active_bins  format=comma15.,
           sum(had_dfu) as dfu_active_bins format=comma15.
    from bin_activity_commercial;
quit;
title;
