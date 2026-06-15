/* autoexec for t004_medicare_collapse
   ----------------------------------------------------------------------
   step3b_medicare_rerun.sas appends per-patient x bin rows from seven
   Medicare claim tables into one accumulator (_bin_mcr_raw), then
   collapses across tables (a patient can appear in multiple tables in the
   same bin) and inner-joins to the DM cohort. Here _bin_mcr_raw is built
   directly as WORK data with the same key (bene_id x bin) and a couple of
   intentional within-bin duplicates so the max()/sum() collapse is
   exercised; dm_cohort_medicare is a small roster so the inner join drops
   a non-cohort patient.
*/
options compress=yes nofmterr;
options obs=100;

data _bin_mcr_raw;
    length bene_id $10 season $1;
    input bene_id $ bin_year half season $ season_year
          had_dm had_l97 had_combo had_dfu n_dm_claims n_dfu_claims;
    datalines;
B100 2017 1 S 2017 1 0 0 0 3 0
B100 2017 1 S 2017 0 1 1 1 2 1
B100 2018 2 A 2018 1 0 0 0 4 0
B101 2019 1 U 2019 1 1 0 1 5 2
B101 2019 1 U 2019 1 0 1 1 3 1
B102 2020 2 W 2021 1 1 1 1 6 3
B999 2017 1 S 2017 1 0 0 0 2 0
;
run;

data dm_cohort_medicare;
    length bene_id $10;
    input bene_id $;
    datalines;
B100
B101
B102
;
run;
