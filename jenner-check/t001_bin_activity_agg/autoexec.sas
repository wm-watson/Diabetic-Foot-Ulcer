/* autoexec for t001_bin_activity_agg
   ----------------------------------------------------------------------
   Builds a small in-memory stand-in for the per-patient x time-bin
   activity table that step3b_bin_activity.sas extracts from the APCD
   server over ODBC. The columns and value domains match the real
   bin_activity_commercial table (one row per patient x bin tuple, with
   the meteorological half / season / season_year keys and the activity
   flags had_dm/had_l97/had_combo/had_dfu plus claim counts), so the
   author's aggregation and summary SQL runs against representative data
   without any database connection.
*/
options obs=100;

data bin_activity_commercial;
    length submitter $4 group_policy $8 person_code $4 season $1;
    input submitter $ group_policy $ person_code $
          bin_year half season $ season_year
          had_dm had_l97 had_combo had_dfu n_dm_claims n_dfu_claims;
    datalines;
A001 GP100 P01 2017 1 S 2017 1 0 0 0 3 0
A001 GP100 P01 2017 1 U 2017 1 1 0 1 4 2
A001 GP100 P01 2017 2 A 2017 1 0 0 0 2 0
A001 GP100 P01 2018 1 W 2018 1 1 1 1 5 3
A002 GP100 P02 2017 1 S 2017 1 0 0 0 1 0
A002 GP100 P02 2018 2 U 2018 1 0 0 0 2 0
A002 GP100 P02 2019 1 S 2019 1 1 0 1 3 1
A003 GP200 P01 2020 2 A 2020 1 0 0 0 4 0
A003 GP200 P01 2021 1 S 2021 1 1 1 1 6 4
A003 GP200 P01 2021 1 S 2021 1 1 0 1 2 1
A004 GP200 P03 2022 2 W 2023 1 1 1 1 3 2
A004 GP200 P03 2022 1 U 2022 1 0 0 0 1 0
;
run;
