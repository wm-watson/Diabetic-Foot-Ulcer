/* autoexec for t002_bin_summary_reports
   ----------------------------------------------------------------------
   Stands up the two collapsed activity tables that step3b_bin_activity.sas
   exports (bin_activity_commercial keyed by submitter/group_policy/
   person_code, bin_activity_medicare keyed by bene_id), each one row per
   patient x bin. The summary reports in script.sas (Parts F1-F4) read
   only these two tables, so the ODBC extraction is replaced by small
   representative WORK data spanning the 2017-2022 analytic window.
*/
options obs=100;

data bin_activity_commercial;
    length submitter $4 group_policy $8 person_code $4 season $1;
    input submitter $ group_policy $ person_code $
          bin_year half season $ season_year
          had_dm had_l97 had_combo had_dfu n_dm_claims n_dfu_claims;
    datalines;
A001 GP100 P01 2017 1 U 2017 1 1 1 1 4 2
A001 GP100 P01 2018 1 S 2018 1 0 0 0 3 0
A002 GP100 P02 2017 2 A 2017 1 0 0 0 2 0
A002 GP100 P02 2019 1 S 2019 1 1 0 1 3 1
A003 GP200 P01 2020 2 W 2021 1 1 1 1 5 3
A003 GP200 P01 2021 1 S 2021 1 1 0 1 2 1
A004 GP200 P03 2022 1 U 2022 1 0 0 0 1 0
A004 GP200 P03 2022 2 A 2022 1 1 1 1 4 2
;
run;

data bin_activity_medicare;
    length bene_id $10 season $1;
    input bene_id $ bin_year half season $ season_year
          had_dm had_l97 had_combo had_dfu n_dm_claims n_dfu_claims;
    datalines;
B100 2017 1 S 2017 1 0 0 0 6 0
B100 2018 2 A 2018 1 1 0 1 4 1
B101 2019 1 U 2019 1 1 1 1 7 4
B101 2020 1 S 2020 1 1 0 1 3 1
B102 2021 2 W 2022 1 1 1 1 5 3
B102 2022 1 U 2022 1 0 0 0 2 0
B103 2017 2 A 2017 1 1 0 1 4 2
;
run;
