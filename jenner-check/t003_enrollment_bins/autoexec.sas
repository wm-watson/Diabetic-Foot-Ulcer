/* autoexec for t003_enrollment_bins
   ----------------------------------------------------------------------
   step3c_enrollment.sas builds _enrollment_by_year from APCD MEST and
   BEN_SUM enrollment tables over ODBC: one row per apcd_unique_id x year
   carrying 12 character monthly-coverage flags ('0'/'1') m01-m12 and a
   harmonized gender. The bin rollup and cohort-flag DATA steps that
   follow read only that table, so it is reproduced here as small WORK
   data with realistic partial- and full-coverage patterns.
*/
options obs=100;

data _enrollment_by_year;
    length apcd_unique_id $90 gender $1
           m01-m12 $1;
    input apcd_unique_id $ gender $ year
          m01 $ m02 $ m03 $ m04 $ m05 $ m06 $
          m07 $ m08 $ m09 $ m10 $ m11 $ m12 $;
    datalines;
UID001 M 2017 1 1 1 1 1 1 1 1 1 1 1 1
UID001 M 2018 1 1 1 1 1 1 1 1 1 1 1 1
UID001 M 2019 1 1 1 1 1 1 1 1 1 1 1 1
UID001 M 2020 1 1 1 1 1 1 1 1 1 1 1 1
UID001 M 2021 1 1 1 1 1 1 1 1 1 1 1 1
UID001 M 2022 1 1 1 1 1 1 1 1 1 1 1 1
UID002 F 2017 0 0 0 1 1 1 1 1 1 0 0 0
UID002 F 2018 1 1 1 1 1 1 0 0 0 0 0 0
UID002 F 2019 1 1 1 1 1 1 1 1 1 1 1 1
UID003 F 2021 0 0 1 1 1 1 1 1 1 1 1 1
UID003 F 2022 1 1 1 1 1 1 1 1 0 0 0 0
;
run;
