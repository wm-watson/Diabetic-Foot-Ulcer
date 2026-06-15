/* autoexec for t005_medicaid_race_dedup
   ----------------------------------------------------------------------
   step3e_medicaid_race.sas Part C builds work.medicaid_race by joining a
   raw MEMBER race/ethnicity pull (mylib._mbr_race_raw, over ODBC) to the
   Medicaid stratum roster (work.medicaid_ids), then de-duplicates to one
   row per person preferring a non-missing race code. Here work.medicaid_race
   is created directly as WORK data with the same columns, including a
   person who has both a missing-race and a populated-race row so the
   "prefer non-missing race" dedup is exercised.
*/
options obs=100;

data work.medicaid_race;
    length apcd_unique_id $90 member_race $4 member_eth1 $4 member_eth2 $4;
    input apcd_unique_id $ member_race $ member_eth1 $ member_eth2 $
          race_eth_source_year;
    datalines;
UID001 .    H .    2021
UID001 WHT  H .    2022
UID002 BLK  NH .   2023
UID003 .    .  .   2020
UID003 ASN  NH .   2022
UID004 WHT  .  .   2019
;
run;
