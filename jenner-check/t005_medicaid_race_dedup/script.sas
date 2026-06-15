/* ----------------------------------------------------------------------
   Adapted from step3e_medicaid_race.sas (Part C dedup + Part D report).

   De-duplicates the Medicaid-stratum race table to one row per person,
   preferring a non-missing race value (PROC SORT by descending race, then
   FIRST. selection), then writes the completeness report counting
   non-missing race/ethnicity via PROC SQL INTO: macro variables and a
   DATA _NULL_ PUT block. Logic preserved verbatim from upstream; the
   ODBC-sourced inputs are replaced by the WORK stand-in built in
   autoexec.sas and the hardcoded report path is dropped (PUT to log).
   ---------------------------------------------------------------------- */

/* One row per person (defensive). Prefer a non-missing race value if
   duplicates exist. */
proc sort data=work.medicaid_race;
    by apcd_unique_id descending member_race;
run;
data work.medicaid_race;
    set work.medicaid_race;
    by apcd_unique_id;
    if first.apcd_unique_id;
run;

proc sql;
    select count(*)                                            into :n_tot
        from work.medicaid_race;
    select sum(member_race is not null and member_race ne '')  into :n_race
        from work.medicaid_race;
    select sum(member_eth1 is not null and member_eth1 ne '')  into :n_e1
        from work.medicaid_race;
    select sum(member_eth2 is not null and member_eth2 ne '')  into :n_e2
        from work.medicaid_race;
quit;

data _null_;
    put "MEDICAID-STRATUM RACE / ETHNICITY RE-EXTRACT (step3e)";
    put "=====================================================";
    put " ";
    put "Medicaid-stratum persons:        &n_tot";
    put "  with non-missing race:         &n_race";
    put "  with non-missing ethnicity 1:  &n_e1";
    put "  with non-missing ethnicity 2:  &n_e2";
run;

title "Step 3e-C: Deduplicated Medicaid Race Stratum";
proc print data=work.medicaid_race noobs;
    var apcd_unique_id member_race member_eth1 member_eth2 race_eth_source_year;
run;
title;

%put NOTE: Step 3e dedup + completeness complete.;
