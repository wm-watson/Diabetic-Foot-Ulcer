/*****************************************************************************
 Step 3e: Targeted Re-Extract of Race / Ethnicity for the Medicaid Stratum

 Purpose
 -------
 step5_zip_extract.sas pulled Medicare race (bene_race_cd / rti_race_cd)
 but never pulled the MEMBER/ELG member race & ethnicity fields for the
 non-Medicare pipeline. Medicaid DOES collect race/ethnicity (federally
 mandated for state Medicaid programs) and the AR APCD carries it in the
 MEMBER (ELG) ethnicity fields. This step recovers it for the Medicaid
 stratum so a race-stratified sensitivity can be run on the
 Delta-Interior Medicaid amputation finding (Health & Place equity
 framing).

 Scope
 -----
 - Restrict to apcd_unique_id values whose primary_payer_category =
   'MEDICAID' in payer_strata.csv (the Medicaid stratum, ~25,510 in the
   fractional cohort).
 - Pull race + ethnicity from the most recent MEMBER record per person
   (max(reckey)), mirroring the latest-record convention used in
   step5_zip_extract.sas Part A-1.

 *** CRITICAL: VERIFY FIELD CODES BEFORE RUNNING ***
 -------------------------------------------------------------------------
 The exact ME field codes for member RACE and member ETHNICITY 1 must be
 confirmed against the AR APCD claims-based / member-eligibility element
 list (the analog of "240223 MCR APCD Element List FINAL.xlsx" but for
 the commercial/Medicaid ELG layout) OR the live ACHI Confluence ELG
 dictionary.

 Known with confidence (verified earlier in this project from the
 Confluence ELG dictionary):
   - ME026 = "Member Ethnicity 2"  (varchar 2; Appendix I – Ethnicity
              code set; required as of 2022-06-30; ~50% completeness
              threshold historically)

 To be verified by the analyst before running (placeholders below):
   - &me_race  : ME code for "Member Race"        (e.g., ME024 or ME021)
   - &me_eth1  : ME code for "Member Ethnicity 1" (e.g., ME025)
   - &me_eth2  : ME026  (believed confirmed; re-confirm)

 If the element list shows different codes, change ONLY the three %let
 statements below; the rest of the script is code-agnostic.
 -------------------------------------------------------------------------

 Prerequisites
 -------------
 - payer_strata.csv (from step3d_payer_strata.sas) in D:\WPWatson
 - MEMBER table accessible via DSN=APCD-24D
 - The person-linkage chain (MEMBER.me001 + me107 -> MEST -> apcd_unique_id)
   established in SESSION_RECAP_AR_APCD.md and used in step5 Part A.

 Output (D:\WPWatson)
 --------------------
   medicaid_race.csv  : apcd_unique_id, member_race, member_eth1,
                        member_eth2, race_eth_source_year
   medicaid_race_completeness.txt : completeness report by year

 Author: William P. Watson, PhD Candidate (UAMS)
 Created: 2026-05-15
*****************************************************************************/

libname arapcd odbc
    noprompt="dsn=APCD-24D;Trusted_connection=yes"
    schema=public;
libname mylib 'D:\WPWatson';
%let outdir = D:\WPWatson;

/* ===================================================================== */
/* >>> VERIFY THESE THREE CODES AGAINST THE ELG ELEMENT LIST <<<         */
/* ===================================================================== */
%let me_race = ME024;   /* Member Race        -- VERIFY */
%let me_eth1 = ME025;   /* Member Ethnicity 1 -- VERIFY */
%let me_eth2 = ME026;   /* Member Ethnicity 2 -- believed confirmed     */
/* ===================================================================== */


/* --------------------------------------------------------------------- */
/* PART A: Load the Medicaid-stratum apcd_unique_ids                      */
/* --------------------------------------------------------------------- */
proc import
    datafile="&outdir.\payer_strata.csv"
    out=work._strata
    dbms=csv replace;
    getnames=yes;
    guessingrows=max;
run;

data work.medicaid_ids;
    set work._strata;
    where upcase(primary_payer_category) = 'MEDICAID'
          and apcd_unique_id is not null
          and apcd_unique_id ne '';
    keep apcd_unique_id;
run;

proc sql noprint;
    select count(*) into :n_mcd from work.medicaid_ids;
quit;
%put NOTE: Medicaid-stratum apcd_unique_ids loaded: &n_mcd;


/* --------------------------------------------------------------------- */
/* PART B: Pull race/ethnicity from the most recent MEMBER record        */
/*         per person.  Person linkage:                                  */
/*           MEMBER (me001 + me107)  ->  MEST (submitter + member_id)     */
/*             ->  apcd_unique_id  (the join key we filter on).          */
/*                                                                       */
/*  We do the heavy join on the database via passthrough, returning      */
/*  only Medicaid-stratum persons.                                       */
/* --------------------------------------------------------------------- */
proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._mbr_race_raw as
    select * from connection to odbc (
        select  t.apcd_unique_id,
                m.&me_race  as member_race,
                m.&me_eth1  as member_eth1,
                m.&me_eth2  as member_eth2,
                m.me014_year as race_eth_source_year
        from public.MEMBER m
        /* most recent MEMBER record per person */
        inner join (
            select me001, me006, me010, max(reckey) as max_rk
            from public.MEMBER
            group by me001, me006, me010
        ) b
          on  m.me001 = b.me001 and m.me006 = b.me006
          and m.me010 = b.me010 and m.reckey = b.max_rk
        /* MEMBER -> MEST to obtain apcd_unique_id */
        inner join public.AR_APCD_24B_MEST t
          on  m.me001 = t.submitter
          and m.me107 = t.member_id
        where t.apcd_unique_id is not null
          and t.apcd_unique_id <> ''
    );
    disconnect from odbc;
quit;
%put NOTE: B-1 raw MEMBER race/ethnicity pulled.;


/* --------------------------------------------------------------------- */
/* PART C: Restrict to the Medicaid stratum and de-duplicate             */
/* --------------------------------------------------------------------- */
proc sql;
    create table work.medicaid_race as
    select  r.apcd_unique_id,
            r.member_race  length=4,
            r.member_eth1  length=4,
            r.member_eth2  length=4,
            r.race_eth_source_year
    from mylib._mbr_race_raw r
    inner join work.medicaid_ids k
      on r.apcd_unique_id = k.apcd_unique_id;
quit;

/* One row per person (defensive; the max(reckey) join should already   */
/* yield one).  Prefer a non-missing race value if duplicates exist.    */
proc sort data=work.medicaid_race;
    by apcd_unique_id descending member_race;
run;
data work.medicaid_race;
    set work.medicaid_race;
    by apcd_unique_id;
    if first.apcd_unique_id;
run;


/* --------------------------------------------------------------------- */
/* PART D: Export + completeness report                                  */
/* --------------------------------------------------------------------- */
proc export data=work.medicaid_race
    outfile="&outdir.\medicaid_race.csv"
    dbms=csv replace;
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

filename rpt "&outdir.\medicaid_race_completeness.txt";
data _null_;
    file rpt;
    put "MEDICAID-STRATUM RACE / ETHNICITY RE-EXTRACT (step3e)";
    put "=====================================================";
    put " ";
    put "Field codes used (VERIFY against ELG element list):";
    put "  Member Race        = &me_race";
    put "  Member Ethnicity 1 = &me_eth1";
    put "  Member Ethnicity 2 = &me_eth2";
    put " ";
    put "Medicaid-stratum persons:        &n_tot";
    put "  with non-missing race:         &n_race";
    put "  with non-missing ethnicity 1:  &n_e1";
    put "  with non-missing ethnicity 2:  &n_e2";
    put " ";
    put "NOTE: pre-2022 ethnicity completeness is expected to be lower";
    put "(field historically optional; required only as of 2022-06-30).";
    put "Check completeness by race_eth_source_year before using in a";
    put "race-stratified analysis; restrict to >=2022 if sparse earlier.";
run;

%put NOTE: Step 3e complete. Outputs: medicaid_race.csv (&n_tot rows);
%put NOTE: Completeness report: &outdir.\medicaid_race_completeness.txt;

/* Cleanup */
proc datasets library=mylib nolist;
    delete _mbr_race_raw;
quit;
