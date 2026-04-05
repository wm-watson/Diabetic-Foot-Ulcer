/*****************************************************************************
 Step 3b: Patient × Time-Bin Activity Extraction

 Extracts per-patient activity flags for half-year and meteorological-season
 bins. Used as input to ZCTA × time-bin aggregation in R for Emerging Hot
 Spot Analysis (EHSA).

 Why this script exists:
   step3_cohort.sas produces patient-level first/last DM and DFU dates but
   NOT per-bin activity. Using first-to-last as a proxy for bin membership
   smears intermittent DFU activity across every bin in the window, which
   dampens real temporal signal that EHSA needs to detect.

 Output tables (in D:\WPWatson/mylib, one row per patient × bin):
   bin_activity_commercial : 2017-2024, commercial claims
   bin_activity_medicare   : 2014-2022, Medicare claims (7 tables)

 Bin structure:
   Each row represents a unique (year, half, season, season_year) tuple
   for a given patient in a given data source. A calendar year has up to
   6 such tuples (Jan-Feb, Mar-May, Jun, Jul-Aug, Sep-Nov, Dec) because
   December belongs to the NEXT meteorological winter.

 Bin conventions:
   half          = 1 if month in Jan-Jun, else 2
   season        = W (Dec-Feb), S (Mar-May), U (Jun-Aug), A (Sep-Nov)
   season_year   = year (calendar), except December: season_year = year+1
                   so Dec 2017 + Jan-Feb 2018 = "Winter 2018" (standard
                   meteorological labeling)

 R-side usage:
   For H1/H2 analysis:        GROUP BY (data_source, patient_id, year, half)
   For seasonal analysis:     GROUP BY (data_source, patient_id, season_year, season)
   For annual analysis:       GROUP BY (data_source, patient_id, year)

 Activity flags (per bin):
   had_dm      : any E10.x or E11.x diagnosis in this bin
   had_l97     : any L97.x diagnosis in this bin
   had_combo   : any DM-ulcer combo code (E10.621/622, E11.621/622) in this bin
   had_dfu     : had_l97 OR had_combo
   n_dm_claims : count of claims with any DM diagnosis in this bin
   n_dfu_claims: count of claims with any DFU diagnosis in this bin

 Prerequisites:
   - step3_cohort.sas must have run first (we reference dm_cohort_* datasets
     to restrict the bin extraction to already-identified DM patients,
     mirroring the "DM first, then procedures" logic).
   - Libname mylib pointing to D:\WPWatson
   - ODBC DSN "APCD-24D" configured

 Notes on scale:
   Commercial: ~100K DM patients × 8 years × ~6 bins/year = ~4.8M rows max
   Medicare:   ~265K DM patients × 9 years × ~6 bins/year = ~14M rows max
   Both are feasible through ODBC with per-table appending.
*****************************************************************************/

libname mylib 'D:\WPWatson';

%let q = %str(%');

/* ======================================================================= */
/* PART M: Helper macros (duplicated from step3_cohort.sas for self-        */
/*         containment; edit in both files if the DX field lists change).  */
/* ======================================================================= */

%macro comm_like(prefix);
    upper(mc039) like &q.&prefix.%&q.
    or upper(mc041) like &q.&prefix.%&q.
    %do i = 42 %to 53;
        or upper(mc0&i.) like &q.&prefix.%&q.
    %end;
%mend;

%macro comm_in_combo;
    upper(mc039) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.)
    or upper(mc041) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.)
    %do i = 42 %to 53;
        or upper(mc0&i.) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.)
    %end;
%mend;

%macro mcr_like(prefix, max_dx, has_admtg);
    upper(prncpl_dgns_cd) like &q.&prefix.%&q.
    %do i = 1 %to &max_dx;
        or upper(icd_dgns_cd&i.) like &q.&prefix.%&q.
    %end;
    %if &has_admtg = 1 %then %do;
        or upper(admtg_dgns_cd) like &q.&prefix.%&q.
    %end;
%mend;

%macro mcr_in_combo(max_dx, has_admtg);
    upper(prncpl_dgns_cd) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.)
    %do i = 1 %to &max_dx;
        or upper(icd_dgns_cd&i.) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.)
    %end;
    %if &has_admtg = 1 %then %do;
        or upper(admtg_dgns_cd) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.)
    %end;
%mend;

/* ======================================================================= */
/* PART B: Commercial bin activity                                          */
/*                                                                          */
/* Per year table CLAIM_SVC_DT_yyyy, GROUP BY                               */
/*   (submitter, group_policy, person_code, year, half, season, season_year)*/
/* Returns one row per patient per unique bin tuple.                        */
/* ======================================================================= */

%macro get_comm_bin(yr, first=0);
    proc sql;
        connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
        create table _comm_bin_yr as
        select * from connection to odbc (
            select mc001 as submitter,
                   mc006 as group_policy,
                   mc009 as person_code,
                   extract(year from mc017) as bin_year,
                   case when extract(month from mc017) between 1 and 6
                        then 1 else 2 end as half,
                   case
                       when extract(month from mc017) in (12,1,2) then &q.W&q.
                       when extract(month from mc017) in (3,4,5)  then &q.S&q.
                       when extract(month from mc017) in (6,7,8)  then &q.U&q.
                       else &q.A&q.
                   end as season,
                   case when extract(month from mc017) = 12
                        then extract(year from mc017) + 1
                        else extract(year from mc017)
                   end as season_year,
                   /* DM activity */
                   max(case when (%comm_like(E10)) or (%comm_like(E11))
                            then 1 else 0 end) as had_dm,
                   /* DFU activity */
                   max(case when (%comm_like(L97)) then 1 else 0 end) as had_l97,
                   max(case when (%comm_in_combo)  then 1 else 0 end) as had_combo,
                   max(case when (%comm_like(L97)) or (%comm_in_combo)
                            then 1 else 0 end) as had_dfu,
                   /* Claim counts */
                   sum(case when (%comm_like(E10)) or (%comm_like(E11))
                            then 1 else 0 end) as n_dm_claims,
                   sum(case when (%comm_like(L97)) or (%comm_in_combo)
                            then 1 else 0 end) as n_dfu_claims
            from public.CLAIM_SVC_DT_&yr.
            where (%comm_like(E10)) or (%comm_like(E11))
               or (%comm_like(L97)) or (%comm_in_combo)
            group by mc001, mc006, mc009,
                     extract(year from mc017),
                     case when extract(month from mc017) between 1 and 6
                          then 1 else 2 end,
                     case
                         when extract(month from mc017) in (12,1,2) then &q.W&q.
                         when extract(month from mc017) in (3,4,5)  then &q.S&q.
                         when extract(month from mc017) in (6,7,8)  then &q.U&q.
                         else &q.A&q.
                     end,
                     case when extract(month from mc017) = 12
                          then extract(year from mc017) + 1
                          else extract(year from mc017)
                     end
        );
        disconnect from odbc;
    quit;

    %if &first = 1 %then %do;
        data mylib.bin_activity_commercial; set _comm_bin_yr; run;
    %end;
    %else %do;
        proc append base=mylib.bin_activity_commercial
                    data=_comm_bin_yr force; run;
    %end;
    proc datasets lib=work nolist; delete _comm_bin_yr; quit;
    %put NOTE: Commercial bin activity &yr. complete.;
%mend;

%get_comm_bin(2017, first=1);
%get_comm_bin(2018);
%get_comm_bin(2019);
%get_comm_bin(2020);
%get_comm_bin(2021);
%get_comm_bin(2022);
%get_comm_bin(2023);
%get_comm_bin(2024);

/* Collapse any duplicate bins introduced by the append (shouldn't happen   */
/* within a single year table but safety net if year tables overlap).      */
proc sql;
    create table mylib._bin_comm_agg as
    select submitter, group_policy, person_code,
           bin_year, half, season, season_year,
           max(had_dm)       as had_dm,
           max(had_l97)      as had_l97,
           max(had_combo)    as had_combo,
           max(had_dfu)      as had_dfu,
           sum(n_dm_claims)  as n_dm_claims,
           sum(n_dfu_claims) as n_dfu_claims
    from mylib.bin_activity_commercial
    group by submitter, group_policy, person_code,
             bin_year, half, season, season_year;
quit;
proc sql; drop table mylib.bin_activity_commercial; quit;
proc sql;
    create table mylib.bin_activity_commercial as
    select * from mylib._bin_comm_agg;
quit;
proc datasets lib=mylib nolist; delete _bin_comm_agg; quit;

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
    from mylib.bin_activity_commercial;
quit;

/* ======================================================================= */
/* PART C: Medicare bin activity                                            */
/*                                                                          */
/* One macro call per claim table (7 tables total). Appends to a single     */
/* accumulator then aggregates because the same patient may appear in       */
/* multiple tables in the same bin.                                         */
/* ======================================================================= */

%macro get_mcr_bin(name, tbl, max_dx, has_admtg, first=0);
    proc sql;
        connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
        create table _mcr_bin_tbl as
        select * from connection to odbc (
            select bene_id,
                   extract(year from clm_from_dt) as bin_year,
                   case when extract(month from clm_from_dt) between 1 and 6
                        then 1 else 2 end as half,
                   case
                       when extract(month from clm_from_dt) in (12,1,2) then &q.W&q.
                       when extract(month from clm_from_dt) in (3,4,5)  then &q.S&q.
                       when extract(month from clm_from_dt) in (6,7,8)  then &q.U&q.
                       else &q.A&q.
                   end as season,
                   case when extract(month from clm_from_dt) = 12
                        then extract(year from clm_from_dt) + 1
                        else extract(year from clm_from_dt)
                   end as season_year,
                   max(case when (%mcr_like(E10, &max_dx., &has_admtg.))
                              or (%mcr_like(E11, &max_dx., &has_admtg.))
                            then 1 else 0 end) as had_dm,
                   max(case when (%mcr_like(L97, &max_dx., &has_admtg.))
                            then 1 else 0 end) as had_l97,
                   max(case when (%mcr_in_combo(&max_dx., &has_admtg.))
                            then 1 else 0 end) as had_combo,
                   max(case when (%mcr_like(L97, &max_dx., &has_admtg.))
                              or (%mcr_in_combo(&max_dx., &has_admtg.))
                            then 1 else 0 end) as had_dfu,
                   sum(case when (%mcr_like(E10, &max_dx., &has_admtg.))
                              or (%mcr_like(E11, &max_dx., &has_admtg.))
                            then 1 else 0 end) as n_dm_claims,
                   sum(case when (%mcr_like(L97, &max_dx., &has_admtg.))
                              or (%mcr_in_combo(&max_dx., &has_admtg.))
                            then 1 else 0 end) as n_dfu_claims
            from public.APCD_MCR_&tbl._CLM
            where ((%mcr_like(E10, &max_dx., &has_admtg.))
                or (%mcr_like(E11, &max_dx., &has_admtg.))
                or (%mcr_like(L97, &max_dx., &has_admtg.))
                or (%mcr_in_combo(&max_dx., &has_admtg.)))
              and clm_from_dt < &q.2025-01-01&q.
            group by bene_id,
                     extract(year from clm_from_dt),
                     case when extract(month from clm_from_dt) between 1 and 6
                          then 1 else 2 end,
                     case
                         when extract(month from clm_from_dt) in (12,1,2) then &q.W&q.
                         when extract(month from clm_from_dt) in (3,4,5)  then &q.S&q.
                         when extract(month from clm_from_dt) in (6,7,8)  then &q.U&q.
                         else &q.A&q.
                     end,
                     case when extract(month from clm_from_dt) = 12
                          then extract(year from clm_from_dt) + 1
                          else extract(year from clm_from_dt)
                     end
        );
        disconnect from odbc;
    quit;

    %if &first = 1 %then %do;
        data mylib._bin_mcr_raw; set _mcr_bin_tbl; run;
    %end;
    %else %do;
        proc append base=mylib._bin_mcr_raw data=_mcr_bin_tbl force; run;
    %end;
    proc datasets lib=work nolist; delete _mcr_bin_tbl; quit;
    %put NOTE: Medicare bin activity &name. (&tbl.) complete.;
%mend;

%get_mcr_bin(prtb, PRTB_CAR, 12, 0, first=1);
%get_mcr_bin(out,  OUT,      25, 0);
%get_mcr_bin(inp,  INP,      25, 1);
%get_mcr_bin(snf,  SNF,      25, 1);
%get_mcr_bin(hha,  HHA,      25, 0);
%get_mcr_bin(hsp,  HSP,      25, 0);
%get_mcr_bin(dme,  DME,      12, 0);

/* Collapse across the 7 tables: a patient may appear in multiple tables    */
/* within the same bin (e.g., an outpatient visit AND a carrier line on     */
/* the same day). Take MAX of flags, SUM of claim counts.                   */
proc sql;
    create table mylib.bin_activity_medicare as
    select bene_id,
           bin_year, half, season, season_year,
           max(had_dm)       as had_dm,
           max(had_l97)      as had_l97,
           max(had_combo)    as had_combo,
           max(had_dfu)      as had_dfu,
           sum(n_dm_claims)  as n_dm_claims,
           sum(n_dfu_claims) as n_dfu_claims
    from mylib._bin_mcr_raw
    group by bene_id, bin_year, half, season, season_year;
quit;
proc datasets lib=mylib nolist; delete _bin_mcr_raw; quit;

%put NOTE: Part C Medicare bin activity complete.;

title "Step 3b-C: Medicare Bin Activity Summary";
proc sql;
    select count(*) as n_rows format=comma15.,
           count(distinct bene_id) as n_patients format=comma12.,
           min(bin_year) as min_year,
           max(bin_year) as max_year,
           sum(had_dm)  as dm_active_bins  format=comma15.,
           sum(had_dfu) as dfu_active_bins format=comma15.
    from mylib.bin_activity_medicare;
quit;

/* ======================================================================= */
/* PART D: Restrict to cohort patients only                                 */
/*   Inner-join the bin activity tables to the DM cohorts from step3.      */
/*   This drops bins for patients who somehow landed in bin extraction      */
/*   but aren't in the DM cohort (shouldn't happen given WHERE filters,     */
/*   but safety net).                                                       */
/* ======================================================================= */

proc sql;
    create table mylib._bin_comm_final as
    select a.*
    from mylib.bin_activity_commercial a
    inner join dm_cohort_commercial b
        on a.submitter = b.submitter
       and a.group_policy = b.group_policy
       and a.person_code = b.person_code;
quit;
proc sql; drop table mylib.bin_activity_commercial; quit;
proc sql;
    create table mylib.bin_activity_commercial as
    select * from mylib._bin_comm_final;
quit;
proc datasets lib=mylib nolist; delete _bin_comm_final; quit;

proc sql;
    create table mylib._bin_mcr_final as
    select a.*
    from mylib.bin_activity_medicare a
    inner join dm_cohort_medicare b
        on a.bene_id = b.bene_id;
quit;
proc sql; drop table mylib.bin_activity_medicare; quit;
proc sql;
    create table mylib.bin_activity_medicare as
    select * from mylib._bin_mcr_final;
quit;
proc datasets lib=mylib nolist; delete _bin_mcr_final; quit;

/* ======================================================================= */
/* PART E: Export CSVs for R                                                */
/* ======================================================================= */

%let outdir = D:\WPWatson;

proc export data=mylib.bin_activity_commercial
    outfile="&outdir.\bin_activity_commercial.csv"
    dbms=csv replace;
run;

proc export data=mylib.bin_activity_medicare
    outfile="&outdir.\bin_activity_medicare.csv"
    dbms=csv replace;
run;

/* ======================================================================= */
/* PART F: Summary reports                                                  */
/* ======================================================================= */

title "Step 3b-F1: Bin Activity by Year - Commercial";
proc sql;
    select bin_year,
           count(distinct catx('|', submitter, group_policy, person_code))
               as n_active_patients format=comma12.,
           sum(had_dm)  as dm_bins  format=comma15.,
           sum(had_dfu) as dfu_bins format=comma15.
    from mylib.bin_activity_commercial
    group by bin_year
    order by bin_year;
quit;

title "Step 3b-F2: Bin Activity by Year - Medicare";
proc sql;
    select bin_year,
           count(distinct bene_id) as n_active_patients format=comma12.,
           sum(had_dm)  as dm_bins  format=comma15.,
           sum(had_dfu) as dfu_bins format=comma15.
    from mylib.bin_activity_medicare
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
            from mylib.bin_activity_commercial
            where bin_year between 2017 and 2022
        union all
        select bin_year, half, had_dm, had_dfu
            from mylib.bin_activity_medicare
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
            from mylib.bin_activity_commercial
            where season_year between 2017 and 2022
        union all
        select season_year, season, had_dm, had_dfu
            from mylib.bin_activity_medicare
            where season_year between 2017 and 2022
    )
    group by season_year, season
    order by season_year, season;
quit;

title;

%put NOTE: step3b_bin_activity.sas complete.;
%put NOTE: Output CSVs: bin_activity_commercial.csv, bin_activity_medicare.csv;
%put NOTE: Primary analytic window (Option C): 2017-2022 combined.;
%put NOTE: Sensitivity windows available: commercial 2017-2024, Medicare 2014-2022.;

/*****************************************************************************
 NOTES:

 1. BIN GRANULARITY: Output rows are at the finest cross-granularity of
    H1/H2 and meteorological season, which gives up to 6 rows per patient
    per calendar year. R aggregates these to H1/H2 (12 bins over 2017-2022)
    or seasonal (24 bins over 2017-2022) as needed for each analysis.

 2. METEOROLOGICAL SEASONS: season_year reflects the labeling convention
    where December belongs to the NEXT winter (e.g., Dec 2017 + Jan-Feb 2018
    = "Winter 2018"). R analysis groups by (season_year, season).

 3. DM FIRST, THEN DFU: Part D restricts bin activity to patients in the
    dm_cohort_* datasets from step3. This mirrors the "require DM, then look
    for procedures" logic applied in step3 Parts P1-P3/T.

 4. DENOMINATOR NOTE: "Active DM in bin" means the patient had at least one
    DM claim in that bin. For patients with intermittent DM coding (a real
    phenomenon in claims data), this is more accurate than assuming
    continuous activity from first_dm_year to last_dm_year. The R pipeline
    will use had_dm = 1 as the denominator criterion per ZCTA-bin.

 5. NUMERATOR NOTE: "Active DFU in bin" = had_l97 OR had_combo. Tier
    assignment (Tier 2 temporal) is patient-level and joined from
    step3's tier2_temporal flag in the R pipeline. A patient classified as
    Tier 2 contributes to the DFU numerator in every bin where had_dfu = 1.

 6. AMPUTATION CENSORING: applied in R using first_amp_date. A patient is
    removed from both numerator and denominator in bins after their first
    (DFU-related) amputation. See R/_assumptions.md §5.3.

 7. EXPORT: CSVs are written to D:\WPWatson for transfer to Mac/Dropbox.
    The user should copy bin_activity_commercial.csv and
    bin_activity_medicare.csv to the Dropbox analytic folder.
*****************************************************************************/
