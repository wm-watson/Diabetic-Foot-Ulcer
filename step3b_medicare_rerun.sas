/* =======================================================================
   step3b_medicare_rerun.sas

   Targeted re-run of ONLY the Medicare bin activity extraction (Part C)
   through export (Part E) from step3b_bin_activity.sas.

   PRECONDITION: dm_cohort_medicare must still exist in WORK from the
   step3_cohort.sas run. If it does not, re-run step3_cohort.sas first.

   FIX: prncpl_dgns_cd (wrong) -> prncpal_dgns_cd (correct column name).
   ======================================================================= */

options compress=yes nofmterr;
libname mylib "D:\WPWatson";
%let q = %str(%');

/*--- Medicare helper macros (FIXED column name) ---*/
%macro mcr_like(prefix, max_dx, has_admtg);
    %if &has_admtg = 1 %then %do;
        upper(admtg_dgns_cd) like &q.&prefix.%&q. or
    %end;
    upper(prncpal_dgns_cd) like &q.&prefix.%&q.
    %do i = 1 %to &max_dx;
        or upper(icd_dgns_cd&i.) like &q.&prefix.%&q.
    %end;
%mend;

%macro mcr_in_combo(max_dx, has_admtg);
    %if &has_admtg = 1 %then %do;
        upper(admtg_dgns_cd) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.) or
    %end;
    upper(prncpal_dgns_cd) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.)
    %do i = 1 %to &max_dx;
        or upper(icd_dgns_cd&i.) in (&q.E10621&q.,&q.E10622&q.,&q.E11621&q.,&q.E11622&q.)
    %end;
%mend;

/* ======================================================================= */
/* PART C: Medicare bin activity (7 claim tables)                           */
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

/* Collapse across the 7 tables */
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

/* Inner-join to DM cohort for safety */
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

/* Export */
%let outdir = D:\WPWatson;
proc export data=mylib.bin_activity_medicare
    outfile="&outdir.\bin_activity_medicare.csv"
    dbms=csv replace;
run;

/* Summary */
title "Medicare Bin Activity Summary";
proc sql;
    select count(*) as n_rows format=comma15.,
           count(distinct bene_id) as n_patients format=comma12.,
           min(bin_year) as min_year,
           max(bin_year) as max_year,
           sum(had_dm)  as dm_active_bins  format=comma15.,
           sum(had_dfu) as dfu_active_bins format=comma15.
    from mylib.bin_activity_medicare;
quit;
title;

%put NOTE: Medicare bin activity re-run complete.;
%put NOTE: Output: D:\WPWatson\bin_activity_medicare.csv;
