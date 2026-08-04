/*****************************************************************************
 Step 7: Common Primary Care (E&M) Visit Extract

 Purpose
 -------
 Extracts per-person half-year flags for "common primary care" visits so the
 R pipeline can test whether the Delta cold-spot in DFU prevalence persists
 for general PCP-type utilization. If PCP cold-spots overlap the DFU cold-
 spot, the DFU signal is likely an access-to-care artifact rather than a
 disease-distribution phenomenon.

 Code set (per user, 2026-08-04)
 -------------------------------
   Office/outpatient E&M: 99201-99215
   Preventive visits:     99381-99387 (new), 99391-99397 (established)

 No place-of-service (mc015) restriction is applied: these HCPCS codes are
 outpatient-office by definition and do not overlap the ED (99281-99285) or
 inpatient E&M (99221-99239) code families.

 Architecture
 ------------
   PART A:  Commercial PCP-visit passthrough (CLAIM_SVC_DT 2017-2022, mc055,
            grouped by person-year with H1/H2 flags via mc017).
   PART B:  Medicare Part B carrier PCP passthrough (APCD_MCR_PRTB_CAR_LIN,
            hcpcs_cd, by bene_id-year).
   PART C:  Medicare outpatient revenue PCP passthrough (APCD_MCR_OUT_REV,
            hcpcs_cd, by bene_id-year).
   PART D:  Full-population MEST enrollment (mirror step3c, NO DM filter).
   PART E:  Full-population MEMBER + BEN_SUM + MEST identity join and ZIP
            lookup (mirror step5, NO DM filter).
   PART F:  Merge PCP flags across payers, key on apcd_unique_id, export
            pcp_visits_bin.csv and all_enrollment.csv, all_zip_lookup.csv.
   PART G:  Summary + cleanup.

 Inputs (server)
 ---------------
   public.CLAIM_SVC_DT_2017..2022      (commercial + Medicaid + MA claims)
   public.APCD_MCR_PRTB_CAR_LIN         (Medicare FFS carrier line, PCP-heavy)
   public.APCD_MCR_OUT_REV              (Medicare FFS outpatient revenue center)
   public.AR_APCD_24B_MEST              (enrollment string + payer_type)
   public.APCD_MCR_BEN_SUM              (FFS beneficiary summary)
   public.MEMBER                        (commercial ZIP + me107 -> MEST)

 Outputs (D:\WPWatson)
 ---------------------
   pcp_visits_bin.csv
     Long-format per apcd_unique_id x bin_id (2017-H1..2022-H2).
     Columns: apcd_unique_id, bin_id, has_pcp_visit (0/1), n_pcp_visits.
     Persons with NO PCP visit in a bin are OMITTED (join left in R).

   all_enrollment.csv
     Wide-format per apcd_unique_id, mirroring step3c's cohort_continuous
     but WITHOUT the DM restriction. All APCD-enrolled persons 2017-2022.
     Columns: apcd_unique_id, gender, m_h1_2017..m_h2_2022,
              total_months_enrolled, n_bins_ge_3_of_6, n_bins_with_any,
              continuous_enrolled.

   all_zip_lookup.csv
     One row per apcd_unique_id with residential ZIP (5-digit AR only).
     Columns: apcd_unique_id, ar_zip, zip_source, data_source.

   step7_summary.txt

 Notes
 -----
 The R script (R/10_pcp_hotspot.R) joins pcp_visits_bin -> all_enrollment
 -> all_zip_lookup -> payer_strata to build ZCTA-bin panels for both:
   (a) whole APCD-enrolled population
   (b) diabetes cohort only (filter by dm_dfu_analytic.csv IDs)
 and runs the same EB / KNN / EHSA pipeline used by scripts 05/07/09.

 Prerequisites
 -------------
 Standalone. Reads from server only. No dependence on step3/step5 WORK
 datasets. Should be runnable in a fresh SAS session.

 Author: William P. Watson (UAMS)
 Created: 2026-08-04
*****************************************************************************/

libname arapcd odbc
    noprompt="dsn=APCD-24D;Trusted_connection=yes"
    schema=public;

libname mylib 'D:\WPWatson';
%let outdir = D:\WPWatson;

%let q = %str(%');

/* Study window */
%let yr_start  = 2017;
%let yr_end    = 2022;
%let year_list = %str('2017','2018','2019','2020','2021','2022');

/* Medical payer types (same as step3c) */
%let medical_payers = %str('COM','MCD','MCR_ADV','QHP','HCIP','EBD','PASSE','MCD_QHP');

/* PCP HCPCS code set — hardcoded as an OR expression to avoid IN-list issues */
/* Office E&M: 99201-99215; Preventive: 99381-99387, 99391-99397             */
%let pcp_codes_sql = %str(
      hcpcs_col in (
        '99201','99202','99203','99204','99205',
        '99211','99212','99213','99214','99215',
        '99381','99382','99383','99384','99385','99386','99387',
        '99391','99392','99393','99394','99395','99396','99397'
      )
);


/* ======================================================================= */
/* PART A: Commercial PCP passthrough (2017-2022)                            */
/*   Per person-year: n_pcp_h1, n_pcp_h2 (visit counts in each half)         */
/*   mc055 = HCPCS, mc017 = service date, key = (mc001, mc006, mc009)        */
/*                                                                            */
/*   NOTE: This table also carries Medicaid + Medicare-Advantage claims       */
/*   (all payers in the AR APCD medical claims file). Payer stratification    */
/*   is applied downstream in R using payer_strata.csv.                       */
/* ======================================================================= */

%macro get_comm_pcp(yr, first=0);
    proc sql;
        connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
        create table _comm_pcp_yr as
        select * from connection to odbc (
            select mc001 as submitter,
                   mc006 as group_policy,
                   mc009 as person_code,
                   &yr. as claim_year,
                   sum(case when extract(month from mc017) <= 6 then 1 else 0 end)
                       as n_pcp_h1,
                   sum(case when extract(month from mc017) >= 7 then 1 else 0 end)
                       as n_pcp_h2
            from public.CLAIM_SVC_DT_&yr.
            where upper(mc055) in (
                    '99201','99202','99203','99204','99205',
                    '99211','99212','99213','99214','99215',
                    '99381','99382','99383','99384','99385','99386','99387',
                    '99391','99392','99393','99394','99395','99396','99397'
                  )
              and mc017 is not null
            group by mc001, mc006, mc009
        );
        disconnect from odbc;
    quit;

    %if &first = 1 %then %do;
        data mylib.comm_pcp_year; set _comm_pcp_yr; run;
    %end;
    %else %do;
        proc append base=mylib.comm_pcp_year data=_comm_pcp_yr force; run;
    %end;

    proc datasets lib=work nolist; delete _comm_pcp_yr; quit;
    %put NOTE: A-&yr. Commercial PCP extract complete.;
%mend;

%get_comm_pcp(2017, first=1);
%get_comm_pcp(2018);
%get_comm_pcp(2019);
%get_comm_pcp(2020);
%get_comm_pcp(2021);
%get_comm_pcp(2022);


/* ======================================================================= */
/* PART B: Medicare Part B carrier PCP passthrough                           */
/*   Per bene_id per year: n_pcp_h1, n_pcp_h2. hcpcs_cd = HCPCS,             */
/*   line_1st_expns_dt = service date.                                        */
/* ======================================================================= */

proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib.mcr_prtb_pcp as
    select * from connection to odbc (
        select bene_id,
               extract(year from line_1st_expns_dt) as claim_year,
               sum(case when extract(month from line_1st_expns_dt) <= 6
                        then 1 else 0 end) as n_pcp_h1,
               sum(case when extract(month from line_1st_expns_dt) >= 7
                        then 1 else 0 end) as n_pcp_h2
        from public.APCD_MCR_PRTB_CAR_LIN
        where upper(hcpcs_cd) in (
                '99201','99202','99203','99204','99205',
                '99211','99212','99213','99214','99215',
                '99381','99382','99383','99384','99385','99386','99387',
                '99391','99392','99393','99394','99395','99396','99397'
              )
          and line_1st_expns_dt is not null
          and extract(year from line_1st_expns_dt) between &yr_start and &yr_end
        group by bene_id, extract(year from line_1st_expns_dt)
    );
    disconnect from odbc;
quit;
%put NOTE: B-1 Medicare Part B carrier PCP extract complete.;


/* ======================================================================= */
/* PART C: Medicare outpatient revenue PCP passthrough                       */
/*   Captures FQHC/RHC/hospital-outpatient E&M billed through the outpatient */
/*   revenue center rather than Part B carrier. hcpcs_cd = HCPCS,            */
/*   rev_cntr_dt = service date.                                             */
/* ======================================================================= */

proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib.mcr_out_pcp as
    select * from connection to odbc (
        select bene_id,
               extract(year from rev_cntr_dt) as claim_year,
               sum(case when extract(month from rev_cntr_dt) <= 6
                        then 1 else 0 end) as n_pcp_h1,
               sum(case when extract(month from rev_cntr_dt) >= 7
                        then 1 else 0 end) as n_pcp_h2
        from public.APCD_MCR_OUT_REV
        where upper(hcpcs_cd) in (
                '99201','99202','99203','99204','99205',
                '99211','99212','99213','99214','99215',
                '99381','99382','99383','99384','99385','99386','99387',
                '99391','99392','99393','99394','99395','99396','99397'
              )
          and rev_cntr_dt is not null
          and extract(year from rev_cntr_dt) between &yr_start and &yr_end
        group by bene_id, extract(year from rev_cntr_dt)
    );
    disconnect from odbc;
quit;
%put NOTE: C-1 Medicare outpatient revenue PCP extract complete.;


/* ======================================================================= */
/* PART D: Full-population MEST enrollment (mirror step3c, NO DM filter)     */
/*   Bitwise-OR monthly enrollment strings across medical payer types,       */
/*   then union with BEN_SUM FFS presence, then aggregate to bin months.    */
/* ======================================================================= */

/* D-1: MEST monthly flags */
proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._mest_enroll_all as
    select * from connection to odbc (
        select apcd_unique_id,
               gender,
               cast(year as integer) as year,
               max(substring(enrollment_string, 1,  1)) as m01,
               max(substring(enrollment_string, 2,  1)) as m02,
               max(substring(enrollment_string, 3,  1)) as m03,
               max(substring(enrollment_string, 4,  1)) as m04,
               max(substring(enrollment_string, 5,  1)) as m05,
               max(substring(enrollment_string, 6,  1)) as m06,
               max(substring(enrollment_string, 7,  1)) as m07,
               max(substring(enrollment_string, 8,  1)) as m08,
               max(substring(enrollment_string, 9,  1)) as m09,
               max(substring(enrollment_string, 10, 1)) as m10,
               max(substring(enrollment_string, 11, 1)) as m11,
               max(substring(enrollment_string, 12, 1)) as m12
        from public.AR_APCD_24B_MEST
        where payer_type in (&medical_payers)
          and year in (&year_list)
          and apcd_unique_id is not null
          and apcd_unique_id <> ''
        group by apcd_unique_id, gender, year
    );
    disconnect from odbc;
quit;
%put NOTE: D-1 MEST enrollment (all persons) extracted.;

/* D-2: BEN_SUM Medicare FFS presence */
proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._bensum_years_all as
    select * from connection to odbc (
        select bene_id,
               apcd_unique_id,
               sex_ident_cd,
               bene_enrollmt_ref_yr as year
        from public.APCD_MCR_BEN_SUM
        where bene_enrollmt_ref_yr between &yr_start and &yr_end
          and apcd_unique_id is not null
          and apcd_unique_id <> ''
    );
    disconnect from odbc;
quit;
%put NOTE: D-2 BEN_SUM (all persons) extracted.;

/* D-3: Union MEST + BEN_SUM (12-month full flags for FFS) */
data mylib._enroll_unified_all;
    length source $4 apcd_unique_id $90 gender $1;
    set
        mylib._mest_enroll_all (in=from_mest)
        mylib._bensum_years_all (in=from_bensum
                                 rename=(sex_ident_cd=_sex_code));
    if from_mest then source = 'MEST';
    else if from_bensum then do;
        source = 'FFS';
        if _sex_code = '1' then gender = 'M';
        else if _sex_code = '2' then gender = 'F';
        else gender = 'U';
        m01='1'; m02='1'; m03='1'; m04='1'; m05='1'; m06='1';
        m07='1'; m08='1'; m09='1'; m10='1'; m11='1'; m12='1';
    end;
    drop _sex_code;
run;

/* D-4: Bitwise OR across sources per person-year */
proc sql;
    create table mylib._enroll_by_year_all as
    select apcd_unique_id,
           case when max(gender) = 'U' then min(gender) else max(gender) end
               as gender length=1,
           year,
           max(m01) as m01, max(m02) as m02, max(m03) as m03,
           max(m04) as m04, max(m05) as m05, max(m06) as m06,
           max(m07) as m07, max(m08) as m08, max(m09) as m09,
           max(m10) as m10, max(m11) as m11, max(m12) as m12
    from mylib._enroll_unified_all
    group by apcd_unique_id, year;
quit;

/* D-5: Convert monthly chars to numeric, sum to H1 / H2 months per year */
data mylib._enroll_bins_all;
    set mylib._enroll_by_year_all;
    array mchar {12} $1 m01-m12;
    array mnum  {12}    n01-n12;
    do i = 1 to 12;
        mnum[i] = input(mchar[i], 1.);
        if missing(mnum[i]) then mnum[i] = 0;
    end;
    h1_months = sum(of n01-n06);
    h2_months = sum(of n07-n12);
    drop i n01-n12;
run;

/* D-6: Pivot wide: one row per apcd_unique_id with 12 bin-month columns */
proc sql;
    create table mylib.all_enrollment_raw as
    select apcd_unique_id,
           max(gender) as gender length=1,
           sum(case when year=2017 then h1_months else 0 end) as m_h1_2017,
           sum(case when year=2017 then h2_months else 0 end) as m_h2_2017,
           sum(case when year=2018 then h1_months else 0 end) as m_h1_2018,
           sum(case when year=2018 then h2_months else 0 end) as m_h2_2018,
           sum(case when year=2019 then h1_months else 0 end) as m_h1_2019,
           sum(case when year=2019 then h2_months else 0 end) as m_h2_2019,
           sum(case when year=2020 then h1_months else 0 end) as m_h1_2020,
           sum(case when year=2020 then h2_months else 0 end) as m_h2_2020,
           sum(case when year=2021 then h1_months else 0 end) as m_h1_2021,
           sum(case when year=2021 then h2_months else 0 end) as m_h2_2021,
           sum(case when year=2022 then h1_months else 0 end) as m_h1_2022,
           sum(case when year=2022 then h2_months else 0 end) as m_h2_2022
    from mylib._enroll_bins_all
    group by apcd_unique_id;
quit;

/* Add derived fields */
data mylib.all_enrollment;
    set mylib.all_enrollment_raw;
    array bins {12} m_h1_2017 m_h2_2017 m_h1_2018 m_h2_2018
                    m_h1_2019 m_h2_2019 m_h1_2020 m_h2_2020
                    m_h1_2021 m_h2_2021 m_h1_2022 m_h2_2022;
    total_months_enrolled = sum(of bins[*]);
    n_bins_ge_3_of_6 = 0;
    n_bins_with_any  = 0;
    all_bins_full    = 1;
    do i = 1 to 12;
        if bins[i] >= 3 then n_bins_ge_3_of_6 + 1;
        if bins[i] >= 1 then n_bins_with_any  + 1;
        if bins[i] <  6 then all_bins_full = 0;
    end;
    continuous_enrolled = all_bins_full;
    drop i all_bins_full;
run;
%put NOTE: D-6 All-population enrollment table built.;


/* ======================================================================= */
/* PART E: Full-population ZIP lookup + identity join                        */
/*   Mirrors step5's ZIP hierarchy but WITHOUT the DM restriction:           */
/*     Commercial: MEMBER.me017 (preferred) -> CLAIM_2024.mc016 (fallback)  */
/*     Medicare:   BEN_SUM.zip_cd                                            */
/*   Then keyed on apcd_unique_id via MEMBER.me107 -> MEST (commercial) or  */
/*   BEN_SUM.apcd_unique_id directly (Medicare).                             */
/* ======================================================================= */

/* E-1: Latest MEMBER per (me001, me006, me010) — needed for commercial ZIP */
proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._member_all as
    select * from connection to odbc (
        select a.me001, a.me006, a.me010, a.me016, a.me017, a.me107
        from public.MEMBER a
        inner join (
            select me001, me006, me010, max(reckey) as max_rk
            from public.MEMBER
            group by me001, me006, me010
        ) b on a.me001 = b.me001 and a.me006 = b.me006
           and a.me010 = b.me010 and a.reckey = b.max_rk
    );
    disconnect from odbc;
quit;
%put NOTE: E-1 MEMBER (all persons) extracted.;

/* E-2: MEST identity — map (submitter, member_id) -> apcd_unique_id + gender */
proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._mest_id as
    select * from connection to odbc (
        select a.submitter, a.member_id, a.apcd_unique_id, a.gender
        from public.AR_APCD_24B_MEST a
        inner join (
            select submitter, member_id, max(year) as max_yr
            from public.AR_APCD_24B_MEST
            group by submitter, member_id
        ) b on a.submitter = b.submitter and a.member_id = b.member_id
           and a.year = b.max_yr
    );
    disconnect from odbc;
quit;
%put NOTE: E-2 MEST identity extracted.;

/* E-3: BEN_SUM latest — Medicare ZIP */
proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._bensum_latest_all as
    select * from connection to odbc (
        select a.bene_id, a.zip_cd, a.state_code, a.apcd_unique_id
        from public.APCD_MCR_BEN_SUM a
        inner join (
            select bene_id, max(bene_enrollmt_ref_yr) as max_yr
            from public.APCD_MCR_BEN_SUM
            group by bene_id
        ) b on a.bene_id = b.bene_id
           and a.bene_enrollmt_ref_yr = b.max_yr
    );
    disconnect from odbc;
quit;
%put NOTE: E-3 BEN_SUM latest (all persons) extracted.;

/* E-4: Claim ZIP fallback from 2024 commercial claims */
proc sql;
    connect to odbc (noprompt="dsn=APCD-24D;Trusted_connection=yes");
    create table mylib._claim_zip_2024_all as
    select * from connection to odbc (
        select mc001 as submitter, mc006 as group_policy, mc009 as person_code,
               max(mc016) as zip_claim
        from public.CLAIM_SVC_DT_2024
        where mc016 is not null and mc016 <> ''
        group by mc001, mc006, mc009
    );
    disconnect from odbc;
quit;
%put NOTE: E-4 Claim ZIP fallback extracted.;

/* E-5: Commercial ZIP lookup keyed on apcd_unique_id (via MEMBER.me107 -> MEST) */
proc sql;
    create table work.zip_commercial as
    select case
               when m.me017 is not null and m.me017 ne '' then m.me017
               when cz.zip_claim is not null and cz.zip_claim ne '' then cz.zip_claim
               else ''
           end as zip_final length=10,
           case
               when m.me017 is not null and m.me017 ne '' then 'MEMBER'
               when cz.zip_claim is not null and cz.zip_claim ne '' then 'CLAIM'
               else 'MISSING'
           end as zip_source length=10,
           t.apcd_unique_id,
           'COMMERCIAL' as data_source length=10
    from mylib._member_all m
    left join mylib._claim_zip_2024_all cz
        on m.me001 = cz.submitter
       and m.me006 = cz.group_policy
       and m.me010 = cz.person_code
    left join mylib._mest_id t
        on m.me001 = t.submitter
       and m.me107 = t.member_id
    where t.apcd_unique_id is not null
      and t.apcd_unique_id ne '';
quit;

/* E-6: Medicare ZIP lookup keyed on apcd_unique_id */
proc sql;
    create table work.zip_medicare as
    select b.zip_cd as zip_final length=10,
           case when b.zip_cd is not null and b.zip_cd ne '' then 'BEN_SUM'
                else 'MISSING' end as zip_source length=10,
           b.apcd_unique_id,
           'MEDICARE' as data_source length=10
    from mylib._bensum_latest_all b
    where b.apcd_unique_id is not null
      and b.apcd_unique_id ne '';
quit;

/* E-7: Stack + Arkansas filter + dedup on apcd_unique_id                    */
/*      When a person appears in both sources, prefer BEN_SUM (higher ZIP    */
/*      completeness).                                                        */
proc sql;
    create table work.zip_stack as
    select 'MEDICARE' as data_source length=10,
           apcd_unique_id,
           substr(strip(zip_final), 1, 5) as ar_zip length=5,
           zip_source,
           1 as src_rank    /* prefer Medicare on tie */
    from work.zip_medicare
    where zip_final ne ''
      and substr(strip(zip_final), 1, 2) in ('71','72')

    union all

    select 'COMMERCIAL' as data_source,
           apcd_unique_id,
           substr(strip(zip_final), 1, 5) as ar_zip length=5,
           zip_source,
           2 as src_rank
    from work.zip_commercial
    where zip_final ne ''
      and substr(strip(zip_final), 1, 2) in ('71','72');
quit;

proc sql;
    create table work.all_zip_lookup as
    select apcd_unique_id,
           max(ar_zip) as ar_zip length=5,       /* stable pick within group */
           min(src_rank) as best_src_rank,
           max(data_source) as data_source length=10,
           max(zip_source) as zip_source length=10
    from work.zip_stack
    group by apcd_unique_id;
quit;
%put NOTE: E-7 All-population ZIP lookup built.;


/* ======================================================================= */
/* PART F: Merge PCP flags -> apcd_unique_id, pivot to bin, export           */
/* ======================================================================= */

/* F-1: Commercial PCP per person-year -> apcd_unique_id */
proc sql;
    create table work.comm_pcp_id as
    select t.apcd_unique_id,
           c.claim_year,
           sum(c.n_pcp_h1) as n_pcp_h1,
           sum(c.n_pcp_h2) as n_pcp_h2
    from mylib.comm_pcp_year c
    inner join mylib._member_all m
        on c.submitter = m.me001
       and c.group_policy = m.me006
       and c.person_code = m.me010
    inner join mylib._mest_id t
        on m.me001 = t.submitter
       and m.me107 = t.member_id
    where t.apcd_unique_id is not null and t.apcd_unique_id ne ''
    group by t.apcd_unique_id, c.claim_year;
quit;

/* F-2: Medicare PCP: stack Part B carrier + Outpatient revenue, sum by     */
/*      (bene_id, year), then map to apcd_unique_id via BEN_SUM.             */
data work._mcr_pcp_stack;
    set mylib.mcr_prtb_pcp mylib.mcr_out_pcp;
run;

proc sql;
    create table work._mcr_pcp_by_bene as
    select bene_id,
           claim_year,
           sum(n_pcp_h1) as n_pcp_h1,
           sum(n_pcp_h2) as n_pcp_h2
    from work._mcr_pcp_stack
    where claim_year between &yr_start and &yr_end
    group by bene_id, claim_year;
quit;

proc sql;
    create table work.mcr_pcp_id as
    select b.apcd_unique_id,
           m.claim_year,
           m.n_pcp_h1,
           m.n_pcp_h2
    from work._mcr_pcp_by_bene m
    inner join mylib._bensum_latest_all b
        on m.bene_id = b.bene_id
    where b.apcd_unique_id is not null and b.apcd_unique_id ne '';
quit;

/* F-3: Union commercial + Medicare, then aggregate to person-year */
data work.pcp_year_union;
    length apcd_unique_id $90;
    set work.comm_pcp_id work.mcr_pcp_id;
run;

proc sql;
    create table work.pcp_year as
    select apcd_unique_id,
           claim_year,
           sum(n_pcp_h1) as n_pcp_h1,
           sum(n_pcp_h2) as n_pcp_h2
    from work.pcp_year_union
    where apcd_unique_id is not null and apcd_unique_id ne ''
      and claim_year between &yr_start and &yr_end
    group by apcd_unique_id, claim_year;
quit;

/* F-4: Reshape to long bin_id format */
data work.pcp_bin_long;
    set work.pcp_year;
    length bin_id $8;
    if n_pcp_h1 > 0 then do;
        bin_id = catx('-', put(claim_year,4.), 'H1');
        has_pcp_visit = 1;
        n_pcp_visits  = n_pcp_h1;
        output;
    end;
    if n_pcp_h2 > 0 then do;
        bin_id = catx('-', put(claim_year,4.), 'H2');
        has_pcp_visit = 1;
        n_pcp_visits  = n_pcp_h2;
        output;
    end;
    keep apcd_unique_id bin_id has_pcp_visit n_pcp_visits;
run;

/* F-5: Exports */
proc export data=work.pcp_bin_long
    outfile="&outdir.\pcp_visits_bin.csv"
    dbms=csv replace;
run;

proc export data=mylib.all_enrollment
    outfile="&outdir.\all_enrollment.csv"
    dbms=csv replace;
run;

proc export data=work.all_zip_lookup
    outfile="&outdir.\all_zip_lookup.csv"
    dbms=csv replace;
run;
%put NOTE: F-5 CSV exports complete.;


/* ======================================================================= */
/* PART G: Summary + cleanup                                                */
/* ======================================================================= */

proc sql;
    select count(*)                    into :n_all_enroll  from mylib.all_enrollment;
    select count(*)                    into :n_continuous  from mylib.all_enrollment
        where continuous_enrolled = 1;
    select count(*)                    into :n_zip_lookup  from work.all_zip_lookup;
    select count(distinct apcd_unique_id) into :n_pcp_persons from work.pcp_bin_long;
    select count(*)                    into :n_pcp_rows    from work.pcp_bin_long;
    select sum(has_pcp_visit)          into :n_pcp_bins    from work.pcp_bin_long;
quit;

filename sumrep "&outdir.\step7_summary.txt";
data _null_;
    file sumrep;
    put "STEP 7: PCP VISIT EXTRACT (all-population)";
    put "==========================================";
    put " ";
    put "Study window: 2017-01 through 2022-12 (12 half-year bins)";
    put "PCP HCPCS codes: 99201-99215 (office E&M), 99381-99387, 99391-99397 (preventive)";
    put " ";
    put "Enrollment (all APCD-enrolled 2017-2022):";
    put "  Total persons:                    &n_all_enroll";
    put "  Continuously enrolled (72 mo):    &n_continuous";
    put " ";
    put "ZIP lookup (Arkansas ZIPs only):";
    put "  Persons with AR ZIP:              &n_zip_lookup";
    put " ";
    put "PCP visits:";
    put "  Persons with >=1 PCP visit:       &n_pcp_persons";
    put "  Person-bin rows exported:         &n_pcp_rows";
    put " ";
    put "Outputs:";
    put "  pcp_visits_bin.csv       (long: apcd_unique_id x bin_id)";
    put "  all_enrollment.csv       (wide: bin-months per person)";
    put "  all_zip_lookup.csv       (per-person AR ZIP)";
run;

%put NOTE: Step 7 complete. Summary at &outdir.\step7_summary.txt;

proc datasets library=mylib nolist;
    delete comm_pcp_year mcr_prtb_pcp mcr_out_pcp
           _mest_enroll_all _bensum_years_all _enroll_unified_all
           _enroll_by_year_all _enroll_bins_all all_enrollment_raw
           _member_all _mest_id _bensum_latest_all _claim_zip_2024_all;
quit;

proc datasets lib=work nolist;
    delete comm_pcp_id mcr_pcp_id _mcr_pcp_stack _mcr_pcp_by_bene
           pcp_year_union pcp_year zip_commercial zip_medicare zip_stack;
quit;

/*****************************************************************************
 NOTES

 1. HCPCS CODE CHOICE
    99201 was deleted 2021-01-01 by AMA. Kept in the code list so
    pre-2021 claims still match; post-2021 claims naturally won't carry it.
    99441-99443 (telephone E&M, non-face-to-face) are NOT included -- they
    were surge-adopted during 2020 COVID and can inflate PCP-visit counts
    for that year in ways that don't reflect prior utilization patterns.

 2. NO PLACE-OF-SERVICE FILTER
    Office/preventive E&M codes are outpatient-office by definition; ED and
    inpatient E&M use disjoint code families. mc015 (POS on commercial
    claims) is inconsistently populated across submitters and would drop
    legitimate visits without gaining specificity.

 3. MEDICAID
    Medicaid PCP visits flow through the commercial CLAIM_SVC_DT_YYYY
    tables (same schema, distinguished by MEST payer_type). Nothing
    additional needed here -- payer stratification happens in R via
    payer_strata.csv.

 4. MEDICARE FFS OUTPATIENT VS PART B CARRIER
    Both extracts run because FQHC/RHC/hospital-outpatient E&M can bill
    through either channel. Part D combines them per bene_id-year and
    dedupes on the (apcd_unique_id, year) key.

 5. ENROLLMENT BUILD MIRRORS STEP3C
    Same bitwise-OR logic, same medical-payer set, same MEST year cast,
    same H1/H2 bin definition -- but WITHOUT the DM cohort inner-join
    (Part E in step3c). Person-halfyears in the R pipeline are therefore
    directly comparable to the DFU denominator.

 6. ZIP HIERARCHY
    Commercial: MEMBER.me017 preferred, mc016 claim fallback (matches step5)
    Medicare:   BEN_SUM.zip_cd
    Cross-source: prefer BEN_SUM ZIP on tie (higher completeness).
*****************************************************************************/
