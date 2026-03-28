/*****************************************************************************
 Step 4: Identify Diabetic Foot Ulcers (DFU)
 Purpose: Among the diabetes cohort from Step 3, identify DFU cases

 ICD-10-CM Code List for DFU:
 =========================================================================
 PRIMARY CODES — L97.x series (Non-pressure chronic ulcer of lower limb):
   L97.1xx  Non-pressure chronic ulcer of thigh
   L97.2xx  Non-pressure chronic ulcer of calf
   L97.3xx  Non-pressure chronic ulcer of ankle
   L97.4xx  Non-pressure chronic ulcer of heel and midfoot
   L97.5xx  Non-pressure chronic ulcer of other part of foot
   L97.8xx  Non-pressure chronic ulcer of other part of lower leg
   L97.9xx  Non-pressure chronic ulcer of unspecified part of lower leg

   Severity (7th character where applicable):
     1 = limited to breakdown of skin
     2 = with fat layer exposed
     3 = with necrosis of muscle
     4 = with necrosis of bone
     8 = with other specified severity
     9 = with unspecified severity

   Laterality (5th character):
     1 = right
     2 = left
     9 = unspecified

 DIABETES-SPECIFIC COMBINATION CODES:
   E10.621  T1D with foot ulcer
   E10.622  T1D with other skin ulcer
   E11.621  T2D with foot ulcer
   E11.622  T2D with other skin ulcer

 SUPPLEMENTAL (used with L97.x as manifestation):
   E10.620  T1D with dermatitis
   E10.628  T1D with other skin complications
   E11.620  T2D with dermatitis
   E11.628  T2D with other skin complications

 NOTE: Per ICD-10-CM coding guidelines, diabetic foot ulcers should be
 coded with BOTH the diabetes code (E10/E11.621) AND the L97.x code.
 In practice, claims may have only one or the other. We capture all.
 =========================================================================

 Commercial DX fields: 14 diagnosis columns
   mc039 = admitting dx
   mc041 = principal dx
   mc042 = dx2, mc043 = dx3, ... mc053 = dx13

 Commercial person key: mc001 (submitter) + mc006 (hashed group/policy)
   + mc009 (member sequence) — 3-field composite

 Prerequisite: Step 3 must be run first to create:
   - dm_cohort_commercial (with person_code, group_policy, submitter, dm_type)
   - dm_cohort_medicare   (with bene_id, dm_type)
   - dm_claims_commercial (claim-level diabetes records)
   - dm_claims_medicare   (claim-level Medicare diabetes records)
*****************************************************************************/

libname arapcd odbc
    noprompt="dsn=APCD-24D;Trusted_connection=yes"
    schema=public;

%let outdir = /Users/williamwatson/Claude_Code/Aim_3_Dissertation;

/* ======================================================================= */
/* Define DFU code patterns                                                */
/* ======================================================================= */

/*
   We need to search for these patterns in diagnosis fields:
   - L97.1% through L97.9% (non-pressure chronic ulcer of lower limb)
   - E10.621, E10.622 (T1D foot/skin ulcer)
   - E11.621, E11.622 (T2D foot/skin ulcer)

   Commercial claims have 14 DX fields:
     mc039 (admitting dx), mc041 (principal dx), mc042..mc053 (dx2..dx13)
*/

/* ======================================================================= */
/* PART A: Commercial/Medicaid DFU — CLAIM_SVC_DT                          */
/*   Scans 14 DX fields: mc039, mc041-mc053                               */
/*   Person key: mc001 + mc006 + mc009 (3-field composite)                 */
/* ======================================================================= */

%macro scan_dfu_year(yr);
    select c.mc009 as person_code,
           c.mc006 as group_policy,
           c.mc001 as submitter,
           c.mc017 as service_date,
           &yr. as claim_year,
           c.mc039 as dx_admitting,
           c.mc041 as dx_principal,
           c.mc042 as dx2,
           c.mc018 as claim_type,      /* I=Institutional, P=Professional */
           c.mc019 as place_of_service,
           /* Flag DFU code source — scan all 14 DX fields */
           case when (   upcase(c.mc039) like 'L97%'
                      or upcase(c.mc041) like 'L97%'
                      or upcase(c.mc042) like 'L97%'
                      or upcase(c.mc043) like 'L97%'
                      or upcase(c.mc044) like 'L97%'
                      or upcase(c.mc045) like 'L97%'
                      or upcase(c.mc046) like 'L97%'
                      or upcase(c.mc047) like 'L97%'
                      or upcase(c.mc048) like 'L97%'
                      or upcase(c.mc049) like 'L97%'
                      or upcase(c.mc050) like 'L97%'
                      or upcase(c.mc051) like 'L97%'
                      or upcase(c.mc052) like 'L97%'
                      or upcase(c.mc053) like 'L97%'
                )
                then 1 else 0 end as has_l97,
           case when (   upcase(c.mc039) in ('E10621','E10622','E11621','E11622')
                      or upcase(c.mc041) in ('E10621','E10622','E11621','E11622')
                      or upcase(c.mc042) in ('E10621','E10622','E11621','E11622')
                      or upcase(c.mc043) in ('E10621','E10622','E11621','E11622')
                      or upcase(c.mc044) in ('E10621','E10622','E11621','E11622')
                      or upcase(c.mc045) in ('E10621','E10622','E11621','E11622')
                      or upcase(c.mc046) in ('E10621','E10622','E11621','E11622')
                      or upcase(c.mc047) in ('E10621','E10622','E11621','E11622')
                      or upcase(c.mc048) in ('E10621','E10622','E11621','E11622')
                      or upcase(c.mc049) in ('E10621','E10622','E11621','E11622')
                      or upcase(c.mc050) in ('E10621','E10622','E11621','E11622')
                      or upcase(c.mc051) in ('E10621','E10622','E11621','E11622')
                      or upcase(c.mc052) in ('E10621','E10622','E11621','E11622')
                      or upcase(c.mc053) in ('E10621','E10622','E11621','E11622')
                )
                then 1 else 0 end as has_dm_combo,
           /* Capture the first L97 code found across all 14 DX fields */
           case
               when upcase(c.mc039) like 'L97%' then upcase(c.mc039)
               when upcase(c.mc041) like 'L97%' then upcase(c.mc041)
               when upcase(c.mc042) like 'L97%' then upcase(c.mc042)
               when upcase(c.mc043) like 'L97%' then upcase(c.mc043)
               when upcase(c.mc044) like 'L97%' then upcase(c.mc044)
               when upcase(c.mc045) like 'L97%' then upcase(c.mc045)
               when upcase(c.mc046) like 'L97%' then upcase(c.mc046)
               when upcase(c.mc047) like 'L97%' then upcase(c.mc047)
               when upcase(c.mc048) like 'L97%' then upcase(c.mc048)
               when upcase(c.mc049) like 'L97%' then upcase(c.mc049)
               when upcase(c.mc050) like 'L97%' then upcase(c.mc050)
               when upcase(c.mc051) like 'L97%' then upcase(c.mc051)
               when upcase(c.mc052) like 'L97%' then upcase(c.mc052)
               when upcase(c.mc053) like 'L97%' then upcase(c.mc053)
               else ''
           end as l97_code length=10,
           /* Capture the specific E1x.62x code across all 14 DX fields */
           case
               when upcase(c.mc039) in ('E10621','E10622','E11621','E11622')
                    then upcase(c.mc039)
               when upcase(c.mc041) in ('E10621','E10622','E11621','E11622')
                    then upcase(c.mc041)
               when upcase(c.mc042) in ('E10621','E10622','E11621','E11622')
                    then upcase(c.mc042)
               when upcase(c.mc043) in ('E10621','E10622','E11621','E11622')
                    then upcase(c.mc043)
               when upcase(c.mc044) in ('E10621','E10622','E11621','E11622')
                    then upcase(c.mc044)
               when upcase(c.mc045) in ('E10621','E10622','E11621','E11622')
                    then upcase(c.mc045)
               when upcase(c.mc046) in ('E10621','E10622','E11621','E11622')
                    then upcase(c.mc046)
               when upcase(c.mc047) in ('E10621','E10622','E11621','E11622')
                    then upcase(c.mc047)
               when upcase(c.mc048) in ('E10621','E10622','E11621','E11622')
                    then upcase(c.mc048)
               when upcase(c.mc049) in ('E10621','E10622','E11621','E11622')
                    then upcase(c.mc049)
               when upcase(c.mc050) in ('E10621','E10622','E11621','E11622')
                    then upcase(c.mc050)
               when upcase(c.mc051) in ('E10621','E10622','E11621','E11622')
                    then upcase(c.mc051)
               when upcase(c.mc052) in ('E10621','E10622','E11621','E11622')
                    then upcase(c.mc052)
               when upcase(c.mc053) in ('E10621','E10622','E11621','E11622')
                    then upcase(c.mc053)
               else ''
           end as dm_ulcer_code length=10
    from arapcd.CLAIM_SVC_DT_&yr. c
    inner join dm_cohort_commercial d
        on c.mc009 = d.person_code
       and c.mc006 = d.group_policy
       and c.mc001 = d.submitter
    where upcase(c.mc039) like 'L97%'
       or upcase(c.mc041) like 'L97%'
       or upcase(c.mc042) like 'L97%'
       or upcase(c.mc043) like 'L97%'
       or upcase(c.mc044) like 'L97%'
       or upcase(c.mc045) like 'L97%'
       or upcase(c.mc046) like 'L97%'
       or upcase(c.mc047) like 'L97%'
       or upcase(c.mc048) like 'L97%'
       or upcase(c.mc049) like 'L97%'
       or upcase(c.mc050) like 'L97%'
       or upcase(c.mc051) like 'L97%'
       or upcase(c.mc052) like 'L97%'
       or upcase(c.mc053) like 'L97%'
       or upcase(c.mc039) in ('E10621','E10622','E11621','E11622')
       or upcase(c.mc041) in ('E10621','E10622','E11621','E11622')
       or upcase(c.mc042) in ('E10621','E10622','E11621','E11622')
       or upcase(c.mc043) in ('E10621','E10622','E11621','E11622')
       or upcase(c.mc044) in ('E10621','E10622','E11621','E11622')
       or upcase(c.mc045) in ('E10621','E10622','E11621','E11622')
       or upcase(c.mc046) in ('E10621','E10622','E11621','E11622')
       or upcase(c.mc047) in ('E10621','E10622','E11621','E11622')
       or upcase(c.mc048) in ('E10621','E10622','E11621','E11622')
       or upcase(c.mc049) in ('E10621','E10622','E11621','E11622')
       or upcase(c.mc050) in ('E10621','E10622','E11621','E11622')
       or upcase(c.mc051) in ('E10621','E10622','E11621','E11622')
       or upcase(c.mc052) in ('E10621','E10622','E11621','E11622')
       or upcase(c.mc053) in ('E10621','E10622','E11621','E11622')
%mend scan_dfu_year;

proc sql;
    create table dfu_claims_commercial as
    %scan_dfu_year(2017)
    union all
    %scan_dfu_year(2018)
    union all
    %scan_dfu_year(2019)
    union all
    %scan_dfu_year(2020)
    union all
    %scan_dfu_year(2021)
    union all
    %scan_dfu_year(2022)
    union all
    %scan_dfu_year(2023)
    union all
    %scan_dfu_year(2024)
    ;
quit;

/* Add DFU identification source and severity */
data dfu_claims_commercial;
    set dfu_claims_commercial;
    length dfu_source $12 dfu_severity $30 dfu_site $30;

    /* Classification of how DFU was identified */
    if has_l97 = 1 and has_dm_combo = 1 then dfu_source = 'BOTH';
    else if has_l97 = 1 then dfu_source = 'L97_ONLY';
    else if has_dm_combo = 1 then dfu_source = 'COMBO_ONLY';

    /* Extract severity from L97 code
       With dots: L97.522 → severity at position 7
       Without dots: L97522 → severity at position 6
       Normalize by removing dots first, then severity = position 6 */
    length _l97clean $10;
    _l97clean = compress(l97_code, '.');
    if _l97clean ne '' and length(strip(_l97clean)) >= 6 then do;
        _sev = substr(strip(_l97clean), 6, 1);
        select(_sev);
            when('1') dfu_severity = 'Skin breakdown only';
            when('2') dfu_severity = 'Fat layer exposed';
            when('3') dfu_severity = 'Necrosis of muscle';
            when('4') dfu_severity = 'Necrosis of bone';
            when('8') dfu_severity = 'Other specified';
            when('9') dfu_severity = 'Unspecified';
            otherwise dfu_severity = 'Unknown/incomplete code';
        end;
    end;
    else dfu_severity = 'No L97 code';

    /* Extract site from L97 code (position 4 after removing dots) */
    if _l97clean ne '' and length(strip(_l97clean)) >= 4 then do;
        _site = substr(strip(_l97clean), 4, 1);
        select(_site);
            when('1') dfu_site = 'Thigh';
            when('2') dfu_site = 'Calf';
            when('3') dfu_site = 'Ankle';
            when('4') dfu_site = 'Heel and midfoot';
            when('5') dfu_site = 'Other part of foot';
            when('8') dfu_site = 'Other lower leg';
            when('9') dfu_site = 'Unspecified lower leg';
            otherwise dfu_site = 'Unknown';
        end;
    end;
    else dfu_site = 'No L97 code';

    drop _sev _site _l97clean;
run;

/* Aggregate to patient level for commercial — using 3-field composite key */
proc sql;
    create table dfu_cohort_commercial as
    select a.person_code,
           a.group_policy,
           a.submitter,
           b.dm_type,
           min(a.service_date) as first_dfu_date format=yymmdd10.,
           min(a.claim_year) as first_dfu_year,
           max(a.claim_year) as last_dfu_year,
           count(*) as n_dfu_claims,
           max(a.has_l97) as ever_l97,
           max(a.has_dm_combo) as ever_dm_combo,
           /* Most severe ulcer recorded */
           max(case
               when dfu_severity = 'Necrosis of bone' then 4
               when dfu_severity = 'Necrosis of muscle' then 3
               when dfu_severity = 'Fat layer exposed' then 2
               when dfu_severity = 'Skin breakdown only' then 1
               else 0
           end) as max_severity_rank,
           'COMMERCIAL' as data_source length=10
    from dfu_claims_commercial a
    inner join dm_cohort_commercial b
        on a.person_code = b.person_code
       and a.group_policy   = b.group_policy
       and a.submitter    = b.submitter
    group by a.person_code, a.group_policy, a.submitter, b.dm_type;
quit;

/* ======================================================================= */
/* PART B: Medicare DFU — Multiple claim tables                            */
/*   Uses prncpal_dgns_cd, icd_dgns_cd1..N — NO changes needed            */
/* ======================================================================= */

/* Macro for scanning DFU in Medicare tables with N diagnosis fields */
%macro scan_mcr_dfu(tbl, max_dx, has_admtg=0);
    proc sql;
        create table dfu_mcr_&tbl. as
        select bene_id,
               clm_from_dt as service_date,
               year(clm_from_dt) as claim_year,
               prncpal_dgns_cd,
               case when (
                   %if &has_admtg. = 1 %then %do;
                       upcase(admtg_dgns_cd) like 'L97%' or
                   %end;
                   upcase(prncpal_dgns_cd) like 'L97%'
                   %do i = 1 %to &max_dx.;
                       or upcase(icd_dgns_cd&i.) like 'L97%'
                   %end;
               ) then 1 else 0 end as has_l97,
               case when (
                   %if &has_admtg. = 1 %then %do;
                       upcase(admtg_dgns_cd) in ('E10621','E10622','E11621','E11622') or
                   %end;
                   upcase(prncpal_dgns_cd) in ('E10621','E10622','E11621','E11622')
                   %do i = 1 %to &max_dx.;
                       or upcase(icd_dgns_cd&i.) in ('E10621','E10622','E11621','E11622')
                   %end;
               ) then 1 else 0 end as has_dm_combo,
               /* Capture first L97 code found for severity */
               case
                   %if &has_admtg. = 1 %then %do;
                       when upcase(admtg_dgns_cd) like 'L97%' then upcase(admtg_dgns_cd)
                   %end;
                   when upcase(prncpal_dgns_cd) like 'L97%' then upcase(prncpal_dgns_cd)
                   %do i = 1 %to &max_dx.;
                       when upcase(icd_dgns_cd&i.) like 'L97%' then upcase(icd_dgns_cd&i.)
                   %end;
                   else ''
               end as l97_code length=10,
               /* Capture DM combo code */
               case
                   %if &has_admtg. = 1 %then %do;
                       when upcase(admtg_dgns_cd) in ('E10621','E10622','E11621','E11622')
                            then upcase(admtg_dgns_cd)
                   %end;
                   when upcase(prncpal_dgns_cd) in ('E10621','E10622','E11621','E11622')
                        then upcase(prncpal_dgns_cd)
                   %do i = 1 %to &max_dx.;
                       when upcase(icd_dgns_cd&i.) in ('E10621','E10622','E11621','E11622')
                            then upcase(icd_dgns_cd&i.)
                   %end;
                   else ''
               end as dm_ulcer_code length=10
        from arapcd.APCD_MCR_&tbl._CLM
        where bene_id in (select bene_id from dm_cohort_medicare)
          and (
               %if &has_admtg. = 1 %then %do;
                   upcase(admtg_dgns_cd) like 'L97%'
                   or upcase(admtg_dgns_cd) in ('E10621','E10622','E11621','E11622')
                   or
               %end;
               upcase(prncpal_dgns_cd) like 'L97%'
               or upcase(prncpal_dgns_cd) in ('E10621','E10622','E11621','E11622')
               %do i = 1 %to &max_dx.;
                   or upcase(icd_dgns_cd&i.) like 'L97%'
                   or upcase(icd_dgns_cd&i.) in ('E10621','E10622','E11621','E11622')
               %end;
          )
          and year(clm_from_dt) <= 2024
        ;
    quit;
%mend scan_mcr_dfu;

/* Part B Carrier (12 DX fields) */
%scan_mcr_dfu(PRTB_CAR, 12);

/* Outpatient (25 DX fields) */
%scan_mcr_dfu(OUT, 25);

/* Inpatient (25 DX fields + admtg_dgns_cd) */
%scan_mcr_dfu(INP, 25, has_admtg=1);

/* SNF (25 DX fields + admtg_dgns_cd), HHA, HSP, DME */
%scan_mcr_dfu(SNF, 25, has_admtg=1);
%scan_mcr_dfu(HHA, 25);
%scan_mcr_dfu(HSP, 25);
%scan_mcr_dfu(DME, 12);

/* Stack all Medicare DFU claims */
data dfu_claims_medicare;
    set dfu_mcr_PRTB_CAR
        dfu_mcr_OUT
        dfu_mcr_INP
        dfu_mcr_SNF
        dfu_mcr_HHA
        dfu_mcr_HSP
        dfu_mcr_DME;
    length dfu_source $12 dfu_severity $30 dfu_site $30;

    if has_l97 = 1 and has_dm_combo = 1 then dfu_source = 'BOTH';
    else if has_l97 = 1 then dfu_source = 'L97_ONLY';
    else if has_dm_combo = 1 then dfu_source = 'COMBO_ONLY';

    length _l97clean $10;
    _l97clean = compress(l97_code, '.');
    if _l97clean ne '' and length(strip(_l97clean)) >= 6 then do;
        _sev = substr(strip(_l97clean), 6, 1);
        select(_sev);
            when('1') dfu_severity = 'Skin breakdown only';
            when('2') dfu_severity = 'Fat layer exposed';
            when('3') dfu_severity = 'Necrosis of muscle';
            when('4') dfu_severity = 'Necrosis of bone';
            when('8') dfu_severity = 'Other specified';
            when('9') dfu_severity = 'Unspecified';
            otherwise dfu_severity = 'Unknown/incomplete code';
        end;
    end;
    else dfu_severity = 'No L97 code';

    if _l97clean ne '' and length(strip(_l97clean)) >= 4 then do;
        _site = substr(strip(_l97clean), 4, 1);
        select(_site);
            when('1') dfu_site = 'Thigh';
            when('2') dfu_site = 'Calf';
            when('3') dfu_site = 'Ankle';
            when('4') dfu_site = 'Heel and midfoot';
            when('5') dfu_site = 'Other part of foot';
            when('8') dfu_site = 'Other lower leg';
            when('9') dfu_site = 'Unspecified lower leg';
            otherwise dfu_site = 'Unknown';
        end;
    end;
    else dfu_site = 'No L97 code';

    drop _sev _site _l97clean;
run;

/* Aggregate Medicare DFU to patient level */
proc sql;
    create table dfu_cohort_medicare as
    select a.bene_id,
           b.dm_type,
           min(a.service_date) as first_dfu_date format=yymmdd10.,
           min(a.claim_year) as first_dfu_year,
           max(a.claim_year) as last_dfu_year,
           count(*) as n_dfu_claims,
           max(a.has_l97) as ever_l97,
           max(a.has_dm_combo) as ever_dm_combo,
           max(case
               when a.dfu_severity = 'Necrosis of bone' then 4
               when a.dfu_severity = 'Necrosis of muscle' then 3
               when a.dfu_severity = 'Fat layer exposed' then 2
               when a.dfu_severity = 'Skin breakdown only' then 1
               else 0
           end) as max_severity_rank,
           'MEDICARE' as data_source length=10
    from dfu_claims_medicare a
    inner join dm_cohort_medicare b
        on a.bene_id = b.bene_id
    group by a.bene_id, b.dm_type;
quit;

/* ======================================================================= */
/* PART C: Summary Reports                                                 */
/* ======================================================================= */

title "Step 4: DFU Identification Source Distribution (Commercial)";
proc sql;
    select dfu_source,
           count(*) as n_claims format=comma12.,
           count(distinct catx('|', submitter, group_policy, person_code)) as n_patients format=comma12.
    from dfu_claims_commercial
    group by dfu_source;
quit;

title "Step 4: DFU Identification Source Distribution (Medicare)";
proc sql;
    select dfu_source,
           count(*) as n_claims format=comma12.,
           count(distinct bene_id) as n_patients format=comma12.
    from dfu_claims_medicare
    group by dfu_source;
quit;

title "Step 4: DFU Patient Counts by DM Type and Data Source";
proc sql;
    select data_source, dm_type,
           count(*) as n_dfu_patients format=comma12.,
           sum(n_dfu_claims) as total_dfu_claims format=comma15.
    from (
        select dm_type, data_source, n_dfu_claims from dfu_cohort_commercial
        union all
        select dm_type, data_source, n_dfu_claims from dfu_cohort_medicare
    )
    group by data_source, dm_type
    order by data_source, dm_type;
quit;

title "Step 4: DFU by Year — Commercial";
proc sql;
    select claim_year,
           count(*) as n_dfu_claims format=comma12.,
           count(distinct catx('|', submitter, group_policy, person_code)) as n_patients format=comma12.
    from dfu_claims_commercial
    group by claim_year
    order by claim_year;
quit;

title "Step 4: DFU by Year — Medicare";
proc sql;
    select claim_year,
           count(*) as n_dfu_claims format=comma12.,
           count(distinct bene_id) as n_patients format=comma12.
    from dfu_claims_medicare
    group by claim_year
    order by claim_year;
quit;

title "Step 4: DFU Severity Distribution (L97 codes only)";
proc sql;
    select dfu_severity,
           count(*) as n_claims format=comma12.
    from (
        select dfu_severity from dfu_claims_commercial where dfu_severity ne 'No L97 code'
        union all
        select dfu_severity from dfu_claims_medicare where dfu_severity ne 'No L97 code'
    )
    group by dfu_severity
    order by dfu_severity;
quit;

title "Step 4: DFU Site Distribution (L97 codes only)";
proc sql;
    select dfu_site,
           count(*) as n_claims format=comma12.
    from (
        select dfu_site from dfu_claims_commercial where dfu_site ne 'No L97 code'
        union all
        select dfu_site from dfu_claims_medicare where dfu_site ne 'No L97 code'
    )
    group by dfu_site
    order by dfu_site;
quit;

title "Step 4: Top 20 L97 Codes Observed";
proc sql outobs=20;
    select l97_code,
           count(*) as n_claims format=comma12.
    from (
        select l97_code from dfu_claims_commercial where l97_code ne ''
        union all
        select l97_code from dfu_claims_medicare where l97_code ne ''
    )
    group by l97_code
    order by n_claims desc;
quit;

title;

/*****************************************************************************
 FULL ICD-10-CM CODE LIST USED FOR DFU IDENTIFICATION:

 PRIMARY (L97.x — Non-pressure chronic ulcer of lower limb):
   L97.1xx  Thigh (right/left/unspecified x severity)
   L97.2xx  Calf
   L97.3xx  Ankle
   L97.4xx  Heel and midfoot
   L97.5xx  Other part of foot
   L97.8xx  Other part of lower leg
   L97.9xx  Unspecified part of lower leg

 DIABETES-SPECIFIC COMBINATION CODES:
   E10.621  Type 1 diabetes with foot ulcer
   E10.622  Type 1 diabetes with other skin ulcer
   E11.621  Type 2 diabetes with foot ulcer
   E11.622  Type 2 diabetes with other skin ulcer

 CASE IDENTIFICATION CLASSIFICATION:
   BOTH       = L97.x AND E1x.62x present on same claim
   L97_ONLY   = Only L97.x code, no diabetes-ulcer combo code
   COMBO_ONLY = Only E1x.62x code, no L97.x code

 SEVERITY HIERARCHY (from L97.x 7th character):
   4 = Necrosis of bone (most severe)
   3 = Necrosis of muscle
   2 = Fat layer exposed
   1 = Skin breakdown only (least severe)
   0 = Unspecified / other / not coded

 COMMERCIAL DATA NOTES:
   - Person key is 3-field composite: mc001 + mc006 + mc009
     (submitter + hashed group/policy + member sequence)
   - 14 DX fields searched: mc039 (admitting), mc041 (principal),
     mc042..mc053 (dx2..dx13)
   - A DFU claim with E11.621 and L97.5xx on the same claim will
     now be correctly classified as BOTH across all 14 DX positions.

 MEDICARE DATA NOTES:
   - Person key is bene_id
   - Uses prncpal_dgns_cd + icd_dgns_cd1..N (up to 25 fields)

 CAVEATS:
   - L97.x codes are not specific to diabetic ulcers; they may also appear
     for venous, arterial, or other chronic ulcers. We restrict to patients
     already identified as diabetic in Step 3.
   - In practice, many claims code only E11.621 without a companion L97.x
     code (violating coding guidelines). These are captured as COMBO_ONLY.
*****************************************************************************/
