# Instructions for an LLM: Draft the Methods Section

**Purpose.** Convert the structured decisions in this repository into a
publication-ready Methods section (and full manuscript draft) for
**Health & Place** (Elsevier). Rolling submission; no fixed deadline.

**Journal pivot (2026-05-05):** The original target was *Preventing
Chronic Disease* (PCD), Jan 2026 Call for Papers (deadline 2026-05-01).
That deadline passed. The paper has been retargeted to **Health &
Place** because (a) H&P's spatial-epidemiology readership is the
natural audience for the methodological contribution (payer-stratified
spatial analysis + three-outcome framework + fractional-cohort
selection-bias diagnosis); (b) H&P allows longer-form Methods and more
figures, accommodating the multi-stratum × multi-outcome × cohort-
sensitivity matrix; (c) reviewers will not need basic spatial-epi
terminology explained, freeing word budget for substantive findings.
See §2 below for the H&P-specific structural requirements that differ
from the prior PCD framing.

**Last updated: 2026-05-05** — pivots target journal from PCD to
Health & Place; promotes fractional cohort to primary (after
2026-04-30 hypothesis-test analysis showed continuous cohort
systematically excludes high-churn populations and hides the Delta
amputation signal). Methodology decisions otherwise unchanged from
2026-04-28 rev 4. Rev 4 incorporated the payer-stratified analysis as
primary methodology (formerly a sensitivity), the corrected Memphis
edge mechanism (TN-licensed Medicare Advantage rather than TN-licensed
commercial insurance), and the Mixed-payer Delta-Interior amputation
finding. Earlier additions (rev 3, 2026-04-21): three-outcome
framework, enrollment-based two-cohort design, local EB smoothing,
analytic-vs-display suppression separation.

> **Data scope verified 2026-04-30 against the AR APCD Medicare Element
> List** (`240223 MCR APCD Element List FINAL.xlsx`). The extract
> includes BOTH Medicare FFS and Medicare Advantage. The seven
> `MCR_*_CLM` tables are FFS claims (per the data dictionary);
> `MCR_BEN_SUM` is the CMS Master Beneficiary Summary File covering
> all Medicare beneficiaries (FFS + MA). MA encounter data is in
> `CLAIM_SVC_DT_YYYY` tables alongside Commercial and Medicaid; MA
> enrollment is in `AR_APCD_24B_MEST` with `payer_type = MCR_ADV`.
> The "Medicare" stratum in `step3d_payer_strata.sas` therefore
> correctly encompasses both FFS and MA beneficiaries; no relabeling
> needed. The methods section can refer to "Medicare" without
> qualification.

---

## 1. Inputs the LLM must read before drafting

Read all of the following, in this order, and do not begin drafting until
all have been ingested:

1. `R/_assumptions.md` — the authoritative methodological decisions log.
   **This is the primary source.** Every Methods paragraph must be
   traceable to a numbered section of this file. Sections added in
   rev 3–4 are especially important: §4.4 Memphis edge effect (rev 4
   mechanism: TN-MA, not TN-commercial), §6.1 two-cohort enrollment
   design, §6.4 suppression separation, §8.5 three-outcome framework,
   **§8.6 payer-stratified analysis as primary methodology (rev 4)**,
   §9.1 MNAR options.
2. `PAPER_OUTLINE.md` — target journal fit, scope boundary (descriptive
   only), population, tiered case definition rationale.
3. `sas/step3_cohort.sas` — SAS cohort extraction including Part T
   (claim-level Tier 2 temporal matching: L97 + debridement within 0–30
   days forward).
4. `sas/step3b_bin_activity.sas` — per-patient × time-bin activity
   extraction producing half-year and meteorological-seasonal bins.
5. `sas/step3c_enrollment.sas` — enrollment-based denominator
   construction. Pulls monthly enrollment flags from the AR_APCD_24B_MEST
   (Member Enrollment Selection Table) and yearly presence from
   APCD_MCR_BEN_SUM. Exports `cohort_continuous.csv` (continuous
   enrollment 2017–2022) and `cohort_fractional.csv` (fractional
   person-time). **Primary cohort = fractional (rev 6, 2026-05-05).**
   The continuous cohort is reported as the principal sensitivity
   analysis. Rationale: the 2026-04-30 cohort-comparison test showed
   that continuous-enrollment selection systematically excludes the
   highest-risk Arkansas populations (Medicaid-churn, dual-eligibles)
   and hides the Delta-Interior amputation signal. See §8.6 of the
   assumptions log for the empirical comparison.
6. `sas/step5_zip_extract.sas` — ZIP-based geographic assignment and
   final analytic file assembly; cross-payer study_id construction.
7. `R/01_load_and_clean.R` through `R/06_ehsa_prep.R` — the R pipeline
   (exclusions, tier assignment, enrollment-weighted ZCTA aggregation,
   EHSA space-time cube preparation).
8. `R/07_amputation_spatial.R` — parallel static spatial analysis with
   amputation incidence as the outcome.
9. `R/08_amp_among_dfu_spatial.R` — conditional static spatial analysis:
   amputation rate *among* Tier 2 DFU patients.
10. `sas/step3d_payer_strata.sas` — assigns each patient to a primary
    payer category (Medicare / Medicaid / Commercial / Mixed) based on
    enrollment-month dominance (≥80% threshold). Exports
    `payer_strata.csv`.
11. `R/09_payer_stratified_spatial.R` — payer-stratified static spatial
    analysis. Reruns the DFU prevalence, amputation incidence, AND
    conditional amputation analyses for each of four payer strata.
    **This is the primary analytic product** (per rev 4 promotion).
    Controlled by `DFU_PAYER_STRATUM` and `DFU_OUTCOME` env vars.
    Supports `OUTCOME=dfu`, `OUTCOME=amp`, and `OUTCOME=amp_dfu`
    (conditional, Tier 2 DFU-restricted denominator). Total of
    4 strata × 3 outcomes = 12 analytic combinations available.

---

## 2. Required structure (Health & Place house style)

H&P uses a flexible structured-section format. Produce the following
**main sections** for the Methods, with the listed subsections inside
each. The H&P audience tolerates and rewards methodological depth, so
prefer slightly longer subsections with clear rationale over
PCD-style telegraphic prose.

### 2.1 Data and Study Population
1. **Data Source** (AR APCD; Medicare extract verified)
2. **Inclusion and Exclusion Criteria**
3. **Cross-Payer Linkage and Identity-Resolution**

### 2.2 Case Definitions
4. **Diabetes Classification** (T1D, T2D, Ambiguous)
5. **Diabetic Foot Ulcer Tiered Definitions** (Tier 1, Tier 2, Tier 3)
6. **Procedure Codes** (debridement, amputation)

### 2.3 Geographic and Temporal Framework
7. **ZCTA Assignment and Filtering** (incl. Memphis edge effect §4.4)
8. **Time-Bin Structure** (12 half-year bins; sensitivity seasonal)
9. **Study Window** (Option C: 2017–2022)

### 2.4 Denominator Construction (Enrollment-Based, Two-Cohort)
10. **Enrollment-Weighted Person-Halfyears** (MEST + BEN_SUM)
11. **Fractional Cohort (PRIMARY)** — full description
12. **Continuous-Enrollment Cohort (SENSITIVITY)** — full description
13. **Why Fractional is Primary** — selection-bias rationale (§8.6)

### 2.5 Outcomes (Three-Measure Framework)
14. **DFU Coding Prevalence** (script 05)
15. **Amputation Incidence** (script 07)
16. **Conditional Amputation among DFU Patients** (script 08)
17. **Why Three Outcomes** — divergence as finding (§8.5)

### 2.6 Spatial Statistical Methods
18. **Spatial Weights** (adaptive KNN, k=8)
19. **Local Empirical Bayes Smoothing** (Marshall 1991)
20. **Global Moran's I** (999 permutations)
21. **Local Getis-Ord Gi*** with single-test z-bands (§8.2)
22. **LISA** (sensitivity)
23. **Multiple Testing** (FDR-robust subset reported separately)

### 2.7 Payer Stratification (Primary Analytic Product)
24. **Stratification Scheme** (≥80% dominance; Mixed = residual)
25. **Why Stratified** (pooled Moran's I attenuates true signal)
26. **Stratum × Outcome Matrix** (4 strata × 3 outcomes = 12 analyses)

### 2.8 Sensitivity Analyses
27. **Cohort Comparison** (fractional vs continuous; §8.6)
28. **Tier 1 / Tier 3 Outcome Sensitivity**
29. **Memphis-Border Exclusion**
30. **Alternative Spatial Weights**

### 2.9 Software and Reproducibility (REQUIRED for H&P)
H&P expects a reproducibility statement; word budget allows for a
substantive paragraph here (not optional as it would be for PCD).
Include: R/SAS versions, key packages with versions (sf, spdep,
data.table, ggplot2, tigris), public GitHub URL
(https://github.com/wm-watson/Diabetic-Foot-Ulcer), data access
statement (AR APCD via DUA; not publicly redistributable).

---

## 3. Content requirements for each subsection

### 3.1 Data Source and Study Population
- Name the Arkansas All-Payer Claims Database (AR APCD); state the
  commercial window (2017–2024) and Medicare window (2014–2022).
- State the inclusion criterion (≥1 claim with ICD-10-CM E10.x or E11.x
  in any diagnosis position).
- List exclusions **in the CONSORT order** used in the flow diagram:
  missing ZIP → non-Arkansas ZIP → age <18 → cross-payer deduplication
  (prefer Medicare). Report exact N at each step from
  `outputs/descriptives/flow_counts_full.csv` once the pipeline has run.
- Define `study_id = apcd_unique_id || gender` for cross-payer linkage,
  state the fraction that could not be linked, and **note the identity-
  resolution collision limitation** (§6.1): the APCD does not have SSN,
  so individuals who change insurers may receive a new `apcd_unique_id`
  with no linkage to their old record. In the 1% development sample,
  0.02% of member records exhibited inconsistent identity resolution.
  Direction of bias is conservative for hot-spot findings.
- Name the UAMS IRB approval and the AR APCD data use agreement (the
  author will fill in the numbers; leave placeholders `[IRB #___]`).

### 3.2 Diabetes and Diabetic Foot Ulcer Case Definitions
- Three diabetes categories: **T1D**, **T2D**, **Ambiguous** (both E10
  and E11 observed). State that the primary analysis uses T1D + T2D only
  and that ambiguous-DM is held for sensitivity.
- Define the three DFU tiers verbatim from `_assumptions.md` §3.3:
  - **Tier 1:** any L97.x or any DM-ulcer combination code
    (E10.621/622, E11.621/622).
  - **Tier 2 (primary):** L97.x co-occurring with a lower-extremity
    debridement claim **within 0–30 days forward**, or any combination
    code. Cite **Barshes et al. (2021)** for the temporal-match rationale
    and state that the match is performed at the claim level in SAS
    (step3_cohort.sas Part T), not at the patient-aggregate level.
  - **Tier 3:** combination codes only.
- List the debridement CPT codes (97597, 97598, 11042–11047, 97602) and
  the amputation codes (CPT 27590–27598, 28800–28825; ICD-10-PCS 0Y6xxxx
  for Medicare inpatient).
- State that procedures are extracted only for patients already in the
  DM cohort (left-joined in SAS Part P3).

### 3.3 Geographic Unit and Time Bins
- State that patient ZIP is taken from the MEMBER (commercial) or
  BEN_SUM (Medicare) table, mapped to ZCTA via the UDS Mapper 2023
  ZIP-to-ZCTA crosswalk, and filtered to ZIPs beginning with 71 or 72.
- Note that ZCTA is **fixed per patient** across the study period
  (relocation not tracked) and that this is a limitation (§4.1).
- **Describe the Memphis edge effect (§4.4, mechanism revised
  2026-04-28)** explicitly as a data-capture artifact affecting five
  Arkansas counties (Crittenden, Mississippi, Phillips, Lee,
  St. Francis). The dominant mechanism is **TN-licensed Medicare
  Advantage plans** (Humana TN, BCBS-TN MA, Cigna TN MA) in which AR
  border residents enroll via Memphis brokers; these plans do not
  report to the AR APCD. Secondary mechanisms include Memphis VA
  utilization and self-pay/charity care at Memphis safety-net
  providers. AR Medicaid, Medicare FFS, and AR-licensed commercial
  insurance are fully captured regardless of where care is delivered.
  Note explicitly that the **payer-stratified analysis** (§3.6 below;
  §8.6 in the assumptions log) refutes the earlier hypothesis that the
  cold spot was a TN-commercial-insurance artifact: the commercial
  stratum has the highest DFU rate of any region in Border-Memphis
  (9.77/1,000) and zero commercial cold-spot ZCTAs there. State that
  the five counties are retained in the primary analysis and handled
  via a sensitivity-exclusion analysis (§3.7 below).
- Describe the **Option C window**: the primary analysis pools
  commercial and Medicare over **2017–2022** to maximize payer coverage
  while satisfying the ArcGIS EHSA minimum of 10 time slices.
- Describe the primary time-bin scheme: **12 half-year bins (H1 =
  Jan–Jun; H2 = Jul–Dec) across 2017–2022**.
- Describe the sensitivity bin scheme: **24 meteorological seasonal bins
  (W = Dec–Feb, S = Mar–May, U = Jun–Aug, A = Sep–Nov)** with December
  rolled into the next year's winter bin.
- Cite `step3b_bin_activity.sas` as the source of the per-patient × bin
  activity flags.

### 3.4 Enrollment-Based Denominators and Cohort Definition *(rev 6, 2026-05-05 — cohort priority flipped)*

This subsection replaces the prior "claims-observed denominator"
language. The prior approach (count of distinct DM patients with any
claim in a bin) is biased against high-churn populations and deflates
rates in rural/Medicaid-heavy ZCTAs. State the following:

- Denominators are constructed from the **Member Enrollment Selection
  Table (AR_APCD_24B_MEST)**, which provides monthly enrollment flags
  per person × payer × year, supplemented with **APCD_MCR_BEN_SUM**
  year-level records for Medicare FFS beneficiaries.
- Enrollment is merged by **bitwise OR across medical payer types** for
  each person-year (COM, MCD, MCR_ADV, QHP, HCIP, EBD, PASSE, MCD_QHP);
  dental-only and pharmacy-only payer types are excluded. A patient
  with Commercial January–June and Medicaid July–December is credited
  with 12/12 months.
- Medicare FFS beneficiaries (not in MEST) are credited with 12/12
  months in any year their BEN_SUM record is present. Cite §6.1 for
  justification.
- **Two cohorts are constructed (rev 6, 2026-05-05 — priority
  flipped):**
  - **Cohort A (PRIMARY): Fractional person-time.** Any patient with
    ≥1 month of medical enrollment 2017–2022 contributes
    `enrolled_months/6` per bin, with a per-bin floor of ≥3 of 6
    months (50% of the bin). Below the floor, the bin contributes
    zero (rationale: more unobserved than observed). Cohort size:
    313,177 patients. The 50% floor is analogous to HEDIS's 11/12
    annual continuous enrollment rule, scaled to the 6-month bin
    structure.
  - **Cohort B (SENSITIVITY): Continuously enrolled.** All 72 months
    of medical enrollment 2017–2022 (merged across payers). Patients
    contribute 1.0 person-halfyear per bin. Cohort size: 190,943
    patients (~41% smaller than fractional).
- **Why fractional is primary** (state explicitly in Methods):
  empirical comparison (2026-04-30) demonstrated that the
  continuous-enrollment cohort systematically excludes
  high-churn populations — the very Medicaid, dual-eligible, and
  payer-transitioning patients most affected by Arkansas's
  diabetic foot ulcer burden. The fractional cohort produced
  pooled Moran's I of 0.27 (DFU prevalence) and 0.44 (amputation
  incidence) versus 0.18 and 0.30 in continuous, and revealed a
  Delta-Interior amputation hot spot (3 cold / 10 hot ZCTAs)
  that was ambiguous in continuous (14 / 14). Selection-bias
  rationale rather than convention drives the cohort choice.
- Numerator is cohort-dependent: a patient contributes a case to the
  bin containing their first Tier-2-qualifying DFU (or first amputation
  for scripts 07/08). Numerator-date establishes enrollment at that
  instant, so no additional enrollment check is required.
- **Small-cell suppression (§6.4):** the CDC small-cell display rule
  (numerator <11 OR denominator <20) is applied to displayed rate
  labels and cell-level tables only. The spatial *analysis*
  (Gi*/LISA, EHSA) runs on all ZCTAs with a valid denominator
  (dm_denom ≥ 20 person-halfyears) so that low-DFU ZCTAs are included
  in cluster detection. This follows standard spatial-epi practice
  (Waller & Gotway 2004) and allows the local EB smoothing to
  stabilize small-N estimates.
- **DFU-aware amputation censoring (§5.3):** patients are removed from
  numerator and denominator in bins strictly after their first
  amputation **only when** the first amputation occurred on or after
  the first DFU date. Historical amputations preceding any DFU claim
  are treated as unrelated and do not cause censoring.

### 3.5 Outcome Measures *(new subsection, 2026-04-21)*

State that three complementary spatial outcomes are analyzed (§8.5):

1. **DFU prevalence** (script 05): cases per 1,000 DM person-halfyears.
   Sensitive to ICD coding variation and care engagement. Primary
   interpretation is "where is DFU being identified and coded."
2. **Amputation incidence in all DM** (script 07): first lower-extremity
   amputation per 1,000 DM person-halfyears. Amputation is a forced
   clinical event (unlike L97 coding), so this outcome is less sensitive
   to coding variation. Primary interpretation is "where is limb loss
   happening."
3. **Conditional amputation rate** (script 08): first amputation per
   1,000 DFU person-halfyears, restricted to the Tier 2 DFU cohort.
   Isolates the DFU→amputation progression gradient from baseline DFU
   prevalence. Primary interpretation is "given recognized disease,
   where is progression to amputation highest — i.e., where is
   healthcare access failing patients who are already in the system."

State that the divergence between these three outcomes *is* a finding
of the paper: mapping DFU prevalence alone is insufficient for public
health intervention targeting in Arkansas.

**Key interpretive findings for the paper to highlight (rev 4,
updated 2026-04-28 with conditional payer-stratified results):**

- **Ozark protective signal is robust across all four payer strata**
  for the conditional amputation outcome. Mixed-payer Ozarks shows
  54 cold vs 0 hot ZCTAs (71% of Ozark Mixed-payer ZCTAs are cold
  spots). Even patients with the most fragmented coverage rarely
  progress to amputation in the Ozarks. The "hot DFU prevalence"
  pattern in Medicare reflects retiree chronic-coding intensity, not
  worse disease — confirmed by uniformly low amputation progression
  in the same population. Possible mechanisms: regional wound-care
  provider density, primary-care continuity, lower disease severity
  at presentation. Worth dedicated investigation.

- **Arkansas River Valley** (Pope, Johnson, Yell, Logan, Howard
  counties) — robust progression hot spot. 17 hot vs 2 cold ZCTAs in
  the Medicare conditional amputation stratum (script 09 amp_dfu),
  EB rate 22.0/1,000 DFU PHY (highest of any region in Medicare).
  Robust to payer stratification.

- **Southwestern Arkansas / Texarkana corridor (NEW finding)** —
  Howard, Sevier, Little River, and Hempstead counties show the
  highest DFU → amputation progression rates in the state in the
  Mixed-payer conditional analysis (EB rates 27–42 per 1,000 DFU
  PHY). Howard County also appears in the top 10 hot ZCTAs of the
  Medicare amp_dfu stratum, supporting that the signal is not purely
  a payer-mix artifact. **This region was not flagged by any of the
  pooled or single-payer DFU-prevalence or amputation-incidence
  analyses.** It surfaces only in the conditional + payer-stratified
  framework. Tied with River Valley as a top-priority intervention
  target. Methods section should explicitly state that this finding
  emerged from the conditional payer-stratified analysis and would
  have been missed by conventional descriptive mapping.

- **Delta-Interior amputation hot spot in the Mixed-payer stratum**
  (16 hot vs 7 cold ZCTAs in conditional amp) — patients with payer
  churn (dual-eligibles, Medicaid-expansion churners) are spatially
  concentrated in the Delta-Interior and have the worst amputation
  outcomes. Together with the Texarkana finding, this is one of two
  highest-priority intervention findings in the paper.

- **Memphis-border "cold spot" is a Medicare/Medicaid DFU-prevalence
  data-capture artifact (§3.3), not evidence of low disease burden.**
  The commercial stratum shows the highest DFU rate of any AR region
  in those counties (9.77/1,000), and the conditional amputation
  analysis shows that among captured DFU patients in Border-Memphis,
  Medicare progression is ELEVATED (8 hot vs 5 cold ZCTAs, EB rate
  19.2/1,000 DFU PHY — well above state average). The captured
  patients are progressing to amputation at high rates; the missing
  patients (TN-MA enrollees) likely have similar or worse outcomes.
  The Methods section should explicitly distinguish "low captured
  prevalence" from "low actual disease burden" for this region.

### 3.6 Statistical Analysis
- **Spatial weights:** adaptive k-nearest-neighbor with k = 8,
  row-standardized. Justify the KNN choice over queen contiguity by
  noting that many rural AR ZCTAs have few or no shared boundaries.
- **Rate stabilization via local Empirical Bayes smoothing** (Marshall
  1991, `spdep::EBlocal`). Each ZCTA's rate is shrunk toward its KNN-8
  neighborhood mean proportional to its denominator-driven
  unreliability. Standard practice for small-area disease mapping;
  preserves spatial heterogeneity while denoising Poisson noise in
  small-denominator rural ZCTAs. Justify by reporting the raw rate
  range and the post-smoothing range from script output. In sparse
  payer strata where EB produces NA in zero-event neighborhoods, the
  pipeline falls back to raw rates (`spdep::EBlocal` guard in
  `R/09_payer_stratified_spatial.R`).
- **Payer stratification as primary methodology (rev 4 — §8.6).**
  Each patient is assigned to a primary payer category based on
  ≥80% enrollment-month dominance: MEDICARE, MEDICAID, COMMERCIAL, or
  MIXED (no single payer ≥80%). The primary spatial analyses
  (DFU prevalence, amputation incidence, conditional amputation) are
  run within each stratum. **The pooled cross-payer analysis is
  retained only as a baseline reference; it systematically attenuates
  the spatial signal because it averages across payer-specific
  gradients that point in different directions** (pooled DFU Moran's
  I = 0.18 vs Medicare-only Moran's I = 0.72). State this finding
  explicitly in the Methods.
- **Global spatial autocorrelation:** Global Moran's I with 999
  permutations on the EB-smoothed rate surface. Report values for
  pooled and for each payer stratum (table format, three outcomes ×
  five strata).
- **Local clustering (static):** Local Getis-Ord Gi* with 999
  permutations. Display maps use **single-test z-score thresholds**
  (ArcGIS Pro / Anselin convention; §8.2) with significance bands at
  90% (±1.65), 95% (±1.96), and 99% (±2.58). Combining FDR-adjusted
  q-values with these bands creates a spurious gap in the gradient
  that leaves only the most extreme ZCTAs visible; FDR-robust subsets
  are therefore reported as a separate boolean flag and shown in a
  supplementary figure.
- **LISA (Local Moran's I):** run as a sensitivity with identical
  weights, single-test p-values for display, and quadrant assignment
  (High-High, Low-Low, High-Low, Low-High).
- **Emerging Hot Spot Analysis:** ArcGIS Pro 3.x Space Time Cube
  (NetCDF) built from the ZCTA polygon layer and the half-year rate
  panel, with a 6-month time step and a 1-step neighborhood time lag.
  The Mann-Kendall trend test classifies each location into one of the
  17 EHSA categories (8 hot, 8 cold, no pattern).
- Cite Harris et al. (2017) and the ArcGIS documentation for the EHSA
  trend classification rules.

### 3.7 Sensitivity Analyses
Enumerate, in order, the sensitivities from `_assumptions.md` §9.
**Note (rev 4):** Payer stratification is no longer a sensitivity — it
is the primary methodology (§3.6). The list now contains:

1. Include ambiguous-DM in Tiers 1 and 2 and re-run Gi* and EHSA;
   report hot-spot concordance.
2. Compare Tier 1 and Tier 3 hot-spot patterns against Tier 2 primary.
3. Stricter patient-level Tier 2b (first debridement within 365 days of
   first DFU).
4. Alternative spatial weights (queen contiguity with island fill; KNN
   k = 6 and k = 10).
5. Raw rates vs EB-smoothed rates for visual stability.
6. **Fractional-cohort sensitivity** — rerun scripts 05/07/08/09 with
   `DFU_COHORT=fractional` to verify hot spot locations are stable
   across the continuous-enrollment inclusion criterion.
7. **Memphis-border exclusion sensitivity** (§4.4) — rerun all three
   spatial analyses with the five Memphis-adjacent counties excluded.
   Stability of hot spots outside those counties establishes robustness
   to the MNAR data-capture artifact.

State that formal MNAR adjustment (TN-Medicare-Advantage penetration
correction from CMS contract enrollment files, spatial regression with
Memphis-distance covariate, pattern-mixture model) is deferred to
Paper 2.

---

## 4. Writing rules

- **Voice:** third-person, past tense, active voice where possible
  ("We defined…", "Patients were excluded if…").
- **Word budget (H&P, rev 6):** target **2,000–2,500 words for the
  full Methods section**. H&P typically allows up to 7,000 words for
  the full manuscript including all sections; Methods proportional
  share is generous. The earlier 900–1,100 PCD target is obsolete —
  H&P readers expect more methodological detail than PCD readers do.
  Within Methods, allocate roughly:
  - §2.1 Data and Population: 250 words
  - §2.2 Case Definitions: 350 words (Tier 2 temporal + Barshes)
  - §2.3 Geographic and Temporal: 350 words (incl. Memphis edge)
  - §2.4 Denominator Construction: 400 words (the methodologically
    novel part; deserves the most ink)
  - §2.5 Outcomes: 250 words (the three-outcome rationale)
  - §2.6 Spatial Statistical Methods: 350 words
  - §2.7 Payer Stratification: 250 words
  - §2.8 Sensitivities: 150 words
  - §2.9 Software / Reproducibility: 100 words
- **Numbers:** use the exact counts produced by
  `outputs/descriptives/flow_counts_full.csv`,
  `D:\WPWatson\enrollment_summary.txt` (copied to Dropbox), and
  the Gi*/LISA tables in `outputs/static_spatial/`. If those files do
  not yet exist when you draft, insert the placeholder `[N from
  flow_counts]` and the author will fill in.
- **Citations (minimum set to include for H&P, rev 6):**
  - **Barshes NR**, Minc SD. "Healthcare disparities in vascular
    surgery…" (Tier 2 temporal definition rationale).
  - **Getis A, Ord JK.** "The analysis of spatial association by use of
    distance statistics" (Gi*).
  - **Anselin L.** "Local indicators of spatial association — LISA"
    (LISA).
  - **Marshall RJ** 1991. "Mapping disease and mortality rates using
    empirical Bayes estimators" (local EB smoothing).
  - **Waller LA, Gotway CA** 2004. *Applied Spatial Statistics for
    Public Health Data* (small-area disease mapping; analytic vs
    display suppression).
  - **Harris NL et al.** or ArcGIS Pro documentation (EHSA
    classification).
  - **MAUP-relevant citation** (Openshaw 1984 or similar) — H&P
    reviewers expect explicit acknowledgment of the modifiable areal
    unit problem when ZCTAs are the chosen unit. Note that we held
    ZCTA as the analytic unit and discussed county-level aggregation
    as a deferred sensitivity (§8.6 of assumptions).
  - **Diez Roux AV** 2001 ("Investigating neighborhood and area
    effects on health") — canonical H&P-friendly reference for
    place-based health analysis.
  - **Krieger N, Williams DR, Moss NE** 1997 ("Measuring social
    class in US public health research") — for the payer-as-proxy-
    for-SES interpretation in §2.7.
  - **Barshes-style amputation rate citations** for AR Delta context
    (regional disparities in DFU outcomes).
  - **CDC small-cell suppression guidance** (for the numerator <11 /
    denominator <20 threshold; this is the standard CDC privacy rule
    used by surveillance journals broadly, not specific to PCD).
  - **UDS Mapper** ZIP-to-ZCTA crosswalk (2023 vintage).
- **Do not** reproduce verbatim text from `_assumptions.md`; paraphrase.
- **Do not** introduce new analytic decisions. If something is missing
  from the assumptions log, stop and flag it in `METHODS_GAPS.md`
  rather than inventing a decision.
- **Tier terminology:** always write "Tier 1", "Tier 2", "Tier 2b",
  "Tier 3" with a capital T. Never abbreviate to T1/T2 (those refer to
  diabetes type in this paper).
- **Cohort terminology (rev 6, 2026-05-05):** always write "fractional
  cohort" and "continuous cohort" (lowercase c); always state that
  **fractional is the primary analysis** and continuous is the
  principal sensitivity. The 2026-04-30 cohort comparison is the
  empirical justification — cite §8.6 of `_assumptions.md` and
  describe the comparison briefly in the Methods.
- **Outcome terminology:** always distinguish:
  - "DFU prevalence" or "DFU coding prevalence"
  - "amputation incidence" (when denominator is all DM)
  - "DFU-to-amputation progression rate" or "conditional amputation
    rate" (when denominator is restricted to DFU patients)
- **Payer-stratum terminology** (rev 4): always write "Medicare
  stratum", "Medicaid stratum", "Commercial stratum", and "Mixed-payer
  stratum". The MIXED stratum is *not* a residual category — it is
  defined as patients with no single payer accounting for ≥80% of
  enrollment months. Treat MIXED as substantively meaningful (high-
  churn population). The pooled (cross-payer) analysis is the
  "baseline" or "reference" analysis, never the "primary" analysis.

---

## 5. Deliverables

Produce these files in the repo root:

1. `paper/methods.md` — the drafted Methods section.
2. `paper/methods_table_of_citations.md` — one row per citation with
   (key, author/year, relevance, target subsection).
3. `METHODS_GAPS.md` — any decisions you could not find in the
   assumptions log, listed one per line with a suggested question for
   the author.

---

## 6. Quality check before returning

Before returning the Methods draft, verify each of the following and
include the checklist (with checks) at the bottom of `paper/methods.md`:

- [ ] Every claim is traceable to a section in `R/_assumptions.md`,
      SAS code comment, or R code comment.
- [ ] Ambiguous-DM is treated as a sensitivity analysis, not the
      primary population.
- [ ] Tier 2 primary definition uses the **claim-level 30-day forward
      window** computed in SAS (not the 365-day patient-level
      approximation, which is Tier 2b sensitivity).
- [ ] Denominator is described as **enrollment-weighted** (MEST +
      BEN_SUM), not claims-observed.
- [ ] **Fractional cohort is named as primary** (rev 6); continuous
      cohort as principal sensitivity. The empirical justification
      (Δ Moran's I, Delta-Interior signal recovery) is stated in
      §3.4.
- [ ] **All three outcomes** (DFU prevalence, amputation incidence,
      conditional amputation) are described in §3.5.
- [ ] **Local EB smoothing** is described in §3.6 with rationale.
- [ ] **Gi*/LISA display uses single-test z-bands**; FDR reported
      separately.
- [ ] Suppression rule is stated as **numerator <11 OR denominator <20
      for display**; analytic threshold is **denominator ≥20 only**.
- [ ] Window strategy is stated as **Option C: 2017–2022 combined
      primary**.
- [ ] **Payer stratification is stated as PRIMARY methodology** (rev 4),
      not a sensitivity. Four strata: Medicare, Medicaid, Commercial,
      Mixed.
- [ ] **Pooled cross-payer analysis is described as the baseline /
      reference, not primary** — and the Methods explicitly notes that
      pooled Moran's I (0.18) understates the true spatial signal
      relative to stratum-specific Moran's I (Medicare 0.72).
- [ ] **Memphis edge mechanism is stated correctly (rev 4):**
      TN-licensed Medicare Advantage is the dominant mechanism, not
      TN-licensed commercial insurance. The commercial stratum has the
      *highest* DFU rate of any region in Border-Memphis.
- [ ] **Mixed-payer Delta-Interior amputation finding** is named as
      the highest-priority intervention finding of the paper.
- [ ] Primary EHSA bins are stated as **12 half-year (H1/H2) slices**
      with 24 seasonal bins as sensitivity.
- [ ] DFU-aware amputation censoring rule is stated correctly (only
      censor when amputation occurred on or after first DFU).
- [ ] **Memphis edge effect is named and described** as an MNAR
      data-capture artifact with a sensitivity-exclusion plan.
- [ ] **Identity-resolution collision** limitation is named with the
      APCD-lacks-SSN rationale.
- [ ] **Health & Place target verified (rev 6, 2026-05-05).** No
      surviving references to PCD, GIS Snapshot format, or the
      May 2026 deadline. Word budget target is 2,000–2,500 (Methods)
      not 900–1,100.
- [ ] **MAUP acknowledged** (Openshaw 1984 or similar). H&P reviewers
      expect explicit treatment of the modifiable areal unit problem.
- [ ] **Place-based health framing** in Introduction (Diez Roux 2001
      or similar) — H&P-style framing emphasizes "place" as a causal
      construct, not just a unit of analysis.
- [ ] **Reproducibility paragraph** included (§2.9): GitHub URL,
      key R/SAS package versions, AR APCD DUA reference.
- [ ] Word count is within the target band.
- [ ] All citations listed in §4 appear in the draft.
- [ ] No verbatim copy from `_assumptions.md`.
- [ ] No invented decisions; every new decision flagged in
      `METHODS_GAPS.md`.
