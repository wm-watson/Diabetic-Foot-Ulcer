# Instructions for an LLM: Draft the Methods Section

**Purpose.** Convert the structured decisions in this repository into a
publication-ready Methods section for *Preventing Chronic Disease* (PCD)
"Original Research" or "GIS Snapshot" format, targeting the **January 2026
Call for Papers: Geospatial Perspectives on the Intersection of Chronic
Disease, Risk Factors, and Health Outcomes** (deadline 2026-05-01).

**Last updated: 2026-04-21** — incorporates the three-outcome framework,
enrollment-based two-cohort design, Memphis edge effect MNAR documentation,
local EB smoothing, and analytic-vs-display suppression separation.

---

## 1. Inputs the LLM must read before drafting

Read all of the following, in this order, and do not begin drafting until
all have been ingested:

1. `R/_assumptions.md` — the authoritative methodological decisions log.
   **This is the primary source.** Every Methods paragraph must be
   traceable to a numbered section of this file. Sections added in rev 3
   (2026-04-21) are especially important: §4.4 Memphis edge effect, §6.1
   two-cohort enrollment design, §6.4 suppression separation, §8.5 three-
   outcome framework, §9.1 MNAR options.
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
   person-time). **Primary cohort = continuous.**
6. `sas/step5_zip_extract.sas` — ZIP-based geographic assignment and
   final analytic file assembly; cross-payer study_id construction.
7. `R/01_load_and_clean.R` through `R/06_ehsa_prep.R` — the R pipeline
   (exclusions, tier assignment, enrollment-weighted ZCTA aggregation,
   EHSA space-time cube preparation).
8. `R/07_amputation_spatial.R` — parallel static spatial analysis with
   amputation incidence as the outcome.
9. `R/08_amp_among_dfu_spatial.R` — conditional static spatial analysis:
   amputation rate *among* Tier 2 DFU patients.

---

## 2. Required structure (PCD house style)

Produce seven subsections **in this order** and under **these exact
headings**:

1. **Data Source and Study Population**
2. **Diabetes and Diabetic Foot Ulcer Case Definitions**
3. **Geographic Unit and Time Bins**
4. **Enrollment-Based Denominators and Cohort Definition**
5. **Outcome Measures** *(new, 2026-04-21)*
6. **Statistical Analysis**
7. **Sensitivity Analyses**

An optional 8th subsection — **Software and Reproducibility** — should be
added only if the final word count allows.

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
- **Describe the Memphis edge effect (§4.4)** explicitly as a data-
  capture artifact affecting five Arkansas counties (Crittenden,
  Mississippi, Phillips, Lee, St. Francis). The mechanism is that
  AR residents employed by Memphis-based organizations carry
  TN-licensed commercial insurance that does not flow to the AR APCD.
  AR Medicaid and Medicare FFS are fully captured regardless of where
  care is delivered. State that the five counties are retained in the
  primary analysis and handled via a sensitivity-exclusion analysis
  (§7 below).
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

### 3.4 Enrollment-Based Denominators and Cohort Definition *(rewritten 2026-04-21)*

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
- **Two cohorts are constructed:**
  - **Cohort 1 (PRIMARY): Continuously enrolled.** All 72 months of
    medical enrollment 2017–2022 (merged across payers). Patients
    contribute 1.0 person-halfyear per bin. Cohort size from
    `enrollment_summary.txt`.
  - **Cohort 2 (SENSITIVITY): Fractional person-time.** Any patient
    with ≥1 month of medical enrollment; contributes `enrolled_months/6`
    per bin with a per-bin floor of ≥3 of 6 months (50% of the bin).
- Numerator is cohort-dependent: a patient contributes a case to the
  bin containing their first Tier-2-qualifying DFU (or first amputation
  for scripts 07/08). Numerator-date establishes enrollment at that
  instant, so no additional enrollment check is required.
- **Small-cell suppression (§6.4):** the PCD display rule (numerator
  <11 OR denominator <20) is applied to displayed rate labels and cell-
  level tables only. The spatial *analysis* (Gi*/LISA, EHSA) runs on
  all ZCTAs with a valid denominator (dm_denom ≥ 20 person-halfyears)
  so that low-DFU ZCTAs are included in cluster detection. This
  follows standard spatial-epi practice (Waller & Gotway 2004) and
  allows the local EB smoothing to stabilize small-N estimates.
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
  range (e.g., 6 to 284 per 1,000) and the post-smoothing range
  (e.g., 18 to 109 per 1,000) from script 05 output.
- **Global spatial autocorrelation:** Global Moran's I with 999
  permutations on the pooled 2017–2022 EB-smoothed rate surface.
  Report the three values from scripts 05, 07, and 08.
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
Enumerate, in order, the sensitivities from `_assumptions.md` §9:
1. Include ambiguous-DM in Tiers 1 and 2 and re-run Gi* and EHSA;
   report hot-spot concordance.
2. Compare Tier 1 and Tier 3 hot-spot patterns against Tier 2 primary.
3. Stricter patient-level Tier 2b (first debridement within 365 days of
   first DFU).
4. Payer-stratified EHSA (commercial 2017–2022 and Medicare 2014–2022
   separately) to verify that the pooled hot-spot pattern is not driven
   by payer mix.
5. Alternative spatial weights (queen contiguity with island fill; KNN
   k = 6 and k = 10).
6. Raw rates vs EB-smoothed rates for visual stability.
7. **Fractional-cohort sensitivity** — rerun scripts 05/07/08 with
   `DFU_COHORT=fractional` to verify hot spot locations are stable
   across the continuous-enrollment inclusion criterion.
8. **Memphis-border exclusion sensitivity** (§4.4, §9) — rerun all
   three spatial analyses with the five Memphis-adjacent counties
   excluded. Stability of hot spots outside those counties
   establishes robustness of the main findings to the MNAR data-
   capture artifact.

State that formal MNAR adjustment (LODES commuter correction, spatial
regression with Memphis-distance covariate, pattern-mixture model) is
deferred to Paper 2.

---

## 4. Writing rules

- **Voice:** third-person, past tense, active voice where possible
  ("We defined…", "Patients were excluded if…").
- **Word budget:** target 900–1,100 words for the full Methods section
  (PCD Original Research). If the format is GIS Snapshot, target 500
  words and collapse subsections 3.4 and 3.7 into one paragraph each.
- **Numbers:** use the exact counts produced by
  `outputs/descriptives/flow_counts_full.csv`,
  `D:\WPWatson\enrollment_summary.txt` (copied to Dropbox), and
  the Gi*/LISA tables in `outputs/static_spatial/`. If those files do
  not yet exist when you draft, insert the placeholder `[N from
  flow_counts]` and the author will fill in.
- **Citations (minimum set to include):**
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
  - **CDC/PCD author guidelines** (for the suppression threshold).
  - **UDS Mapper** ZIP-to-ZCTA crosswalk (2023 vintage).
- **Do not** reproduce verbatim text from `_assumptions.md`; paraphrase.
- **Do not** introduce new analytic decisions. If something is missing
  from the assumptions log, stop and flag it in `METHODS_GAPS.md`
  rather than inventing a decision.
- **Tier terminology:** always write "Tier 1", "Tier 2", "Tier 2b",
  "Tier 3" with a capital T. Never abbreviate to T1/T2 (those refer to
  diabetes type in this paper).
- **Cohort terminology:** always write "continuous cohort" and
  "fractional cohort" (lowercase c); always state that continuous is
  the primary analysis.
- **Outcome terminology:** always distinguish:
  - "DFU prevalence" or "DFU coding prevalence"
  - "amputation incidence" (when denominator is all DM)
  - "DFU-to-amputation progression rate" or "conditional amputation
    rate" (when denominator is restricted to DFU patients)

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
- [ ] **Continuous cohort** is named as primary; fractional cohort as
      sensitivity.
- [ ] **All three outcomes** (DFU prevalence, amputation incidence,
      conditional amputation) are described in §3.5.
- [ ] **Local EB smoothing** is described in §3.6 with rationale.
- [ ] **Gi*/LISA display uses single-test z-bands**; FDR reported
      separately.
- [ ] Suppression rule is stated as **numerator <11 OR denominator <20
      for display**; analytic threshold is **denominator ≥20 only**.
- [ ] Window strategy is stated as **Option C: 2017–2022 combined
      primary; payer-stratified sensitivity**.
- [ ] Primary EHSA bins are stated as **12 half-year (H1/H2) slices**
      with 24 seasonal bins as sensitivity.
- [ ] DFU-aware amputation censoring rule is stated correctly (only
      censor when amputation occurred on or after first DFU).
- [ ] **Memphis edge effect is named and described** as an MNAR
      data-capture artifact with a sensitivity-exclusion plan.
- [ ] **Identity-resolution collision** limitation is named with the
      APCD-lacks-SSN rationale.
- [ ] Word count is within the target band.
- [ ] All citations listed in §4 appear in the draft.
- [ ] No verbatim copy from `_assumptions.md`.
- [ ] No invented decisions; every new decision flagged in
      `METHODS_GAPS.md`.
