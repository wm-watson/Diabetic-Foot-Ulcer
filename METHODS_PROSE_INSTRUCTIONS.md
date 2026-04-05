# Instructions for an LLM: Draft the Methods Section

**Purpose.** Convert the structured decisions in this repository into a
publication-ready Methods section for *Preventing Chronic Disease* (PCD)
"Original Research" or "GIS Snapshot" format, targeting the **January 2026
Call for Papers: Geospatial Perspectives on the Intersection of Chronic
Disease, Risk Factors, and Health Outcomes** (deadline 2026-05-01).

---

## 1. Inputs the LLM must read before drafting

Read all of the following, in this order, and do not begin drafting until
all have been ingested:

1. `R/_assumptions.md` — the authoritative methodological decisions log.
   **This is the primary source.** Every Methods paragraph must be
   traceable to a numbered section of this file.
2. `PAPER_OUTLINE.md` — target journal fit, scope boundary (descriptive
   only), population, tiered case definition rationale.
3. `sas/step3_cohort.sas` — SAS cohort extraction including Part T
   (claim-level Tier 2 temporal matching: L97 + debridement within 0–30
   days forward).
4. `sas/step5_zip_extract.sas` — ZIP-based geographic assignment and
   final analytic file assembly; cross-payer study_id construction.
5. `sas/step3b_bin_activity.sas` — per-patient × time-bin activity
   extraction producing half-year and meteorological-seasonal bins.
6. `R/01_load_and_clean.R` through `R/06_ehsa_prep.R` — the R pipeline
   that implements the exclusions, tier assignment, ZCTA aggregation,
   static spatial statistics, and EHSA space-time cube preparation.

---

## 2. Required structure (PCD house style)

Produce six subsections **in this order** and under **these exact
headings**:

1. **Data Source and Study Population**
2. **Diabetes and Diabetic Foot Ulcer Case Definitions**
3. **Geographic Unit and Time Bins**
4. **Denominators, Numerators, and Small-Cell Suppression**
5. **Statistical Analysis**
6. **Sensitivity Analyses**

An optional 7th subsection — **Software and Reproducibility** — should be
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
- Define `study_id = apcd_unique_id || gender` for cross-payer linkage
  and state the fraction that could not be linked.
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

### 3.4 Denominators, Numerators, and Small-Cell Suppression
- **Denominator** (per ZCTA × bin): unique diabetic patients with an
  active DM claim in the bin.
- **Numerator** (per ZCTA × bin): unique patients in the Tier 2 cohort
  who had an active DFU claim (L97 or combination code) in the bin.
- **Rate:** DFU cases per 1,000 active diabetics.
- **DFU-aware amputation censoring**: patients are removed from
  numerator and denominator in bins strictly after their first
  amputation **only when** the first amputation occurred on or after the
  first DFU date. Historical amputations preceding any DFU claim are
  treated as unrelated and do not cause censoring.
- **Small-cell suppression (PCD standard)**: suppress any cell with
  numerator <11 cases **or** denominator <20 diabetics. Report the
  percentage of cells suppressed in each bin scheme from
  `outputs/panels/suppression_report.csv`. State that suppressed cells
  are encoded as missing (not zero) in the ArcGIS space-time cube so
  they do not generate spurious cold spots.

### 3.5 Statistical Analysis
- **Spatial weights:** adaptive k-nearest-neighbor with k = 8,
  row-standardized. Justify the KNN choice over queen contiguity by
  noting that many rural AR ZCTAs have few or no shared boundaries.
- **Global spatial autocorrelation:** Global Moran's I with 999
  permutations on the pooled 2017–2022 rate surface.
- **Local clustering (static):** Local Getis-Ord Gi* with 999
  permutations and Benjamini–Hochberg FDR correction at q < 0.05. Report
  significance bins at 90%, 95%, and 99% hot/cold thresholds.
- **LISA (Local Moran's I):** run as a sensitivity with identical
  weights and FDR correction, with quadrant assignment (High-High,
  Low-Low, High-Low, Low-High).
- **Emerging Hot Spot Analysis:** ArcGIS Pro 3.x Space Time Cube (NetCDF)
  built from the ZCTA polygon layer and the half-year rate panel, with
  a 6-month time step and a 1-step neighborhood time lag. The Mann-Kendall
  trend test classifies each location into one of the 17 EHSA categories
  (8 hot, 8 cold, no pattern).
- Cite Harris et al. (2017) and the ArcGIS documentation for the EHSA
  trend classification rules.

### 3.6 Sensitivity Analyses
Enumerate, in order, exactly the six sensitivities from
`_assumptions.md` §9:
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
6. Empirical Bayes rate smoothing compared to raw rates for visual
   stability.

---

## 4. Writing rules

- **Voice:** third-person, past tense, active voice where possible
  ("We defined…", "Patients were excluded if…").
- **Word budget:** target 900–1,100 words for the full Methods section
  (PCD Original Research). If the format is GIS Snapshot, target 500
  words and drop subsections 3.4 and 3.6 into one paragraph each.
- **Numbers:** use the exact counts produced by
  `outputs/descriptives/flow_counts_full.csv` and
  `outputs/panels/suppression_report.csv`. If those files do not yet
  exist when you draft, insert the placeholder `[N from flow_counts]`
  and the author will fill in.
- **Citations (minimum set to include):**
  - Barshes NR, Minc SD. "Healthcare disparities in vascular surgery…"
    (Tier 2 temporal definition rationale).
  - Getis A, Ord JK. "The analysis of spatial association by use of
    distance statistics" (Gi*).
  - Anselin L. "Local indicators of spatial association — LISA" (LISA).
  - Harris NL et al. or ArcGIS Pro documentation (EHSA classification).
  - CDC/PCD author guidelines (for the suppression threshold).
  - UDS Mapper ZIP-to-ZCTA crosswalk (2023 vintage).
- **Do not** reproduce verbatim text from `_assumptions.md`; paraphrase.
- **Do not** introduce new analytic decisions. If something is missing
  from the assumptions log, stop and flag it in a `METHODS_GAPS.md`
  file rather than inventing a decision.
- **Tier terminology:** always write "Tier 1", "Tier 2", "Tier 2b",
  "Tier 3" with a capital T. Never abbreviate to T1/T2 (those refer to
  diabetes type in this paper).

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
- [ ] Suppression rule is stated as **numerator <11 OR denominator <20**.
- [ ] Window strategy is stated as **Option C: 2017–2022 combined
      primary; payer-stratified sensitivity**.
- [ ] Primary EHSA bins are stated as **12 half-year (H1/H2) slices**
      with 24 seasonal bins as sensitivity.
- [ ] DFU-aware amputation censoring rule is stated correctly (only
      censor when amputation occurred on or after first DFU).
- [ ] Word count is within the target band.
- [ ] All citations listed in §4 appear in the draft.
- [ ] No verbatim copy from `_assumptions.md`.
- [ ] No invented decisions; every new decision flagged in
      `METHODS_GAPS.md`.
