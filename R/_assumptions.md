# Analytic Assumptions and Limitations — DFU Arkansas Geospatial Paper

**Purpose:** Running log of every methodological decision made in the R pipeline.
Every item here must be addressable in the paper's Methods or Limitations section.

Last updated: 2026-04-21 (rev 3: enrollment-based denominator, two-cohort design)

---

## 1. Data Source and Linkage

### 1.1 Input file
- `dm_dfu_analytic.csv` produced by `step5_zip_extract.sas`
- One row per person-source (commercial and Medicare enrollments appear as separate rows)
- 358,251 rows → 348,469 unique patients → 327,435 unique study_ids

### 1.2 Cross-payer deduplication
- **Rule:** When a patient has both commercial and Medicare records (same `study_id`), prefer Medicare.
- **Rationale:** Medicare capture is more complete for the Arkansas diabetic population during the study period; Medicare procedure extraction is more comprehensive (carrier line + outpatient rev + inpatient/SNF PCS).
- **Impact:** 12,477 patients are duplicated across sources; Medicare record retained. Commercial record discarded entirely (we do not merge procedure flags across sources because date fields are already harmonized upstream).
- **Limitation:** Patients with commercial coverage before Medicare eligibility lose commercial claim history in this simplification. A more complex union is possible but is deferred to Paper 2.

### 1.3 Study ID construction
- `study_id = apcd_unique_id || gender` (M/F/U harmonized)
- Unlinked patients (commercial ~11.9%, Medicare <0.1%) retain `study_id = ""` and cannot be cross-source deduplicated. They are kept in their source-specific cohort.

---

## 2. Study Population

### 2.1 Inclusion
- ≥1 claim with ICD-10-CM E10.x (T1D) or E11.x (T2D) as any-position diagnosis
- Commercial: 14 diagnosis fields scanned (mc039, mc041, mc042–mc053)
- Medicare: 25 diagnosis fields scanned across multiple claim tables

### 2.2 Exclusions
1. Missing ZIP (commercial only: 991 patients, 1.0%; Medicare 100% complete via BEN_SUM)
2. Non-Arkansas ZIP (ZIP not starting with 71 or 72): 1,118 commercial + 5,643 Medicare
3. Age <18 at cohort entry (applied in R, not SAS)

### 2.3 Diabetes type classification
- **T1D:** Only E10.x codes observed
- **T2D:** Only E11.x codes observed
- **AMBIGUOUS:** Both E10.x and E11.x codes observed across the patient's claims
- **Primary analysis:** T1D + T2D only (exclude AMBIGUOUS)
- **Sensitivity analysis:** Include AMBIGUOUS in Tiers 1 and 2; report whether hot/cold spot pattern is stable
- **Rationale for excluding ambiguous in primary:** Many ambiguous patients likely reflect coding error rather than true mixed T1/T2 presentation. Including them inflates the denominator with potentially non-diabetic patients. Large ambiguous fraction (commercial 10.8%, Medicare 12.8%) is noted as a data quality limitation.

---

## 3. DFU Case Definitions (Tiered)

### 3.1 ICD-10-CM codes used
- **L97.x:** Non-pressure chronic ulcer of lower limb (any 4th/5th character)
  - NOT diabetes-specific; captures any lower-extremity ulcer on a diabetic patient
- **Combination codes:** E10.621, E10.622, E11.621, E11.622 (DM with foot ulcer)
  - Highest specificity but known to be under-coded

### 3.2 Procedure codes used
- **Debridement CPT:** 97597, 97598 (selective); 11042–11047 (excisional); 97602 (non-selective)
- **Amputation CPT:** 27590–27598 (above-knee, below-knee); 28800–28825 (foot/toe)
- **Amputation ICD-10-PCS:** 0Y6xxxx (detachment of lower extremity) — Medicare inpatient/SNF only

### 3.3 Three tiers
| Tier | Definition | Role |
|---|---|---|
| Tier 1 | Any L97.x OR any combo code | Broadest — upper-bound DFU prevalence |
| Tier 2 | (L97.x AND debridement anywhere in claims) OR combo code | **Primary analysis** — supports clinical activity |
| Tier 3 | Combo code only | Strictest — lower-bound DFU prevalence |

### 3.4 Critical limitation: temporal matching for Tier 2
- Ideal definition per Barshes et al.: L97.x claim with debridement CPT within 30 days after.
- **Current data limitation:** CSV contains only `first_dfu_date` and `first_debride_date` (patient-level earliest). Claim-level temporal matching (any L97 with any debridement within ±30d) is not computable without re-extraction.
- **Approximation used:** Tier 2 = (`ever_l97` ∧ `has_debridement`) ∨ `ever_dm_combo`. Any debridement during the study period is accepted as supportive evidence.
- **Sensitivity:** A stricter Tier 2b applies the constraint that `first_debride_date` falls within [`first_dfu_date`, `first_dfu_date` + 365 days]. Reported in supplementary material.
- **Future enhancement:** A SAS re-extraction joining L97 claims to debridement claims at the claim level on the server would enable the full ±30 day window. Deferred unless reviewers require.
- Document this explicitly in Methods Limitations.

---

## 4. Geographic Unit

### 4.1 ZCTA assignment
- **Rule:** ZIP from MEMBER (commercial) or BEN_SUM (Medicare) → ZCTA via UDS Mapper 2023 ZIP→ZCTA crosswalk
- **Limitation:** ZCTAs are fixed per patient across all years (latest known ZIP). Patient relocation within the study period is not captured.
- **Impact on EHSA:** The space-time cube treats each patient as residing in their final ZCTA for all person-years. If relocation is correlated with DFU (e.g., patients move from Delta to urban areas for care), this biases hot spot estimates toward the destination. Note as limitation; order of magnitude likely small given AR's low inter-county migration rate.

### 4.2 AR filtering
- Only ZIPs beginning with 71 or 72 retained. This excludes border-adjacent patients with TN/MO/OK/TX/LA ZIPs whose care may still occur in AR.

### 4.3 ZCTA boundary file
- TIGER/Line 2020 national ZCTAs via `tigris` package, filtered to AR ZCTAs present in our cohort.
- S2 geometry disabled (`sf_use_s2(FALSE)`) to avoid degenerate-edge errors after state clipping.

### 4.4 Memphis edge effect (rev 3, 2026-04-21)
Five counties in eastern Arkansas lie within roughly 30 miles of
Memphis, Tennessee: **Crittenden, Mississippi, Phillips, Lee, and
St. Francis**. Residents of these counties have appreciably lower
coded DFU and amputation rates in our data than their clinical
profile (high poverty, high T2D prevalence, documented elsewhere as
a high-burden zone) would predict. Raw amputation rate per 1,000
DM person-halfyears is 0.66 in this region versus 0.85 in
Delta-Interior. The depression is driven by the following payer
mechanisms that bypass the AR APCD:

1. **TN-based commercial insurance via Memphis employment.** Many
   border-county residents work in Memphis (FedEx, St. Jude,
   Methodist Le Bonheur, Regional One, municipal/county government).
   Employer-sponsored insurance from TN-licensed carriers (TN BCBS,
   Cigna-TN, United-TN) does not flow to the AR APCD even when the
   insured resides in Arkansas.
2. **TN-licensed Medicare Advantage plans** purchased by AR
   residents through Memphis brokers.
3. **Memphis VA utilization.** Veterans using the Memphis VA Medical
   Center generate federal VA claims that are not in the AR APCD.
4. **Self-pay / charity care** at Memphis safety-net providers
   (Regional One).

Mechanisms that **do not** contribute to the edge effect (common
misconceptions):
- Tennessee Medicaid / TennCare — Medicaid is state-residency-based;
  an AR resident cannot be on TN Medicaid. AR Medicaid claims for
  out-of-state care do flow to the AR APCD.
- AR-licensed commercial insurance covering care at Memphis providers
  — these claims flow to the APCD regardless of where care is
  delivered.
- Medicare FFS at Memphis providers — CMS-reported and included.

**Analytic handling:**
- Flag the five counties as potentially undercaptured in the main
  analysis (limitation in paper).
- Sensitivity analysis: exclude those counties and re-run the spatial
  analysis. If hot spots elsewhere are stable after exclusion, the
  rest of the map is robust.
- Consider a Memphis-distance covariate in the spatial regression
  (Paper 2).

---

## 5. Time Aggregation

### 5.1 Active diabetics per year
- **Definition:** Patient is "active" in year Y if `first_dm_year ≤ Y ≤ last_dm_year`.
- **Limitation:** This assumes continuous coverage between first and last observed DM claim. Gaps (e.g., coverage lapses) are not detected. Standard approach for claims-based surveillance.

### 5.2 Prevalent DFU per year
- **Definition:** Patient contributes to DFU numerator in year Y if `first_dfu_year ≤ Y ≤ last_dfu_year`. Only patients meeting Tier 2 (primary) are counted.
- **Rationale:** Space-time clustering of burden is the target; any active DFU in a year contributes.

### 5.3 Amputation censoring
- After first amputation, the patient exits both the DFU numerator and the DM denominator for all subsequent years.
- **Rationale:** Post-amputation patients are no longer at risk of new DFU on that limb; leaving them in the denominator deflates prevalence estimates.
- **Limitation:** Bilateral amputation vs. unilateral not distinguished in our PCS/CPT extraction. Future refinement possible.

### 5.4 Left censoring
- Commercial data begins 2017; Medicare data runs 2014–2022 (truncated at 2022).
- Patients with DM onset before 2017 (commercial) or before 2014 (Medicare) appear as prevalent at cohort entry with `first_dm_year` = cohort start year. Their true DM duration is unknown.
- **Impact:** "Time from DM to DFU" is biased downward for left-censored patients. The negative values observed (min –2,587 days Medicare) reflect DFU identified before DM in our window — consistent with L97 being recorded before a DM code on a later claim.
- **Analytic decision:** For the descriptive paper, we do not analyze time-to-DFU. EHSA uses year-level panel data which is robust to left censoring on individual duration.

### 5.5 Medicare 2022 truncation — Option C (decided 2026-04-05)
- Medicare data ends 2022. Commercial data runs through 2024.
- **Primary analysis (Option C):** 2017–2022 combined commercial + Medicare
  with 12 half-year bins (H1/H2) as the primary EHSA time resolution.
- **Sensitivity:** payer-stratified EHSA (commercial 2017–2022, Medicare
  2014–2022) to verify hot-spot stability is not driven by payer mix.
- **Second sensitivity:** 24 meteorological-seasonal bins (W/S/U/A) over
  2017–2022 to address Barshes-style seasonality of ulcer presentation.
- **Rationale:** Option C keeps both payers in the primary map for
  population-level generalizability in Arkansas, and the ≥10-time-slice
  ArcGIS EHSA requirement is met with 12 H1/H2 bins.

### 5.6 Bin structure (step3b_bin_activity.sas)
- `bin_activity_commercial.csv` and `bin_activity_medicare.csv` provide
  one row per (patient × year × half × season × season_year) with flags
  had_dm, had_l97, had_debridement, had_amputation, had_combo.
- R aggregates to the primary 12-bin H1/H2 panel by `group_by(year, half)`
  and to the 24-bin seasonal panel by `group_by(season_year, season)`.
  December is assigned to the next-year winter bin (meteorological convention).
- Claim-level temporal matching for Tier 2 remains in SAS Part T; bin
  activity here is used only for time-slice membership, not case definition.

---

## 6. Prevalence Calculation

### 6.1 Denominator — enrollment-based, two-cohort design (rev 3, 2026-04-21)

Prior approach (rev 1/2): claims-observed denominator (count of unique
DM patient_ids with any claim in a bin). Replaced because:

1. **Insurance churn bias.** Patients who lose coverage mid-window drop
   entirely from the denominator, inflating ZCTA rates in high-churn
   (rural, Medicaid-heavy) areas. Arkansas Medicaid expansion/QHP churn
   is substantial.
2. **Healthy-user invisibility.** Well-managed diabetics with few visits
   are undercounted relative to high-utilizers, distorting the ZCTA mix
   in favor of care-seeking populations.
3. **Payer-transition gaps.** A patient switching Medicaid → Commercial
   vanishes from bins during the transition, even though they were
   continuously at risk.

The replacement uses the **Member Enrollment Selection Table (MEST)**,
which provides a 12-character monthly enrollment flag string per person
× payer × year, with an APCD-derived `apcd_unique_id` that links the
same person across carriers.

**Medical payer types included** (can generate medical claims with DX
codes): COM, MCD, MCR_ADV, QHP, HCIP, EBD, PASSE, MCD_QHP. Excluded
(no medical DX): DNT, PBM, MCRAdvPhrm, EBD_PBM, EBD_TPA, EBD_RET.

Within a person × year, enrollment is merged by **bitwise OR** across
medical payer types. A patient with COM Jan–Jun and MCD Jul–Dec is
credited with 12/12 months.

**Two cohorts are constructed** (see `step3c_enrollment.sas`):

| Cohort | Definition | Role |
|---|---|---|
| **Cohort 1 — Continuous (PRIMARY)** | All 72 months of the study window covered, bitwise OR across medical payers. | Main analysis; strongest internal validity. |
| **Cohort 2 — Fractional (SENSITIVITY)** | Any patient with ≥1 month of medical enrollment anywhere in 2017–2022; contributes `enrolled_months/6` person-halfyears to each bin. | Robustness check; retains churn-exposed populations. |

For Cohort 2, a **per-bin floor of ≥3 of 6 months (50%)** is enforced
at the analysis stage. Bins with <3 months contribute zero person-time
to that bin (patient is "more unobserved than observed"), but the
patient still contributes to other bins where they meet the floor.
Rationale: below 50%, estimated rates are dominated by unobserved
exposure time; DFU cases developing in unobserved months produce a
denominator bias not offset by the numerator. The 50% threshold is
analogous to HEDIS's 11/12 annual continuous enrollment rule, adapted
to our 6-month bin structure.

#### Medicare FFS handling
MEST does not cover Medicare FFS (only Medicare Advantage, payer_type
MCR_ADV). For FFS beneficiaries we have `APCD_MCR_BEN_SUM` with
year-level presence only (`bene_enrollmt_ref_yr`). **Assumption:** if a
`bene_id` has any BEN_SUM record for year Y, the beneficiary is
credited with 12/12 months of enrollment in year Y. Justification: FFS
disenrollment mid-year is rare (death or MA switch, either of which
removes them from further claims in the downstream data). Slightly
over-counts, but does so uniformly across ZCTAs — not a spatial bias.

#### Identity-resolution (collision) limitation
The APCD does not have access to Social Security numbers. Identity
resolution across carriers relies on a probabilistic hash
(`apcd_unique_id`). Individuals who change insurers may receive a new
`apcd_unique_id` with no linkage to their old record, appearing as
distinct patients in both numerator and denominator. In the 1%
development sample, 92 of 442,613 member_ids (0.02%) exhibited
inconsistent identity resolution; 26,513 apcd_unique_ids (6.4%) were
correctly resolved across multiple submitter member_ids. The true
collision rate is unobservable but likely highest among Medicaid
expansion enrollees with coverage churn. **Impact:** this slightly
inflates ZCTA-level denominators in high-churn areas, producing
*conservative (downward-biased)* prevalence estimates in exactly the
areas where DFU burden is expected to be highest. Any hot spots
identified in those areas are therefore robust to this bias.

#### Numerator handling (both cohorts)
A DFU patient counts as 1 case in the bin where their first
Tier-2-qualifying service date falls, regardless of total enrolled
months. The service date itself establishes they were enrolled at that
moment. Rate = cases / person-time, which is the standard
epidemiological formulation.

### 6.2 Numerator
- Prevalent DFU patients per ZCTA-bin meeting Tier 2 (§5.2).

### 6.3 Rate
- **Cases per 1,000 diabetics** (directly age-adjusted only if N permits at ZCTA level — likely not; report crude with age distribution as covariate for Paper 2).
- **Local Empirical Bayes smoothing (Marshall 1991)** applied to pooled
  ZCTA rates before Gi*/LISA. Raw rates from small-denominator ZCTAs (some
  rural ZCTAs have <100 person-halfyears) are dominated by Poisson noise:
  the raw-rate range was 14–284 per 1,000 with the top decile entirely
  composed of ZCTAs with 67–350 person-halfyears, generating spurious
  variance that drowned out the true urban/rural pattern (raw Moran's I =
  0.036, p = 0.04). Local EB shrinks each ZCTA toward its KNN-8
  neighborhood mean by an amount proportional to its denominator
  unreliability, preserving spatial heterogeneity while denoising small-N
  estimates. Local EB rate range: 18–109 per 1,000; Moran's I = 0.279,
  p = 0.001. The crude prevalence map (Figure 1) still shows the raw rate
  for transparency; Gi* and LISA (Figures 2–3) are computed on the
  EB-smoothed rate.
- **Why local rather than global EB:** Global EB pulls all ZCTAs toward the
  statewide mean and over-smooths to the point that no local Gi* clusters
  survive FDR (tested 2026-04-11). Local EB preserves the gradient that
  makes hot/cold spot detection meaningful.

### 6.4 Small-cell suppression (PCD standard)
- **Numerator threshold:** DFU cases <11 per ZCTA-bin suppressed.
- **Denominator threshold:** DM denominator <20 per ZCTA-bin suppressed.
- **Rule:** A ZCTA-bin cell is suppressed if EITHER condition holds.
- **Scope:** Applied to choropleth maps and descriptive count/rate tables
  only. EHSA input is **unsuppressed** — the space-time cube receives the
  full ZCTA × bin panel (including small cells) so that the Mann-Kendall
  trend test has a complete time series. Suppression is a display/privacy
  concern, not an analytic one; EHSA output maps will suppress any ZCTA
  whose underlying cell would have been flagged.
- **Rationale:** PCD requires suppression of small cells to protect
  confidentiality and statistical validity of rate estimates. Rates per
  1,000 partially mask small counts but do not eliminate the concern.
- **If reviewers request stricter suppression at R&R:** thresholds are
  parameterized in `R/04_zcta_aggregation.R` and can be tightened without
  re-running the SAS pipeline.

---

## 7. Demographic Harmonization

### 7.1 Age
- Commercial: `me014_curr_age` (age at most recent MEMBER record)
- Medicare: `age_at_end_ref_yr` (age at end of reference year)
- **Harmonized field:** `age` used as-is; categorized into 18–44, 45–64, 65–74, 75+
- **Limitation:** Ages are not aligned to the same reference date across payers.

### 7.2 Sex
- Commercial: me028 (M/F)
- Medicare: sex_ident_cd (1=M, 2=F, else U) — harmonized to M/F/U in step5.
- Unknown/other categories combined into "U" for Table 1.

### 7.3 Race
- Commercial: NOT COLLECTED. Reported as "Not available" in Table 1.
- Medicare: `bene_race_cd` and `rti_race_cd` (RTI-imputed race).
- **Decision:** Report race only for Medicare subpopulation using RTI race (more accurate than CMS race per Bonito et al. 2008). Commercial race treated as structural missingness, not imputed.

---

## 8. Statistical Methods

### 8.1 Global spatial autocorrelation
- Moran's I with row-standardized spatial weights (adaptive KNN, k = 8).
- 999 permutations for pseudo p-value.

### 8.2 Local clustering — static
- Local Getis-Ord Gi* with adaptive KNN k = 8.
- Significance bins: 90% (±1.65), 95% (±1.96), 99% (±2.58) using
  `localG_perm` with 999 permutations. **Significance bands use
  single-test z-score thresholds for display (ArcGIS Pro / Anselin
  convention).** Attempting to combine FDR-adjusted q-values with the
  z-bands creates a spurious gap in the gradient: only z >= 2.58
  survives, producing an artificially clean "Hot 99%" map with no
  intermediate bands (tested 2026-04-21). The FDR-robust subset is
  retained as a separate boolean flag (`gi_fdr_robust`) on the sf
  object; a supplementary figure will show only those ZCTAs that
  survive BH-FDR q < 0.05 across the 314-test family.
- LISA (Local Moran's I) as sensitivity with same weights and same
  display/FDR logic.
- **Why KNN not contiguity:** Many AR ZCTAs are large and rural with few or no shared boundaries; contiguity weights leave islands. KNN ensures every ZCTA has neighbors.

### 8.3 Emerging Hot Spot Analysis (EHSA)
- ArcGIS Pro Space-Time Cube (NetCDF), ZCTA polygon × 1-year bins.
- Neighborhood: 8 nearest neighbors.
- Time step: 1 year, neighborhood time lag: 1 step.
- Mann-Kendall trend test on Gi* z-scores per location.
- 8 hot categories + 8 cold categories + no pattern.

### 8.4 Multiple testing
- Gi* and LISA use permutation-based pseudo p-values (999 permutations).
- Benjamini-Hochberg FDR correction is computed across all ZCTAs in
  each analysis and stored as `gi_p_fdr` / `lisa_p_fdr`. Display maps
  use single-test z-bands for the reasons noted in §8.2; FDR-robust
  ZCTAs are marked via `gi_fdr_robust` / `lisa_fdr_robust` booleans
  and highlighted in supplementary material.

---

## 9. Sensitivity Analyses Planned

1. Include ambiguous-DM (§2.3) in Tiers 1 and 2 → re-run Gi* and EHSA, compare hot spot stability.
2. Tier 1 and Tier 3 comparison to Tier 2 primary — map concordance and Moran's I comparison.
3. Stricter Tier 2b: `first_debride_date` within 365 days of `first_dfu_date` (§3.4).
4. 2017–2022 restricted window for direct commercial/Medicare comparability.
5. Alternative spatial weights (queen contiguity with island-fill vs KNN k=6 vs k=10).
6. Empirical Bayes smoothing of raw rates vs raw rates for visual comparison.

---

## 10. Known Limitations (for paper Discussion)

1. **Case definition validity** — no chart validation; claims-based only.
2. **Claim-level temporal matching not available** — Tier 2 is approximation (§3.4).
3. **Fixed ZCTA per patient** — relocation not captured (§4.1).
4. **Ambiguous-DM fraction is large** — likely coding-driven, not biological.
5. **Left censoring** — DM history before 2017/2014 unknown (§5.4).
6. **Medicare 2022 cutoff** — 2023–2024 EHSA on commercial-only (§5.5).
7. **Race unavailable for commercial** — racial disparity analysis limited to Medicare.
8. **Uninsured not captured** — AR APCD is insured-population only; uninsured diabetics and those paying cash are invisible.
9. **Dual-eligibles simplified to Medicare** — commercial history before Medicare enrollment lost.
10. **ZCTA = geographic proxy for neighborhood** — does not align with care service areas, food environments, or census tracts.
11. **Sample size for small-area estimation** — some rural ZCTAs have DM denominators <11 and are suppressed.
12. **No adjustment for age, sex, race in descriptive prevalence** — crude rates only; adjustment deferred to Paper 2.
