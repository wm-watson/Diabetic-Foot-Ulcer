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

### 4.4 Memphis edge effect (rev 4, 2026-04-28 — mechanism corrected)

Five counties in eastern Arkansas lie within roughly 30 miles of
Memphis, Tennessee: **Crittenden, Mississippi, Phillips, Lee, and
St. Francis**. Residents of these counties have appreciably lower
coded DFU and amputation rates in our data than their clinical
profile would predict.

**Initial hypothesis (rev 3) — partially incorrect.** I previously
attributed the cold spot primarily to TN-licensed commercial
insurance carried by Memphis-employed AR residents. The
payer-stratified analysis (script 09, run 2026-04-28) refutes this:
**Border-Memphis has the HIGHEST commercial-stratum DFU rate of any
AR region (9.77 per 1,000) and ZERO commercial cold-spot ZCTAs.**
The cold spot is not a commercial-insurance artifact.

**Revised mechanism (rev 4):** the Border-Memphis cold spot is a
**Medicare and Medicaid signal**:

| Stratum | Cold ZCTAs in Border-Memphis | Mean DFU rate |
|---|---|---|
| Medicare | 33 | 6.30 |
| Medicaid | 11 | 4.93 |
| Mixed-payer | 16 | 7.12 |
| **Commercial** | **0** | **9.77** (highest of regions) |

The dominant capture mechanism is therefore most likely
**TN-licensed Medicare Advantage plans** in which AR border residents
are enrolled but which do not report to the AR APCD. The Member
Enrollment Selection Table (MEST) only captures AR-licensed payer
products. CMS-reported Medicare FFS *is* fully captured; AR-licensed
Medicare Advantage *is* captured; **Memphis-area TN-licensed Medicare
Advantage plans (Humana TN, BCBS-TN MA, Cigna TN MA) are not
captured**, and Memphis brokers actively sell these plans into AR
border counties.

Secondary mechanisms (smaller contribution):
- **Memphis VA utilization** — dual-eligible veterans use Memphis VA
  for some care; those federal-VA claims are absent from the APCD.
- **Self-pay / charity care** at Memphis safety-net providers
  (Regional One).
- Some commercial leakage may persist for AR residents on small TN
  group plans, but the data show this is a minor contributor.

Mechanisms that **do not** contribute to the edge effect:
- Tennessee Medicaid / TennCare — Medicaid is state-residency-based;
  an AR resident cannot be on TN Medicaid. AR Medicaid claims for
  out-of-state care do flow to the AR APCD.
- AR-licensed commercial insurance covering care at Memphis providers
  — these claims flow to the APCD regardless of where care is
  delivered.
- Medicare FFS at Memphis providers — CMS-reported and included.

**Why the commercial stratum looks "normal" in Border-Memphis:** the
small AR-licensed commercial population we *do* capture there is a
selected subset (older, more stable employees on AR-licensed plans;
the high-mobility working-age commuters on TN plans are invisible).
The captured commercial subset has high DFU rates because it is an
older / sicker subgroup, not because the region is well-covered.

**Analytic handling:**
- Flag the five counties as undercaptured in the main analysis.
- Sensitivity analysis: exclude those counties and re-run the spatial
  analysis. If hot spots elsewhere are stable after exclusion, the
  rest of the map is robust.
- The corrected mechanism implies that the appropriate adjustment
  approach (Paper 2) is a **TN-MA enrollment proxy** (e.g., per-county
  estimates of TN-MA penetration from CMS Medicare Advantage / Part D
  Contract Enrollment files), not a LODES commuter correction. LODES
  remains useful as a secondary covariate but is no longer the
  primary mechanism we are correcting for.

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

### 6.4 Small-cell suppression (rev 3, 2026-04-21)
- **PCD display rule:** numerator <11 OR denominator <20 per cell.
- **Analytic vs display separation** (critical decision, 2026-04-21):
  Previously the PCD rule was applied as an *exclusion criterion* in
  the pooled Gi*/LISA analysis itself. That hid 361 low-DFU-burden
  ZCTAs (median 2 DFU cases, median 336 DM person-halfyears) -- ZCTAs
  that were analytically fine but failed the PCD display threshold --
  and collapsed the cold-spot signal. Gi* then saw only 314 of 614 AR
  ZCTAs and the map showed all-hot-or-nothing.
- **Current default (`SUPPRESS_DISPLAY = FALSE` in
  `R/05_static_spatial.R`):** the pooled spatial analysis requires
  only `dm_denom >= 20` (denominator floor). This includes zero-DFU
  and low-DFU ZCTAs in the Gi*/LISA computation, yielding 602 modeled
  ZCTAs with a proper hot/cold gradient (91 hot, 69 cold, 442 NS under
  single-test z-bands). Local EB smoothing handles the small-N
  instability.
- **For publication:** set `SUPPRESS_DISPLAY = TRUE` to re-apply the
  PCD <11 cases rule to displayed cell-level rate labels. The Gi*
  analysis itself remains on all 602 ZCTAs; the suppression only
  affects the rate choropleth labels and descriptive tables.
- **EHSA input (script 06)** is unsuppressed regardless of the flag.
- **Rationale for keeping both:** PCD privacy rules protect displayed
  cell-level rates but were never meant to exclude cells from
  analytic methods. Standard spatial epi practice (Anselin, Waller &
  Gotway) is to analyze all ZCTAs with valid denominators and apply
  privacy rules only to displayed rate labels.

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

### 8.5 Three-outcome framework (rev 3, 2026-04-21)
The DFU prevalence map alone is insufficient for public health
targeting in Arkansas because it measures coding intensity and care
engagement, which are confounded with the true spatial distribution
of severe disease. We therefore run three complementary spatial
analyses, each on the same enrollment-weighted denominators:

| Script | Outcome | Numerator | Denominator | What it measures |
|---|---|---|---|---|
| `05_static_spatial.R` | DFU prevalence | DFU (Tier 2) patients | DM person-halfyears | Coding intensity + care engagement |
| `07_amputation_spatial.R` | Amputation incidence | First lower-extremity amputation | DM person-halfyears | Disease burden + progression combined |
| `08_amp_among_dfu_spatial.R` | Progression-given-DFU | First amputation among Tier 2 | DFU person-halfyears | Progression / access failure given disease |

**Why three outcomes:**
- Script 05 is sensitive to ICD coding variation and care engagement.
  Ozark retirees with chronic DFU generate repeat L97 codes in primary
  care, producing "hot" Gi* signal that does not reflect worst
  disease burden.
- Script 07 reduces coding sensitivity (amputation is a forced event
  — cannot fail to code a below-knee amputation) but still mixes two
  gradients: who develops DFU (gradient A) × who progresses to
  amputation (gradient B).
- Script 08 isolates gradient B by conditioning on already having
  coded DFU. This is the cleanest measure of healthcare-access failure
  given recognized disease.

**Interpretive pattern from the continuous cohort (rev 4, 2026-04-28;
revised again 2026-04-28 with conditional payer-stratified results):**
The three maps diverge meaningfully, and the divergence *is* the
finding. Five regions have distinguishable spatial signatures:

- **Ozarks** — Hot on 05 (most coded DFU), Cold on 07 (low amputation
  incidence), **Strongly Cold across ALL FOUR payer strata in the
  conditional analysis (08 + payer-stratified)**. The Ozark
  protective signal is robust across Medicare (29 cold / 5 hot
  ZCTAs), Medicaid (13 cold / 2 hot), Commercial (0 cold / 4 hot —
  small N, less interpretable), and **especially Mixed-payer (54 cold
  / 0 hot, 71% of Ozark Mixed-payer ZCTAs are cold)**. Even patients
  with the most fragmented coverage in the Ozarks rarely progress to
  amputation. Interpretation: **chronic mild DFU in retiree
  populations with stable primary care** is the leading hypothesis,
  but the cross-stratum robustness suggests an Ozark-wide protective
  factor that warrants its own investigation (e.g., regional wound-
  care provider density, primary-care continuity, lower disease
  severity at presentation).

- **Delta-Interior** — Normal on 05 (11.1/1000, at state average), Hot
  on 07 (0.85/1000, elevated amputation), Normal on 08 in the pooled
  view but **strongly hot in the Mixed-payer stratum** (16 hot vs 7
  cold ZCTAs in conditional amp). Mixed-payer = patients with payer
  churn (dual-eligibles, Medicaid expansion churners). Interpretation:
  **the patients with the most fragmented insurance have the worst
  amputation outcomes, and they are spatially concentrated in the
  Delta-Interior**. Pooled or single-payer analysis hides this; only
  payer-stratified analysis surfaces it.

- **Delta-Border-Memphis** — Cold on 05 and 07 (DFU prevalence and
  unconditional amputation), driven by Medicare/Medicaid undercapture
  (§4.4). **The conditional payer-stratified analysis re-frames this
  finding:** among the DFU patients we *do* capture in Border-Memphis,
  Medicare progression is **elevated** (8 hot vs 5 cold ZCTAs in the
  amp_dfu Medicare stratum, EB rate 19.2/1,000 DFU PHY — well above
  state average). This means: the cold spot for *prevalence* is a
  data-capture artifact, but the cold spot does **not** indicate that
  Border-Memphis has favorable disease outcomes. The captured patients
  there progress to amputation at high rates; the missing patients
  (TN-MA enrollees) likely have similar or worse rates. Real disease
  severity in Border-Memphis is at least as bad as Delta-Interior —
  possibly worse — but cannot be estimated from APCD alone.

- **Arkansas River Valley (Pope, Johnson, Yell, Logan, Howard
  counties)** — Moderate on 05, Moderately Hot on 07, **Strongly Hot
  on 08 (pooled and Medicare-stratified)**. The Medicare stratum
  conditional analysis gives 17 hot vs 2 cold ZCTAs in River Valley,
  with EB rates of 22.0/1,000 DFU PHY (highest of any region in
  Medicare). Interpretation: **under-recognized progression hot
  spot**. DFU patients in this region progress to amputation at
  markedly higher rates than elsewhere. Robust to payer stratification.
  Strongest candidate for targeted intervention based on the Medicare
  data alone.

- **Southwestern Arkansas / Texarkana corridor (NEW finding,
  2026-04-28)** — Howard, Sevier, Little River, and Hempstead counties
  emerge as a strong amputation-progression hot spot in the
  **Mixed-payer conditional analysis** (script 09 OUTCOME=amp_dfu,
  STRATUM=MIXED). Top hot ZCTAs include 71852/71851/71831 (Howard),
  71842/71846 (Sevier), 71836/71822 (Little River), 71838 (Hempstead),
  with EB rates of 27–42 per 1,000 DFU PHY. Howard County also appears
  in the top 10 hot ZCTAs of the Medicare amp_dfu stratum, suggesting
  the signal is real and not a payer-mix artifact. **This region was
  not flagged by any of the prior pooled or single-payer DFU-prevalence
  or amputation-incidence analyses**; it surfaces only in the
  conditional, payer-stratified framework. Interpretation: among
  Texarkana-corridor DFU patients with payer churn, progression to
  amputation occurs at the highest rates in the state. Tied with River
  Valley as a top-priority intervention target.

**Global Moran's I values (rev updated 2026-04-28):**

| Outcome | Pooled | Medicare | Medicaid | Commercial | Mixed |
|---|---|---|---|---|---|
| DFU prevalence (script 05) | 0.18 | **0.72** | 0.47 | 0.42 | 0.59 |
| Amputation incidence (07) | 0.30 | 0.35 | 0.44 | 0.35 | 0.42 |
| Amp given DFU (08, 09 amp_dfu) | 0.44 | **0.50** | 0.56* | 0.40* | **0.54** |

*Medicaid (N=342 DFU pts) and Commercial (N=213 DFU pts) amp_dfu
strata are sparse; treat their Moran's I as suggestive only.

**Pooled values systematically understate the spatial pattern**
because they average across payer-specific patterns that point in
different directions. The payer-stratified Moran's I values (script
09) are the more accurate description of the underlying spatial
structure. This motivates the promotion of payer stratification from
sensitivity to primary methodology (§8.6).

### 8.6 Payer-stratified analysis (rev 4, 2026-04-28 — promoted to primary)

The pooled (cross-payer) spatial analysis attenuates real spatial
signal because it averages across payer-specific gradients that point
in different directions. Pooled DFU-prevalence Moran's I = 0.18; the
same outcome stratified by Medicare alone gives Moran's I = 0.72.
The payer-stratified analysis is therefore promoted from a sensitivity
analysis (§9 in earlier revisions) to a **primary methodology** in
the paper.

**Stratification scheme** (`sas/step3d_payer_strata.sas`):
each patient's enrollment-month totals across 2017–2022 are computed
from the MEST + BEN_SUM:
- `months_medicare`   = months with MCR_ADV or BEN_SUM-FFS coverage
- `months_medicaid`   = months with MCD / MCD_QHP / HCIP / PASSE
- `months_commercial` = months with COM / QHP-non-MCD / EBD

A patient is assigned to the stratum with ≥80% of total months in
that payer category; otherwise they are classified as **MIXED**.
Stratum sizes from the fractional cohort (full enrollment population):

| Stratum | N patients |
|---|---|
| MEDICARE | 154,651 |
| MIXED | 108,057 |
| MEDICAID | 25,510 |
| COMMERCIAL | 24,959 |

**Interpretive value:**
- **Medicare stratum** — most reliable signal for DFU coding intensity
  and chronic management; reflects retiree primary-care patterns.
  Strongest Moran's I (0.72 for DFU prevalence) because the population
  is large (N=154,651) and care-utilization is consistent.
- **Medicaid stratum** — captures the working-poor and disabled
  populations with eligibility-based coverage. Smaller N (25,510) but
  represents the highest-poverty subset of AR diabetics.
- **Commercial stratum** — younger, employed, generally healthier
  population. Smallest amputation signal because amputation is rare
  in working-age commercial insureds. Notable use: serves as the
  **diagnostic check on the Memphis edge mechanism** — Border-Memphis
  has the highest commercial DFU rate of any region (9.77/1,000) and
  zero commercial cold-spot ZCTAs, refuting the earlier hypothesis
  that the cold spot was a TN-employer-commercial-insurance artifact.
- **Mixed-payer stratum** — patients with payer churn (commonly dual-
  eligible, Medicaid-expansion churn, or Medicare-MA transitions). This
  is the **clinically highest-risk stratum** and reveals signal that
  pooled analysis hides — notably the Delta-Interior amputation hot
  spot (17 hot vs 7 cold ZCTAs), which is invisible in single-payer
  strata.

**Two clinically critical findings emerge only in payer-stratified
analysis:**

1. **The Mixed-payer Delta-Interior amputation finding** (script 09
   OUTCOME=amp): patients with fragmented insurance coverage are
   spatially concentrated in the Delta-Interior and have the highest
   amputation incidence (17 hot vs 7 cold ZCTAs).
2. **The southwestern Arkansas / Texarkana-corridor progression hot
   spot** (script 09 OUTCOME=amp_dfu, STRATUM=MIXED): Howard, Sevier,
   Little River, and Hempstead counties show the highest DFU →
   amputation progression rates in the state (EB rates 27–42 per
   1,000 DFU person-halfyears). Howard County also appears in the top
   10 hot ZCTAs of the Medicare amp_dfu stratum, supporting that the
   signal is not purely a payer-mix artifact. **This region was not
   flagged by any prior pooled or single-payer analysis.**

Both findings share a common feature: they require both **payer
stratification** AND the **conditional outcome** to surface. They are
the strongest argument in the methods section for why pooled
descriptive mapping is insufficient for AR public-health intervention
targeting.

**Script 09 supports three outcomes:**
- `OUTCOME=dfu` — DFU prevalence among DM (denom: DM person-halfyears)
- `OUTCOME=amp` — amputation incidence among DM (denom: DM PHY)
- `OUTCOME=amp_dfu` — conditional amputation among Tier 2 DFU
  (denom: DFU person-halfyears, restricted to Tier 2). Looser
  denominator floor (DEN_THRESH=5) since DFU person-halfyears are
  inherently smaller.

**Recommended figure structure for paper:**
- Figure 1 (pooled, all three outcomes) — descriptive, sets the
  baseline.
- Figure 2 (Medicare-stratified, all three outcomes) — the dominant
  payer; cleanest signal.
- Figure 3 (Mixed-payer, amputation incidence + conditional amp) —
  the two clinically critical findings.
- Figure 4 (conditional amp across all four strata, panel of 4 maps)
  — shows the Texarkana corridor and Ozark protective gradient.
- Supplementary (Medicaid and Commercial strata) — complete the
  picture; demonstrate that the cold-spot pattern in Border-Memphis
  is Medicare/Medicaid-driven, not commercial.

**Statistical handling of small/sparse strata** (Medicaid and
Commercial amputation): script 09 falls back from Local EB to raw
rate when EB produces NA in zero-event neighborhoods. Affected ZCTAs
are recorded in the script log and noted in the figure caption.

---

## 9. Sensitivity Analyses Planned

**Note (rev 4, 2026-04-28):** The payer-stratified analysis previously
listed here as a sensitivity has been promoted to primary methodology
(§8.6). It is no longer a sensitivity.

1. Include ambiguous-DM (§2.3) in Tiers 1 and 2 → re-run Gi* and EHSA, compare hot spot stability.
2. Tier 1 and Tier 3 comparison to Tier 2 primary — map concordance and Moran's I comparison.
3. Stricter Tier 2b: `first_debride_date` within 365 days of `first_dfu_date` (§3.4).
4. 2017–2022 restricted window for direct commercial/Medicare comparability (subsumed by §8.6 since the payer-stratified analysis already separates them).
5. Alternative spatial weights (queen contiguity with island-fill vs KNN k=6 vs k=10).
6. Empirical Bayes smoothing of raw rates vs raw rates for visual comparison.
7. **Fractional cohort sensitivity** (§6.1) — rerun scripts 05/07/08/09 with
   `DFU_COHORT=fractional` to verify hot spot locations are stable across
   the continuous-enrollment inclusion criterion.
8. **Memphis-border exclusion sensitivity** (§4.4) — rerun all three
   spatial analyses with the five Memphis-adjacent counties (Crittenden,
   Mississippi, Phillips, Lee, St. Francis) excluded. Stability of hot
   spots outside those counties establishes robustness of the main
   findings to the MNAR data-capture artifact.

### 9.1 MNAR adjustment options for Memphis edge (deferred — Paper 2)
The Memphis-border undercapture (§4.4) is formally Missing Not At
Random — missingness is correlated with both location (distance to
Memphis) and outcome. The payer-stratified analysis (§8.6) refined
the mechanism: the cold spot is **Medicare/Medicaid-driven, not
commercial**. Candidate adjustment strategies, in light of the
corrected mechanism:

- **Option A — Exclusion sensitivity** (planned, §9 #8): primary
  robustness check; simplest defense.
- **Option B — TN-Medicare-Advantage penetration correction (NEW,
  rev 4):** use CMS Medicare Advantage / Part D Contract Enrollment
  files (publicly available) to compute, per AR ZCTA, the fraction of
  Medicare beneficiaries enrolled in TN-licensed MA plans. Apply as
  multiplicative correction to the Medicare-stratum denominator.
  This replaces Option B in rev 3 (LODES commuter correction), which
  targeted the wrong mechanism.
- **Option C — Spatial regression with distance-to-Memphis and TN-MA
  penetration as covariates:** formal adjustment in the Paper 2 panel
  regression; hot-spot analysis then runs on residuals.
- **Option D — External benchmarking** against CDC Diabetes Atlas,
  AR Hospital Discharge Data (HDD), or AHRQ SID to estimate
  county-level capture rates.
- **Option E — Pattern mixture / selection model** (Little & Rubin):
  specify plausible δ for how missing cases differ from captured;
  report how hot-spot conclusions vary across the δ range.

Deferred to Paper 2 (regression). For Paper 1, Option A sensitivity
plus §4.4 Limitations language plus the payer-stratified Figure 2/3
(showing the cold spot is Medicare/Medicaid-driven) is the committed
approach.

---

## 10. Known Limitations (for paper Discussion)

1. **Case definition validity** — no chart validation; claims-based only.
2. **Claim-level temporal matching not available** — Tier 2 is approximation (§3.4).
3. **Fixed ZCTA per patient** — relocation not captured (§4.1).
4. **Memphis edge effect — MNAR undercapture in five Arkansas
   counties** (§4.4, mechanism revised 2026-04-28). Crittenden,
   Mississippi, Phillips, Lee, and St. Francis counties have
   depressed DFU and amputation rates driven primarily by AR
   residents enrolled in **TN-licensed Medicare Advantage plans**
   (Humana TN, BCBS-TN MA, Cigna TN MA) that do not report to the AR
   APCD. Secondary mechanisms include Memphis VA utilization and
   self-pay/charity care at Memphis safety-net providers. Earlier
   hypothesis that TN-licensed *commercial* insurance was the primary
   mechanism was refuted by the payer-stratified analysis (§8.6):
   the commercial stratum has the highest DFU rate of any region in
   Border-Memphis (9.77/1,000) and zero commercial cold-spot ZCTAs.
   AR Medicaid and Medicare FFS are fully captured regardless of
   where care is delivered. Primary handling: Option A exclusion
   sensitivity (§9 #8) plus the payer-stratified Figure 2/3 showing
   the cold spot is Medicare/Medicaid-driven; full MNAR adjustment
   via TN-MA-penetration correction or pattern-mixture deferred to
   Paper 2 (§9.1).
5. **Identity-resolution collisions** — APCD does not have SSN; same
   person switching insurers may receive a new `apcd_unique_id` with
   no linkage to the old record (§6.1). Direction of bias is
   conservative (deflates rates in high-churn areas).
6. **Ambiguous-DM fraction is large** — likely coding-driven, not biological.
7. **Left censoring** — DM history before 2017/2014 unknown (§5.4).
8. **Medicare 2022 cutoff** — 2023–2024 EHSA on commercial-only (§5.5).
9. **Race unavailable for commercial** — racial disparity analysis limited to Medicare.
10. **Uninsured not captured** — AR APCD is insured-population only; uninsured diabetics and those paying cash are invisible.
11. **Dual-eligibles simplified to Medicare** — commercial history before Medicare enrollment lost.
12. **ZCTA = geographic proxy for neighborhood** — does not align with care service areas, food environments, or census tracts.
13. **Sample size for small-area estimation** — under current analytic
    threshold (dm_denom ≥ 20), 602 of 614 AR ZCTAs are modeled. The
    12 excluded have truly too-sparse diabetic populations.
14. **No adjustment for age, sex, race in descriptive prevalence** — crude rates only; adjustment deferred to Paper 2.
15. **DFU coding intensity ≠ disease burden** — rationale for the
    three-outcome framework (§8.5). Reporting DFU prevalence alone
    would mislead public health targeting in Arkansas.
16. **Pooled cross-payer analysis attenuates true spatial signal**
    (§8.6, rev 4). Pooled DFU-prevalence Moran's I = 0.18; the
    Medicare-only stratum gives Moran's I = 0.72 for the same
    outcome. Public health decisions made from pooled descriptive
    maps would systematically misidentify the populations and regions
    in greatest need. The paper therefore presents payer-stratified
    maps as the primary analytic product and uses pooled maps only as
    a baseline reference. The Mixed-payer stratum specifically reveals
    a Delta-Interior amputation hot spot (17 hot vs 7 cold ZCTAs)
    that is invisible in single-payer or pooled analysis — patients
    with payer churn (dual-eligibles, Medicaid-expansion churners) are
    the population with the worst outcomes and they are spatially
    concentrated.
