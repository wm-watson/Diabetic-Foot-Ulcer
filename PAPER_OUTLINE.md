# Paper Outline — Descriptive Spatial Analysis of DFU in Arkansas

**Target journal:** *Health & Place* (Elsevier). Rolling submission.
**Type:** Original Research, ~5,500–6,500 words including all sections.
**Methods word budget:** 2,000–2,500 (see `METHODS_PROSE_INSTRUCTIONS.md` §4).
**Framing:** Descriptive geospatial analysis with explicit
methodological contribution (payer stratification + three outcomes +
fractional cohort selection-bias diagnosis). Spatial regression /
modeling deferred to Paper 2.

**Journal pivot history (2026-05-05):** Originally targeted at
*Preventing Chronic Disease* (PCD) Jan 2026 Call for Papers, deadline
May 1, 2026. Pivoted to Health & Place after that deadline passed.
H&P offers a more natural fit for the methodological contribution and
allows longer-form treatment than PCD. See
`METHODS_PROSE_INSTRUCTIONS.md` for the H&P structural requirements
that supersede the PCD-style outline below — the substantive content
of the outline (titles, sections, key results) carries over largely
unchanged but the word counts and section structure are different.

---

## Working Title (options)
1. *Emerging Hot Spots of Diabetic Foot Ulcers in Arkansas: A ZCTA-Level Space-Time Analysis of All-Payer Claims, 2017–2024*
2. *Geographic Variation and Persistent Hot Spots of Diabetic Foot Ulcers in the Arkansas Delta: Evidence from an All-Payer Claims Database*
3. *Tiered Case Definitions and Space-Time Clustering of Diabetic Foot Ulcers Among Arkansans with Diabetes*

---

## Core Contribution (the "why publish")
1. **Methods:** Emerging Hot Spot Analysis applied to claims-derived chronic disease outcomes — rarely done in HSR, common in vector-borne/infectious disease epi.
2. **Case definition rigor:** Three-tier claims-based DFU definition (L97 only → L97+debridement → DM-ulcer combo codes) with sensitivity comparison across tiers.
3. **Population coverage:** AR APCD links commercial + Medicare, capturing diabetics across the full insured population rather than a single payer.
4. **Policy relevance:** Identifies Delta ZCTAs with persistent and intensifying DFU burden — actionable for AR DOH, FQHCs, and wound care program siting.

---

## Structured Abstract (≈250 words)
- **Introduction:** DFU burden, disparities, data gaps for small-area surveillance
- **Purpose:** Characterize ZCTA-level geographic variation and space-time clustering of DFU in AR
- **Methods:** AR APCD 2017–2024; tiered ICD-10/CPT case definitions; Gi*, LISA, EHSA
- **Results:** N diabetics, DFU prevalence range, number of hot spots, EHSA categories
- **Conclusion:** Persistent Delta clustering; implications for targeted intervention

---

## 1. Introduction (≈500 words)

### 1.1 Background
- DFU prevalence in US diabetics (~15–25% lifetime, ~6% annual)
- Clinical and economic burden: amputation risk, mortality, ~$9B/yr US
- Known rural and racial disparities (SE US, Delta region, Black/AI populations)

### 1.2 Gap
- Most DFU surveillance is facility-based or national Medicare only
- Small-area (ZCTA) estimates rare; state-level all-payer estimates rarer
- Space-time dynamics (is the Delta hot spot growing? shrinking? new?) not established
- Claims-based case definitions for DFU not standardized — sensitivity vs. specificity tradeoff untested at small-area scale

### 1.3 Purpose
Three aims:
1. Estimate ZCTA-level DFU prevalence among AR diabetics across three case definitions
2. Identify static hot/cold spots (Gi*, LISA)
3. Characterize space-time trends via Emerging Hot Spot Analysis (EHSA)

---

## 2. Methods (≈1,000 words)

### 2.1 Data Source
- Arkansas All-Payer Claims Database (AR APCD), commercial 2017–2024 + Medicare 2014–2022 (truncated 2022)
- IRB: cite approval
- Cross-payer deduplication on study_id = apcd_unique_id || gender

### 2.2 Study Population
- Diabetes inclusion: ≥1 claim with E10.x (T1D) or E11.x (T2D) (ICD-10-CM) across 14 diagnosis fields (commercial) or 25 fields (Medicare)
- **Primary analysis:** T1D + T2D only (exclude ambiguous — patients with conflicting E10/E11 codes across claims)
- **Sensitivity analysis:** include ambiguous in Tier 1 and Tier 2; report whether hot/cold spot pattern is stable
- Dual-eligible handling (prefer Medicare record)
- Exclusions: missing ZIP, non-AR ZIP (not in 71xxx/72xxx), age <18

### 2.3 DFU Case Definitions (the methods centerpiece)
- **Tier 1 (broadest):** Any L97.x OR DM-ulcer combo code (E10.621, E10.622, E11.621, E11.622)
- **Tier 2 (primary analysis):** L97.x with debridement CPT (97597-98, 11042-47, 97602) within ±30 days, OR combo code
- **Tier 3 (strictest):** DM-ulcer combo code only
- Rationale: L97.x captures "ulcer of lower limb" but is not diabetes-specific; debridement confirms active wound care; combo codes are most specific but under-coded
- Report all three tiers; Tier 2 as primary, Tiers 1/3 as sensitivity

### 2.4 Geographic Unit
- ZCTA assignment hierarchy: latest MEMBER record → claim ZIP → BEN_SUM ZIP
- ZIP-to-ZCTA crosswalk (UDS Mapper 2023)
- Restricted to 2-digit AR prefixes (71, 72)
- Final denominator: 779 AR ZCTAs

### 2.5 Prevalence Calculation
- Numerator: DFU patients per ZCTA (Tier 2)
- Denominator: diabetic patients per ZCTA
- Expressed as cases per 1,000 diabetics
- Small-area smoothing: empirical Bayes shrinkage (or raw with suppression <11)

### 2.6 Spatial Analysis
- **Global Moran's I** — overall spatial autocorrelation test
- **Local Gi\* (Getis-Ord)** — identify high/low clusters; adaptive KNN k=8; 99%/95%/90% CI bins
- **LISA (Local Moran's I)** — HH/LL/HL/LH classification as sensitivity
- **Emerging Hot Spot Analysis (EHSA)** — ArcGIS Pro Space-Time Cube:
  - Bin: 1 year × ZCTA
  - Neighborhood: 8 nearest ZCTAs
  - Time step: 1 year
  - Output categories: new, consecutive, intensifying, persistent, diminishing, sporadic, oscillating, historical hot spots (and cold counterparts)
- Software: R (sf, spdep) for Gi*/LISA; ArcGIS Pro for EHSA

### 2.7 Handling Known Limitations (preempt reviewers)
- Medicare truncation at 2022 → sensitivity analysis limiting to 2017–2022
- Left censoring (DM before 2017) → prevalent vs. incident cases noted
- Amputation censoring (don't double-count DFU post-amp) → applied at patient-year level

---

## 3. Results (≈1,000 words, map- and table-heavy)

### 3.1 Cohort Characteristics (Table 1)
- N diabetics (365K), N DFU by tier, age, sex, race, payer split, T1D/T2D/ambiguous
- Urban/rural distribution (RUCA codes)

### 3.2 ZCTA-Level Prevalence (Figure 1 — choropleth panel)
- Panel A: diabetic count per ZCTA
- Panel B: Tier 2 DFU prevalence per 1,000 diabetics
- Panel C: Tier 1 vs Tier 3 side-by-side (sensitivity)

### 3.3 Static Spatial Clustering (Figure 2, Table 2)
- Global Moran's I, permutation p-value
- Gi* map — hot/cold at 90/95/99% CI
- LISA cluster map
- Concordance between Gi* and LISA (kappa or % agreement)

### 3.4 Emerging Hot Spot Analysis (Figure 3 — the money shot)
- EHSA classification map with 8–10 category legend
- Count of ZCTAs by category (Table 3)
- Narrative on Delta persistence, any new/emerging areas, any diminishing areas
- Optional: time-series line plot of top 5 persistent hot spot ZCTAs

### 3.5 Tier Sensitivity (Table 4)
- Count, % of DM, Moran's I, n hot spots by tier (Tier 1, Tier 2 primary, Tier 3)
- Ambiguous-DM sensitivity row: Tiers 1 and 2 re-run with ambiguous included
- Shows that spatial pattern is robust across case definitions and DM classification

---

## 4. Discussion (≈800 words)

### 4.1 Principal Findings
- Delta region persistently hot across 2017–2024
- Prevalence gradient (quantify — Delta vs NW AR)
- Tier 2 captures X% of Tier 1, X% of Tier 3 — sensitivity story

### 4.2 Comparison to Prior Literature
- Barshes (Houston), Margolis (Medicare), Skrepnek (US), Harding (UK)
- Position: first AR all-payer small-area + first EHSA of DFU

### 4.3 Public Health Implications
- Targeting wound care resources (FQHC podiatry, mobile clinics)
- AR DOH surveillance infrastructure
- Medicaid expansion effects (if distinguishable)
- Link to upcoming CMS/ARIA rural health programs

### 4.4 Limitations
- Claims-based case definitions (no chart validation)
- ZCTA geography mismatch with service areas
- 2022 Medicare cutoff
- Left censoring
- No individual-level SDOH (saved for Paper 2)

### 4.5 Conclusion
- 1–2 sentences, action-oriented

---

## Figures and Tables (planned count)

| # | Type | Content | Status |
|---|---|---|---|
| T1 | Table | Cohort demographics by payer and tier | Need to regenerate after step3 rerun |
| T2 | Table | Moran's I, Gi* hot/cold counts by tier | Partial (have Tier 1) |
| T3 | Table | EHSA category counts | **Need to run EHSA** |
| T4 | Table | Tier sensitivity comparison | Need to run Tiers 2 & 3 |
| F1 | Map panel | Choropleth: DM count + DFU prevalence + tier comparison | Have Tier 1 static; need Tier 2/3 |
| F2 | Map | Gi*/LISA static clusters | Have draft |
| F3 | Map | EHSA categories | **Need to run EHSA** |
| F4 (opt) | Line | Top persistent hot spot trajectories | Nice-to-have |

---

## Work Plan to May 1 (4 weeks)

### Week 1 (now → ~Apr 12)
- [ ] Finalize step3 on server (procedure extraction DM-restricted) ← in progress
- [ ] Rerun step5 with updated procedure fields
- [ ] Pre-submission email to PCD editor re: format (Original Research vs GIS Snapshot)
- [ ] R script: apply Tier 2 temporal window (±30d debridement-to-DFU claim match)
- [ ] Regenerate descriptive stats + choropleths for all 3 tiers

### Week 2 (~Apr 13–19)
- [ ] Build space-time cube in ArcGIS Pro (ZCTA × year)
- [ ] Run EHSA tool with k=8 neighborhood
- [ ] Run static Gi* + LISA for Tiers 2 and 3 (R)
- [ ] Finalize all figures to PCD specs (300dpi, legends, scale bars, north arrows)
- [ ] Draft Methods section

### Week 3 (~Apr 20–26)
- [ ] Draft Introduction + Results
- [ ] Draft Discussion
- [ ] Compile Table 1 (cohort demographics)
- [ ] Cover letter draft
- [ ] Internal review with advisor

### Week 4 (~Apr 27–May 1)
- [ ] Revise from advisor feedback
- [ ] Format to PCD submission requirements (title page, structured abstract, refs AMA style, figure files separate)
- [ ] Compile supplementary material (code availability statement, ICD/CPT code lists)
- [ ] Submit by May 1

---

## PCD-Specific Requirements Checklist
- [ ] Structured abstract (Introduction, Purpose, Methods, Results, Conclusion)
- [ ] ≤3,500 words main text (Original Research) OR ≤1,500 (GIS Snapshot)
- [ ] ≤50 references, AMA style
- [ ] ≤6 tables/figures combined (confirm current limit)
- [ ] Plain-language summary required
- [ ] Public health implications section
- [ ] IRB statement
- [ ] Data availability statement (APCD DUA language)
- [ ] Author contributions (ICMJE)
- [ ] Code availability — point to `wm-watson/Diabetic-Foot-Ulcer` repo

---

## Risks and Mitigations
- **Risk:** EHSA not finished in time → fall back to static Gi*+LISA only, still fits the call ("Maps illustrating geographic variations")
- **Risk:** Tier 2 temporal matching reveals data issues → submit with Tier 1 as primary, Tiers 2/3 as sensitivity
- **Risk:** Advisor unavailable for review → aim for Week 3 completion to buffer
- **Risk:** 3,500 words too tight with 3 aims → drop aim 1 (prevalence description) to supplement, lead with clustering + EHSA

---

## Deferred to Paper 2 (modeling paper)
- Negative binomial / Poisson GLMM of DFU counts
- SDOH covariates (SVI, SDI, broadband, food access)
- Provider supply covariates (podiatrist density, FQHC distance)
- Bayesian spatial smoothing (BYM2/INLA)
- Causal language / policy evaluation
- Target: *Health & Place* or *Health Services Research*
