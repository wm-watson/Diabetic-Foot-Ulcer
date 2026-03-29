# Diabetic Foot Ulcer Emerging Hot Spot Analysis

**Spatial-Temporal Patterns of Diabetic Foot Ulceration in Arkansas Using All-Payer Claims Data**

A medical geography dissertation project analyzing diabetic foot ulcer (DFU) spatial patterns at the ZCTA level in Arkansas, using the All-Payer Claims Database (AAPCD) accessed through the Arkansas Center for Health Improvement (ACHI) APCD server.

---

## Project Overview

This project builds an analytical pipeline to:

1. **Extract** a clean patient-level diabetes and DFU cohort from the APCD server (SAS)
2. **Build** an ArcGIS-ready ZCTA-year panel dataset (R)
3. **Run** Emerging Hot Spot Analysis (EHSA) in ArcGIS Pro
4. **Fit** a spatial panel regression to explain hot/cold spot patterns
5. **Produce** a CONSORT-style flow diagram for the methods section

## Repository Structure

```
.
├── README.md
└── sas/
    ├── step2_inventory.sas    # Table structure verification & data quality checks
    ├── step3_cohort.sas       # Combined DM + DFU cohort (passthrough optimized)
    └── step5_zip_extract.sas  # ZIP extraction, linkage, dedup, CSV export
```

## Data Source

- **Arkansas APCD** (All-Payer Claims Database), DSN: `APCD-24D`
- **Commercial/Medicaid claims**: Year-partitioned tables `CLAIM_SVC_DT_2017` through `CLAIM_SVC_DT_2024`
- **Medicare Fee-for-Service**: 7 claim tables (Part B Carrier, Outpatient, Inpatient, SNF, HHA, Hospice, DME)
- **Enrollment/linkage tables**: MEMBER, MEST (AR_APCD_24B_MEST), APCD_MCR_BEN_SUM
- **Study period**: 2017-2024 (2025 excluded -- partial year creates false cold spots in EHSA)

## Pipeline: SAS Programs

Run sequentially -- Steps 3 and 5 depend on WORK datasets from prior steps.

### Step 2: `step2_inventory.sas`

Verifies table structure, column names, diagnosis field availability, ZIP format, and key fields before any cohort-building begins. Confirms:

- `mc017` is the service date (not `mc015`, which is a place-of-service code)
- Diagnosis columns `mc039`, `mc041`-`mc053` contain ICD-10-CM strings (not `mc022`/`mc023`, which are numeric category codes)
- `me107` exists on MEMBER table for MEST linkage
- `APCD_UNIQUE_ID` exists on BEN_SUM for cross-source deduplication
- ZIP codes (`me017`, `mc016`, `zip_cd`) are 5-digit format

### Step 3: `step3_cohort.sas`

Combined diabetes cohort + DFU identification in a single optimized pass. Uses **explicit SQL passthrough** (`connect to odbc` / `connection to odbc`) to push all WHERE filtering to the database server, halving table scans from ~30 to ~15.

**Diabetes identification:**
- **T1D**: `E10.x` in any DX position
- **T2D**: `E11.x` in any DX position
- **AMBIGUOUS**: Both `E10.x` and `E11.x` present across claims for the same patient

**DFU identification** (restricted to DM patients):
- **L97.x**: Ulcer of lower limb codes (primary DFU identifier)
- **Combo codes**: `E10.621`, `E10.622`, `E11.621`, `E11.622` (diabetes with foot ulcer)
- Classifies DFU source: `BOTH`, `L97_ONLY`, `COMBO_ONLY`
- Extracts severity from L97 code (6th character): Rank 1-4

**Architecture:**
- 8 commercial passthrough queries (one per year table 2017-2024)
- 7 Medicare passthrough queries (one per claim table)
- Each query scans for DM and DFU codes simultaneously
- SAS-side aggregation produces 4 WORK datasets for step 5
- Temp tables deleted after aggregation to free memory

Commercial claims use a **3-field composite key** (`mc001` + `mc006` + `mc009`) as the person identifier.

### Step 5: `step5_zip_extract.sas`

The most complex step, handling:

1. **ZIP extraction**: MEMBER.me017 (preferred) -> CLAIM.mc016 (fallback) for commercial; BEN_SUM.zip_cd for Medicare
2. **Arkansas filtering**: Restricts to AR ZIPs (prefixes `71` and `72`)
3. **Sex harmonization**: Commercial `me028` (already M/F) and Medicare `sex_ident_cd` (1->M, 2->F, else->U)
4. **MEST linkage**: Commercial patients linked via MEMBER(me001+me107) -> MEST -> `apcd_unique_id`
5. **Cross-source dedup**: `study_id` = `apcd_unique_id` + gender, shared across commercial and Medicare
6. **DFU source classification**: Derived `dfu_source` column (BOTH/L97_ONLY/COMBO_ONLY)

**Output CSVs** (produced twice -- pre- and post-dedup with study_id):

| File | Contents |
|------|----------|
| `cohort_commercial.csv` | patient_id, study_id, diabetes_type, first_dm_date, ar_zip, first_dm_year, sex, age |
| `cohort_medicare.csv` | Same structure as above |
| `dfu_commercial.csv` | patient_id, study_id, first_dfu_date, dfu_source, severity_rank, days_dm_to_dfu |
| `dfu_medicare.csv` | Same structure as above |
| `dm_dfu_analytic.csv` | Combined analytic file with all fields |

## Person Linkage Chain

```
CLAIM (mc001 + mc006 + mc009)
  -> MEMBER (me001 + me006 + me010)
    -> MEST (submitter=me001 + member_id=me107)
      -> study_id = apcd_unique_id || gender

Medicare FFS:
  BENE_ID -> BEN_SUM.APCD_UNIQUE_ID
    -> study_id = apcd_unique_id || harmonized_gender  [1->M, 2->F, else->U]
```

Cross-source duplicates = matched `study_id` across MEST and BEN_SUM.

## Key Constraints & Design Decisions

| Constraint | Rationale |
|-----------|-----------|
| Use `mc039`, `mc041`-`mc053` for DX -- never `mc022`/`mc023` | Data dictionary is wrong; those are numeric category codes |
| Use `mc017` for commercial date -- never `mc015` | `mc015` is a 2-digit place-of-service code |
| 3-field composite key for commercial persons | `mc009` alone collapses to ~170 records |
| Exclude 2025 from panel | Partial year creates false cold spot in EHSA |
| ICD codes stored without dots | Server stores `L97522` not `L97.522`; confirmed against local sample |
| Keep AMBIGUOUS diabetes type as separate category | Do not collapse into T1D or T2D in SAS -- decide at R stage |
| Arkansas ZIP filter: prefixes 71 and 72 | Covers full AR ZIP range (71601-72959) |
| Explicit SQL passthrough for claim scanning | Pushes filtering to database; avoids pulling billions of rows through ODBC |

## Downstream Pipeline (Not in This Repository)

The R pipeline (under development) will:

- Combine and deduplicate commercial + Medicare on `study_id`
- Apply HUD ZIP-to-ZCTA crosswalk (2020 vintage throughout)
- Aggregate to ZCTA-year panel (2017-2024)
- Apply empirical Bayes spatial smoothing (critical for sparse rural ZCTAs)
- Attach ACS covariates (disaggregated: pct_black, pct_native -- not composite pct_minority)
- Build adaptive KNN spatial weights (k=8, not Queen contiguity)
- Export for ArcGIS Pro Emerging Hot Spot Analysis
- Fit spatial panel regression (splm) on smoothed DFU rates

## Requirements

- **SAS** with ODBC access to APCD server (DSN=APCD-24D)
- **R** with packages: tidyverse, data.table, tidycensus, tigris, sf, spdep
- **ArcGIS Pro** for Emerging Hot Spot Analysis
- All computation runs locally (Mac, 16 GB RAM)

## Author

William Watson -- PhD Dissertation, Medical Geography
University of Arkansas for Medical Sciences (UAMS)

## License

This repository contains code for a dissertation research project. Data are not included due to APCD data use agreement restrictions.
