# Damage Data Availability and Compatibility

This repository contains the R workflow used for the paper **Data Compatibility as a Bottleneck in Cross-Country Earthquake Damage Modelling: Nepal–Türkiye transfer with a Japan measurement-regime contrast**.

The project studies whether existing post-disaster building-damage datasets can support transferable, cross-country earthquake damage modelling. The central finding is that model transfer is limited less by the choice of classifier than by the availability of comparable asset-level variables, damage labels, hazard measures and data-collection protocols.

## Project overview

Post-earthquake damage datasets are often collected for different operational purposes: engineering reconnaissance, reconstruction surveys, remote-sensing assessments, humanitarian needs assessments or national recovery programmes. Even when these datasets contain apparently similar variables, their meanings may differ across countries and collection regimes.

This repository implements a compatibility-first modelling pipeline using three datasets:

- **Nepal**: 2015 Gorkha earthquake post-disaster building inventory, with a large survey-based asset-level dataset and ordinal EMS-98-style damage labels.
- **Türkiye**: 2023 earthquake-sequence engineering reconnaissance sample, with detailed reinforced-concrete structural variables and hazard-intensity descriptors.
- **Japan**: 2024 Noto Peninsula building-damage inventory, used as a remote-sensing measurement-regime contrast rather than as a symmetric third training country.

The workflow builds harmonised targets, constructs nested feature sets, benchmarks within-country models, tests Nepal–Türkiye cross-country transfer, runs predictor-overlap ablations, and uses a controlled simulation to separate the effect of compatibility regime from model-family choice.

## Research questions

The code supports five main questions:

1. Which asset-level earthquake damage variables are directly compatible, convertible, available only as coarse proxies, or unavailable across Nepal, Türkiye and Japan?
2. How much predictive performance is lost when moving from country-specific feature sets to a minimal harmonised Nepal–Türkiye core?
3. In Nepal–Türkiye transfer, is weak transfer more consistent with model choice or with measurement incompatibility?
4. What does the Japan remote-sensing inventory reveal about the limits of extending survey-based transfer models to a different post-disaster data regime?
5. What minimum asset-level data core would improve future cross-country transferability and support disaster-risk-reduction-oriented post-event surveys?

## Main empirical message

Within-country models recover meaningful predictive signal, but direct cross-country transfer is poor under the available harmonised feature core.

Key results from the manuscript:

- Nepal-only Random Forest binary AUC: **0.864**.
- Türkiye-only Random Forest binary AUC: **0.700**.
- Pooled Nepal–Türkiye Random Forest binary AUC on a random held-out pooled split: **0.827**.
- Nepal-trained model scored on Türkiye: **AUC 0.434**.
- Türkiye-trained model scored on Nepal: **AUC 0.450**.
- Simulation variance decomposition: compatibility regime explains about **52% of AUC variance** and **56% of macro-F1 variance**, compared with about **2%** and **0.3%** for model family.

The below-random cross-country AUCs are not treated as a coding failure. They are the central diagnostic result: forcing different post-disaster datasets into a small common feature space can invert or weaken the relationship between predictors and observed damage.

## Repository structure

```text
.
├── inputs/                         # Raw input data files; not necessarily versioned
├── outputs/                        # Generated intermediate and final results
├── 00_load_raw_data.R              # Load raw Nepal, Türkiye and Japan files
├── 01_build_metadata_dictionary.R  # Build source-variable dictionary and compatibility metadata
├── 02_target_harmonization.R       # Harmonise five-class and binary damage targets
├── 03_feature_harmonization.R      # Build F1, F2, F3 and J1 feature sets
├── 04_country_specific_models.R    # Within-country benchmarks and modelling helpers
├── 05_cross_country_transfer.R     # Nepal–Türkiye transfer and pooled-holdout experiments
├── 06_overlap_ablation.R           # Predictor-overlap ablation experiments
├── 07_simulation_study.R           # Controlled compatibility-regime simulation
├── 08_figures_tables.R             # Paper figures and tables
├── 09_diagnostics.R                # Confusion matrices, calibration and PDP diagnostics
├── 10_subgroup_robustness.R        # Additional subgroup and robustness checks
└── DIAGNOSIS.md                    # Notes on debugging and patched modelling issues
```

## Feature sets

The main harmonised feature sets are:

| Feature set | Scope | Description |
|---|---:|---|
| `F1` | Nepal + Türkiye | Minimal common core: `age`, `no_stories`, `floor_area_m2`, `total_height_m`, `hazard_proxy_distance_km` |
| `F2` | Nepal + Türkiye | `F1` plus a collapsed structural-family descriptor |
| `F3_Nepal` | Nepal | Nepal-specific full predictor set |
| `F3_Türkiye` | Türkiye | Türkiye-specific full predictor set |
| `J1_Japan` | Japan | Reduced binary contrast set: footprint-derived/geospatial variables, MMI and hazard overlays |

Japan is deliberately not used as a symmetric transfer country. It represents a different measurement regime: remote-sensing-derived footprint damage mapping rather than a survey-based building inventory.

## Targets

The workflow uses two damage targets:

- `y5`: five-class ordinal damage target, available for Nepal and Türkiye after harmonisation.
- `y2`: binary severe-damage target, defined as severe/destroyed versus not severe.

For Nepal, the source damage labels follow EMS-98-style ordinal damage grades. For Türkiye, the source structural labels are mapped from `{N, L, M, S, C}` to `{1, 2, 3, 4, 5}`. For Japan, the binary target is retained, but an equivalent five-class ordinal target is not available.

## Main workflow

Run the scripts in order from the repository root, after editing local input paths in `00_load_raw_data.R` if needed.

```r
source("00_load_raw_data.R")
source("01_build_metadata_dictionary.R")
source("02_target_harmonization.R")
source("03_feature_harmonization.R")
source("04_country_specific_models.R")
source("05_cross_country_transfer.R")
source("06_overlap_ablation.R")
source("07_simulation_study.R")
source("08_figures_tables.R")
source("09_diagnostics.R")
source("10_subgroup_robustness.R")
```

After patching or debugging model outputs, the minimum re-run order is:

```r
source("04_country_specific_models.R")
source("05_cross_country_transfer.R")
source("06_overlap_ablation.R")
source("08_figures_tables.R")
```

The simulation results are independent of the empirical data pipeline and do not need to be re-run unless the simulation design changes.

## Expected outputs

The scripts write intermediate and final objects under `outputs/`, including:

- raw canonical `.rds` copies of each source dataset;
- harmonised target files;
- nested feature-set `.rds` files;
- within-country benchmark results;
- cross-country transfer results;
- overlap-ablation summaries;
- simulation results and variance decomposition;
- compatibility heatmaps, transfer matrices, ablation plots, partial-dependence diagnostics and calibration figures.

Important result files include:

```text
outputs/results/within_country_long.rds
outputs/results/cross_country_transfer_long.rds
outputs/results/overlap_ablation_summary.csv
outputs/results/variance_decomp.csv
outputs/metadata/variable_dictionary.csv
```

## Software requirements

The workflow is written in R. The main packages used across the scripts are:

```r
tidyverse
janitor
sf
digest
readxl
rsample
yardstick
ranger
nnet
MASS
```

Some scripts may require additional plotting or table packages depending on local output settings.

## Data notes

The repository assumes that raw source files are available locally. The raw data files may not be committed to the repository depending on licensing, size or access restrictions.

Before running the pipeline, update the input paths in `00_load_raw_data.R`:

```r
cfg <- list(
  paths = list(
    nepal   = "path/to/Nepal.xlsx",
    turkiye = "path/to/turkey.csv",
    japan   = "path/to/japan.gpkg"
  ),
  out_dir = "outputs/raw"
)
```

The loading script records source-file checksums so downstream outputs can be checked against stale or changed input files.

## Method summary

The empirical pipeline follows four steps.

1. **Compatibility audit**: source variables are classified as directly compatible, convertible, coarse proxies or not compatible.
2. **Within-country modelling**: country-specific models are fitted using fuller national feature sets to establish an upper-bound benchmark.
3. **Cross-country transfer**: models are trained on Nepal and tested on Türkiye, trained on Türkiye and tested on Nepal, and trained on pooled Nepal–Türkiye data with and without a country indicator.
4. **Compatibility simulation**: synthetic data are generated under controlled predictor-overlap, measurement-error and label-noise regimes to compare the relative contribution of data compatibility and model family.

The model families include logistic or multinomial regression, proportional-odds ordinal regression, Random Forest and a feed-forward neural network.

## Interpretation guidance

Several results need careful interpretation:

- The Türkiye sample is small, especially for the highest damage class.
- The Nepal hazard variable is a coarse distance-to-epicentre proxy rather than a harmonised shaking-intensity variable.
- The Türkiye sample is reinforced-concrete-focused and not nationally representative.
- Japan is a measurement-regime contrast, not a third symmetric training domain.
- Random pooled holdout performance should not be interpreted as evidence of true cross-country generalisation.

The appropriate conclusion is not that cross-country damage modelling is impossible. The conclusion is that current cross-country transfer is strongly constrained by variable definitions, hazard comparability, target-label compatibility and measurement protocol.

## Minimum standardised variable core

The paper proposes a compact post-event survey core for improving future data reuse:

- persistent asset identifier;
- point geolocation and, where possible, footprint polygon;
- comparable hazard intensity at asset location, such as PGA, PGV, SA(T1) and/or MMI;
- basic geometry: year built, number of storeys, total height and floor area;
- structural-system category mapped to an international taxonomy such as GED4ALL;
- site condition, such as Vs30 or a standard site class;
- comparable ordinal damage grade and binary severity flag;
- survey metadata, including inspection date, survey type and verification status.

This is the data-governance contribution of the project: transferable modelling requires standardised post-event asset-level data infrastructure, not only more flexible algorithms.

## Citation

If you use this repository, please cite the accompanying manuscript:

```text
Andreozzi, C. (2026). Data Compatibility as a Bottleneck in Cross-Country Earthquake Damage Modelling: Nepal–Türkiye transfer with a Japan measurement-regime contrast.
```

## Author

Camilla Andreozzi  
Email: andreozzicamilla@gmail.com

## License

No license has been specified yet. Until a license is added, reuse is restricted by default. Add an explicit license file, such as MIT, Apache-2.0 or CC-BY-4.0, if public reuse is intended.
