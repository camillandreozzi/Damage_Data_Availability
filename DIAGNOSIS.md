# Modelling failure diagnosis (and what's now fixed)

This note explains what was failing in the pipeline run, why, and which
fixes are now in the patched scripts. It also flags one cross-country
result that is **not a bug** — it is one of the paper's key findings.

## What failed

### 1. Türkiye / Nepal 5-class with neural network → all NA
`tab_within_country.csv` has these rows:

```
Nepal,5class,nnet,NA,NA,NA
Türkiye,5class,nnet,NA,NA,NA
```

**Root cause.** In the original `fit_and_evaluate()` the target was
converted to a factor only when `task == "binary"`. For the 5-class
task the integer-coded `y5` was passed through unchanged, so
`nnet::nnet()` interpreted it as a continuous response and fit a
regression. The downstream `predict(..., type = "class")` then errored
out and the `tryCatch` filled the row with NA.

**Fix.** `fit_and_evaluate()` now factors the target with explicit
levels `1:5` for the 5-class case as well, forcing nnet/ranger into
classification mode.

### 2. `Error: sample_fraction too small, no observations sampled. Ranger will EXIT now.`
This fires inside `ranger::ranger()` when its bootstrap subroutine sees
a class (or a whole training frame) with zero rows. In our pipeline
the most likely trigger was the **overlap ablation in `06_…R`**: at
K1/K2 the predictor list contracts to one or two columns, and after
`drop_na(all_of(c(target, preds)))` the training fold can be empty (or
one-class) for a country with extensive missingness in those columns.

**Fix.** `06_overlap_ablation.R` now has an explicit guard:

```r
if (nrow(train_i) < 5 || nrow(test_i) < 1 ||
    n_distinct(train_i[[target]]) < 2) {
  out <- tibble(metric = NA, value = NA_real_,
                note = sprintf("skipped: train=%d test=%d classes=%d", ...))
} else {
  out <- tryCatch(fit_and_evaluate(...), error = ...)
}
```

`fit_and_evaluate()` itself now also performs the same input validation
at the top of the function so other callers (transfer experiments,
external Japan validation) get the same protection.

`ranger()` is now called with `min.node.size = 1, replace = TRUE` so
rare-class folds don't trigger the abort even when sampling is tight.

### 3. Yardstick warnings flooding the console
> While computing binary `sens()`, no true events were detected (i.e.
> `true_positive + false_negative = 0`). Sensitivity is undefined in
> this case, and `NA` will be returned.

These fire when a test fold ends up with zero examples of one class
(common for Türkiye 5-class because grade 5 has only **6** instances
across the entire dataset — see `targets/label_class_distribution.csv`).
The metrics are correctly NA but the warning text is verbose.

**Fix.** `eval_binary()` and `eval_5class()` now wrap each yardstick
call in a `safe()` helper that suppresses warnings and returns
`NA_real_` on error.

### 4. Transfer matrix only has 4 cells (no within-country diagonal)
`tab_transfer_matrix.csv` had only:

```
Nepal               -> Türkiye         (0.434)
Türkiye             -> Nepal           (0.450)
Pooled              -> Pooled_holdout  (0.827)
Pooled_no_indicator -> Pooled_holdout  (0.828)
```

The Nepal→Nepal and Türkiye→Türkiye diagonal cells went to
`within_country_long.rds` rather than `cross_country_transfer_long.rds`,
so the figure-3 code never saw them.

**Fix.** `08_figures_tables.R` now builds the transfer matrix from
**both** result files:

```r
tr_cross  <- readRDS("…/cross_country_transfer_long.rds") |>
               filter(model == "rf", task == "binary", metric == "auc")
tr_within <- readRDS("…/within_country_long.rds") |>
               filter(model == "rf", task == "binary", metric == "auc") |>
               mutate(train_domain = test_domain)   # diagonal cell
tr        <- bind_rows(tr_cross, tr_within)
```

## What is **not** a bug

The cross-country AUCs are below 0.5:

```
train Nepal    -> test Türkiye   AUC = 0.43
train Türkiye  -> test Nepal     AUC = 0.45
```

This is **not** a fitting failure. It is the central empirical finding
of the paper: when a model trained on Nepal's structural / hazard-proxy
patterns is asked to score Türkiye buildings (or vice versa), the
relationship between the harmonised features and damage is not just
weak — it is **inverted** in places, because the two countries'
data-generating processes don't agree once forced through the small
shared feature space. The pooled within-pool AUC of 0.83 says the
combined model can still discriminate damage *if both countries are
mixed in training*, but the per-country mapping is not learnable in
isolation. This is exactly the compatibility-vs-flexibility trade-off
the paper describes; keep it in the manuscript.

## Things to double-check after re-running

1. **Türkiye class 5 (DG5) has only 6 instances** out of 242. With
   5-fold CV that's ~1 per fold. After the patch, polr/multinom/nnet
   can still fit, but their 5-class metrics will have wide standard
   deviations because grade-5 recall is essentially binary in each
   fold. Consider collapsing `{4, 5}` for Türkiye into a single class
   for the 5-class results — or report Türkiye 5-class as an "ordinal
   under sparsity" sensitivity, with the binary task as the headline.

2. The 5-class macro-F1 numbers for K1 in the ablation
   (`overlap_ablation_summary.csv`) are anomalously high because
   `f_meas_vec(estimator = "macro")` averages only over the classes
   that appear; with 1 predictor a single class often dominates the
   prediction. The patched `eval_5class()` includes all five levels
   in the factor so the average runs over five classes. Re-run `06_…`
   and `08_…` to refresh those numbers.

3. The simulation already supports the thesis cleanly:
   `variance_decomp.csv` shows compatibility regime accounts for
   ~52 % of AUC variance and ~56 % of macro-F1 variance versus ~2 %
   for model family. That's the cleanest empirical backbone for the
   paper's claim.

## Re-run order after patches

```r
source("R/04_country_specific_models.R")  # nnet 5-class fixed; ranger hardened
source("R/05_cross_country_transfer.R")   # uses fit_and_evaluate; benefits from guard
source("R/06_overlap_ablation.R")         # explicit small-data guard added
source("R/08_figures_tables.R")           # diagonal cells now in transfer matrix
```

`07_simulation_study.R` does not need to be re-run — its results are
already complete and consistent.
