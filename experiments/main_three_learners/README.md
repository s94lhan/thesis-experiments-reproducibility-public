# Three-Learner Causal Isotonic Experiment

This experiment compares raw and causally calibrated CATE scores from GLMnet, gradient-boosted regression trees, and random forests. It generates Figures 1-4 of the thesis.

## Formal settings

- Monte Carlo replications: 300
- Training sample sizes: 1,000, 2,000, and 5,000
- Test sample size: 3,000
- Independent gamma sample size: 5,000
- Outer cross-fitting: 10 folds
- GBRT: depth 8, 2,000 trees, shrinkage 0.005
- Random forest: 300 trees
- Calibration: pooled causal isotonic cross-calibration
- Treatment threshold: 0

## Run

Open `ThreeLearnersIsoCrossCalibration.Rproj`, restore the locked environment, and run:

```r
install.packages("renv")
renv::restore(project = getwd())
source("RUN_IN_RSTUDIO.R", encoding = "UTF-8")
check_formal_settings()
run_full_experiment()
```

The run can also be restarted by stage:

```r
train_learners()
calibrate_from_cache()
plot_from_results()
show_run_status()
```

Outputs are written to `runs/three_learners_iso_reps300_reduced_nuisance_sl_v1/`. The `runs/` directory is excluded from Git. Compact reference results and Figures 1-4 are in `reference_outputs/`.

To redraw the figures from completed results without retraining or recalibrating, run `plot_from_results()`.

The calibration support code follows van der Laan et al. (2023), *Causal Isotonic Calibration for Heterogeneous Treatment Effects*. Provenance is recorded in `../../THIRD_PARTY_NOTICE.md`.
