# Main Experiment: Three Learners and Causal Isotonic Cross-Calibration

This directory contains the main thesis experiment and generates Figures 4-1 through 4-4.

## Formal design

- Learners: GLMnet, GBRT depth 8, and Random Forest
- Monte Carlo replications: 300
- Training sample sizes: 1,000; 2,000; and 5,000
- Test sample size: 3,000
- Independent gamma sample size: 5,000
- Outer cross-fitting: 10 folds
- Calibration: pooled causal isotonic cross-calibration
- Policy threshold: 0

See `docs/EXPERIMENT_ALIGNMENT.md` in the repository root for the complete configuration.

## Restore the environment

Open `ThreeLearnersIsoCrossCalibration.Rproj` in RStudio and run:

```r
install.packages("renv")
renv::restore()
```

If the Windows system drive is short on space, run `scripts/start_rstudio_on_project_drive.ps1 -Experiment main` from the repository root before restoring the environment.

Without `renv`, the direct installation helper is:

```r
source("INSTALL_PACKAGES.R")
```

## Run from scratch

```r
source("RUN_IN_RSTUDIO.R", encoding = "UTF-8")
check_formal_settings()
train_learners()
calibrate_from_cache()
plot_from_results()
show_run_status()
```

To run all three stages in sequence:

```r
run_full_experiment()
```

The stages perform the following work:

1. `train_learners()` generates the data, performs 10-fold cross-fitting, fits the nuisance Super Learner and three DR CATE learners, and writes the DR caches.
2. `calibrate_from_cache()` reads the DR caches, fits the causal isotonic calibrator, and computes all evaluation metrics.
3. `plot_from_results()` reads the result tables and generates Figures 4-1 through 4-4.

If a run is interrupted, repeat the same stage to resume from the existing checkpoints. Do not use `force = TRUE` during the first complete run.

## Redraw figures without rerunning the experiment

After the formal results have been generated:

```r
source("RUN_IN_RSTUDIO.R", encoding = "UTF-8")
plot_from_results()
```

This command does not retrain the learners or refit calibration.

## Output layout

```text
runs/three_learners_iso_reps300_reduced_nuisance_sl_v1/
├── cache/dr/
├── cache/calibration/
├── results/
├── analysis_data/
└── plots/
```

`runs/` is not committed to GitHub. Compact summary tables and thesis figures from the current formal run are stored in `reference_outputs/`.

## Computational requirements

The formal 300-replication experiment is time- and storage-intensive. Ensure that the repository drive has sufficient free space, then check the configuration and checkpoint state before a long run:

```r
check_formal_settings()
show_run_status()
```

The default worker count is 3. If memory is limited, reduce `--mc-workers` in `experiment_args`. This changes execution speed but should not change the deterministic per-task seeds.

## Third-party dependency

`vendor/original_figure1/` contains the Scenario 1 and causal-calibration support code called by this experiment. Read `THIRD_PARTY_NOTICE.md` in the repository root before public distribution.
