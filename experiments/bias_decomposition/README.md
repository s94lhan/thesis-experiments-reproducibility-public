# Bias-Decomposition Mechanism Experiment

This directory contains the raw-score perturbation study and generates Figures 4-5 through 4-9.

## Formal design

- Calibration sample size: 5,000
- Test sample size: 5,000
- Monte Carlo replications: 300
- Seed base: `20260616`
- Calibration target: oracle doubly robust pseudo-outcome
- Policies: `S_raw > 0` and `S_iso > 0`

The study covers ranking error, additive bias, scale distortion, and the ranking-by-additive-bias interaction. See `docs/EXPERIMENT_ALIGNMENT.md` in the repository root for the complete parameter grids.

## Restore the environment

Open `BiasDecompositionExperiment.Rproj` in RStudio and run:

```r
install.packages("renv")
renv::restore()
```

If the Windows system drive is short on space, run `scripts/start_rstudio_on_project_drive.ps1 -Experiment bias` from the repository root before restoring the environment.

Without `renv`, the direct installation helper is:

```r
source("INSTALL_PACKAGES.R")
```

## Quick configuration check

```r
source("RUN_IN_RSTUDIO.R", encoding = "UTF-8")
show_formal_run_settings()
```

The optional smoke test uses a reduced sample size and reduced parameter grids. It verifies that the code path runs successfully but does not produce a formal thesis result:

```r
run_smoke_test(project_root = getwd())
```

## Formal run

```r
source("RUN_IN_RSTUDIO.R", encoding = "UTF-8")
run_formal_bias_decomposition_experiment(project_root = getwd())
```

Formal outputs are written to:

```text
runs/bias_decomposition_n5000_reps300_final_v1/
├── results/
└── plots/
```

## Redraw figures without rerunning the experiment

When the formal `results/` directory already exists:

```r
source("RUN_IN_RSTUDIO.R", encoding = "UTF-8")
redraw_formal_plots(project_root = getwd())
```

This function reads the existing 300-replication results and does not rerun the formal Monte Carlo loops. The display sample for the Figure 4-6 density plot is reconstructed with the original plotting function and fixed seed; the formal result tables are not modified.

## Reference outputs

`reference_outputs/results/` contains compact detail and summary tables from the formal thesis run. `reference_outputs/plots/` contains Figures 4-5 through 4-9. Complete outputs from any new run are written under the Git-ignored `runs/` directory.
