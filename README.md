# Thesis Simulation Experiments

This repository contains the reproducibility materials for two simulation studies in the thesis *The Impact of Causal Isotonic Cross-Calibration on Fixed-Threshold Treatment Decisions*.

Both formal experiments use 300 Monte Carlo replications. The statistical design, estimators, calibration procedures, and evaluation metrics are unchanged from the thesis analyses.

## Experiments

| Directory | Study | Thesis figures |
| --- | --- | --- |
| `experiments/main_three_learners/` | GLMnet, gradient-boosted regression trees, random forests, and pooled causal isotonic cross-calibration | Figures 1-4 |
| `experiments/bias_decomposition/` | Ranking error, additive bias, scale distortion, and their interaction | Figures 5-9 |

Each experiment contains its own RStudio project, package lockfile, formal run script, source code, README, and compact reference outputs.

## Requirements

- R 4.6.0, or a compatible version
- RStudio is optional but recommended
- The R packages recorded in each experiment's `renv.lock`

Open the selected experiment directory and restore its package environment:

```r
install.packages("renv")
renv::restore(project = getwd())
```

## Run the formal experiments

Main three-learner experiment:

```r
source("RUN_IN_RSTUDIO.R", encoding = "UTF-8")
check_formal_settings()
run_full_experiment()
```

Bias-decomposition experiment:

```r
source("RUN_IN_RSTUDIO.R", encoding = "UTF-8")
show_formal_run_settings()
run_formal_bias_decomposition_experiment(project_root = getwd())
```

Formal output is written to each experiment's Git-ignored `runs/` directory. The full simulations are computationally intensive; interrupted main-experiment stages can be restarted and matching completed caches are reused.

## Reference outputs

The tracked `reference_outputs/` directories contain compact result tables and the nine thesis figures from the formal 300-replication runs. They can be used to compare a new run with the reported results and are not inputs to a new simulation.

Each experiment README also explains how to redraw figures from completed results without rerunning the Monte Carlo simulation.

## Repository check

From the repository root, run:

```powershell
Rscript scripts/check_repository.R
```

This parses the R source files, verifies the two formal configurations, and checks that all nine reference figures are present. It does not run either simulation.

## Reproducibility notes

Package versions are fixed by the two `renv.lock` files. Small numerical differences may arise across operating systems, BLAS implementations, parallel schedules, or package builds, but the aggregate results and figure patterns should remain comparable.

The main experiment includes a fixed snapshot of third-party calibration support code. See [THIRD_PARTY_NOTICE.md](THIRD_PARTY_NOTICE.md) for provenance.
