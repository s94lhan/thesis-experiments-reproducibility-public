# Reproducible Thesis Simulation Experiments

This repository contains two R simulation studies from a graduate thesis. It is organized so that reviewers and collaborators can reproduce the reported results and Figures 4-1 through 4-9 from the released code.

## Repository layout

```text
experiments/
├── main_three_learners/   # GLMnet, GBRT 8, RF, and causal isotonic cross-calibration
└── bias_decomposition/    # Ranking error, additive bias, scale bias, and interactions
docs/
├── EXPERIMENT_ALIGNMENT.md
└── GITHUB_UPLOAD_GUIDE.md
scripts/
├── check_repository.R
└── start_rstudio_on_project_drive.ps1
FILE_MANIFEST.csv           # File sizes and SHA-256 checksums
```

Both formal studies use 300 Monte Carlo replications. See [Experiment alignment](docs/EXPERIMENT_ALIGNMENT.md) for the exact settings and the mapping between code outputs and thesis figures.

## Quick start

1. Install R 4.6.0, or a compatible version, and RStudio.
2. Enter the directory for the experiment you want to reproduce.
3. Install `renv` and restore the locked package environment:

```r
install.packages("renv")
renv::restore()
```

4. Open the experiment's `.Rproj` file and follow its README.

### Keep runtime files off the system drive

On Windows, the launcher below stores R temporary files and the `renv` cache under `Codex work\R-runtime` on the same drive as this repository. It then opens the selected RStudio project without changing any experiment setting.

Run one command at a time from the repository root:

```powershell
.\scripts\start_rstudio_on_project_drive.ps1 -Experiment main
.\scripts\start_rstudio_on_project_drive.ps1 -Experiment bias
```

Formal results are still written to the experiment's project-local `runs/` directory.

## Formal entry points

```text
experiments/main_three_learners/RUN_IN_RSTUDIO.R
experiments/bias_decomposition/RUN_IN_RSTUDIO.R
```

## Preflight check

Run this command from the repository root:

```powershell
Rscript scripts/check_repository.R
```

The check parses all R source files, verifies the formal settings and reference outputs, and checks the manifest file sizes. It does not run a simulation.

`FILE_MANIFEST.csv` records each released file's size and SHA-256 checksum. The manifest does not include itself.

## Computation and outputs

The complete 300-replication main experiment is computationally intensive and produces large per-observation results and model caches. New results are written under each experiment's `runs/` directory. These directories are excluded by `.gitignore` and should not be committed to GitHub.

The tracked `reference_outputs/` directories contain only compact summary tables and the thesis figures. They are supplied for result comparison and are not required inputs for a new simulation run.

## Reproducibility scope

- Formal parameters, seeds, learners, calibration procedures, evaluation metrics, and plotting code match the thesis experiment release.
- Packaging changes are limited to replacing machine-specific paths with project-relative paths, aligning stale 5/100-replication convenience defaults with the formal 300-replication run, and adding release documentation and checks.
- The statistical data-generating mechanisms, estimators, calibration logic, metrics, and plot definitions were not changed.
- Operating-system, BLAS, parallel-scheduling, or underlying tree-library differences can cause small floating-point differences. Aggregate results and qualitative figure patterns should remain consistent.

## Third-party code and public release

The main experiment vendors a snapshot of upstream `causalCalibration` support code under `vendor/original_figure1/`. The locally obtained upstream `DESCRIPTION` file does not declare a license. Review [THIRD_PARTY_NOTICE.md](THIRD_PARTY_NOTICE.md) and confirm redistribution permission before making this repository public.

See the [GitHub publication guide](docs/GITHUB_UPLOAD_GUIDE.md) for the release workflow.
