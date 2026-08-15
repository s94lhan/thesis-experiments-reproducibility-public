# Bias-Decomposition Experiment

This experiment studies ranking error, additive bias, scale distortion, and the interaction between ranking error and additive bias. It generates Figures 5-9 of the thesis.

## Formal settings

- Calibration sample size: 5,000
- Test sample size: 5,000
- Monte Carlo replications: 300
- Seed base: 20260616
- Ranking-noise standard deviations: 0, 0.02, 0.05, 0.08, and 0.12
- Additive shifts: -0.50, -0.25, -0.10, 0, 0.10, 0.25, and 0.50
- Scale factors: 0.25, 0.50, 0.75, 1.00, 1.25, 1.50, and 2.00
- Interaction shifts: -0.10 and 0.10 across the ranking-noise grid
- Calibration target: oracle doubly robust pseudo-outcome
- Treatment rules: `S_raw > 0` and `S_iso > 0`

## Run

Open `BiasDecompositionExperiment.Rproj`, restore the locked environment, and run:

```r
install.packages("renv")
renv::restore(project = getwd())
source("RUN_IN_RSTUDIO.R", encoding = "UTF-8")
show_formal_run_settings()
run_formal_bias_decomposition_experiment(project_root = getwd())
```

Outputs are written to `runs/bias_decomposition_n5000_reps300_final_v1/`. The `runs/` directory is excluded from Git. Compact reference results and Figures 5-9 are in `reference_outputs/`.

To redraw the figures from completed results without rerunning the Monte Carlo simulation:

```r
redraw_formal_plots(project_root = getwd())
```
