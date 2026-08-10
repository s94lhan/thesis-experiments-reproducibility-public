# Thesis-to-Code Experiment Alignment

This document records how the release package corresponds to the simulation design in Chapter 3 and the result figures in Chapter 4 of the thesis. Values supplied by the formal run entry points take precedence over convenience defaults inside helper functions.

## Shared settings

- Monte Carlo replications: 300
- Data-generating process: the thesis implementation of Scenario 1 from van der Laan et al. (2023)
- Policy threshold: 0
- Score-level metrics: MSE, CAL, and Spearman rank correlation
- Policy-level metrics: policy value, treatment rate, decision flips, and flip-value contributions

## Experiment 1: Three Learners and Causal Isotonic Cross-Calibration

Code directory: `experiments/main_three_learners/`

| Item | Formal setting |
|---|---|
| Training sample sizes | 1,000; 2,000; 5,000 |
| Monte Carlo replications | 300 |
| Outer cross-fitting | 10 folds |
| Test sample size | 3,000 |
| Independent gamma sample | 5,000 |
| CATE learners | GLMnet; GBRT depth 8; Random Forest |
| Outcome nuisance library | GLMnet; GBRT depth 8; Random Forest |
| Propensity nuisance library | GLM; GLMnet; GBRT depth 2 |
| Super Learner metalearner | Non-negative least squares |
| GBRT | 2,000 trees; shrinkage 0.005; CATE learner depth 8 |
| Random Forest | 300 trees |
| Calibration | Algorithm 3 pooled causal isotonic cross-calibration |
| Across-fold aggregation | Pointwise lower median of 10 fold-specific predictions |
| CAL trend evaluator | 200 trees; depth 3 |
| Parallel workers | 3 |
| Formal run ID | `three_learners_iso_reps300_reduced_nuisance_sl_v1` |

Each `(sample size, replication)` task uses the deterministic seed

```text
2026 + 100000 * repeat_id + n
```

The test and gamma samples use fixed offsets from that task seed.

### Thesis figure mapping

- Figure 4-1: CATE score-level performance and policy-value changes
- Figure 4-2: Raw-score-quintile distributions of true CATE, raw score, and calibrated score
- Figure 4-3: Effective raw-score threshold and treatment rate
- Figure 4-4: Policy-value contributions from beneficial and harmful decision flips

## Experiment 2: Raw-Score Bias Decomposition

Code directory: `experiments/bias_decomposition/`

Each replication independently generates a calibration sample and a test sample, each with 5,000 observations. This study does not retrain an estimated CATE learner. Instead, it constructs raw scores from the true CATE and fits causal isotonic calibration using the oracle doubly robust pseudo-outcome.

| Subexperiment | Formal parameter grid |
|---|---|
| Ranking error | `sigma = 0, 0.02, 0.05, 0.08, 0.12` |
| Additive bias | `a = -0.50, -0.25, -0.10, 0, 0.10, 0.25, 0.50` |
| Scale distortion | `b = 0.25, 0.50, 0.75, 1.00, 1.25, 1.50, 2.00` |
| Ranking by additive-bias interaction | The sigma grid above with `a = -0.10, 0.10` |
| Monte Carlo replications | 300 |
| Seed base | `20260616` |
| Formal run ID | `bias_decomposition_n5000_reps300_final_v1` |

### Thesis figure mapping

- Figure 4-5: Ranking-error experiment
- Figure 4-6: Score-distribution correction under additive bias
- Figure 4-7: Scale-bias experiment
- Figure 4-8: Sorting-error cross-check under negative additive bias (`a = -0.10`)
- Figure 4-9: Sorting-error cross-check under positive additive bias (`a = 0.10`)

## Non-statistical packaging changes

The following changes improve GitHub portability without changing the statistical experiment:

- Replaced machine-specific absolute paths with paths resolved from the opened RStudio Project.
- Aligned the main experiment's convenience run ID and replication defaults with the formal 300-replication entry point.
- Updated the default run ID used by the analysis-data reader.
- Added formal-setting checks, a reduced smoke test, and plot-only wrappers.
- Added `renv.lock`, English documentation, ignore rules, reference outputs, and a checksum manifest.
