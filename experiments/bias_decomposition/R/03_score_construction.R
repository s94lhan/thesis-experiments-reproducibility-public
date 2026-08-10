threshold_bandwidth <- function(x, q = 0.10) {
  stats::quantile(abs(x), probs = q, na.rm = TRUE, names = FALSE)
}

threshold_weight <- function(x, h) {
  exp(-abs(x) / h)
}

construct_sorting_score <- function(tau, sigma, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Fixed raw-score marginal distribution: only the assignment of score values
  # to individuals changes as ranking noise increases.
  fixed_scores <- sort(tau)
  noisy_rank <- rank(tau + stats::rnorm(length(tau), mean = 0, sd = sigma), ties.method = "first")
  fixed_scores[noisy_rank]
}

construct_additive_bias_score <- function(tau, a) {
  tau + a
}

construct_sorting_additive_bias_score <- function(tau, sigma, a, seed = NULL) {
  construct_sorting_score(tau = tau, sigma = sigma, seed = seed) + a
}

construct_slope_bias_score <- function(tau, a = 0, b = 1) {
  a + b * tau
}

score_rmse <- function(score, tau) {
  sqrt(mean((score - tau)^2, na.rm = TRUE))
}
