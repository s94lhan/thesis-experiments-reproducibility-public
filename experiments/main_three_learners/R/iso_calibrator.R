lower_median <- function(mat) {
  as.vector(apply(mat, 1, quantile, type = 1, probs = 0.5))
}

fit_causal_iso_calibrator <- function(tau, A, Y, EY1, EY0, pA1) {
  causalCalibrate(tau = tau, A = A, Y = Y, EY1 = EY1, EY0 = EY0, pA1 = pA1)
}

predict_causal_iso <- function(calibrator, tau_mat) {
  cross_calibrate(calibrator, tau_mat)
}

effective_threshold <- function(calibrator, score, threshold = 0) {
  score <- sort(unique(as.numeric(score)))
  calibration_function <- calibrator$calibration_function
  if (length(score) == 0L) {
    return(NA_real_)
  }

  span <- max(diff(range(score)), 1)
  left_probe <- min(score) - span
  right_probe <- max(score) + span
  grid <- sort(unique(c(left_probe, score, threshold, right_probe)))
  values <- calibration_function(grid)
  positive <- which(values > threshold)

  if (length(positive) == 0L) {
    return(Inf)
  }
  if (positive[1] == 1L) {
    return(-Inf)
  }
  grid[positive[1]]
}

calibration_map_rows <- function(calibrator, score) {
  score <- sort(unique(as.numeric(score)))
  data.table::data.table(
    raw_score = score,
    theta_iso = as.numeric(calibrator$calibration_function(score))
  )
}
