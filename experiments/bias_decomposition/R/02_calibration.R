causal_calibrate_oracle <- function(score, A, Y, EY1, EY0, pA1) {
  EY <- ifelse(A == 1, EY1, EY0)
  pA <- ifelse(A == 1, pA1, 1 - pA1)
  pseudo_outcome <- EY1 - EY0 + (2 * A - 1) / pA * (Y - EY)

  keep <- is.finite(score) & is.finite(pseudo_outcome)
  if (sum(keep) < 3L) {
    stop("Too few finite observations for isotonic calibration.")
  }

  fit_iso <- stats::isoreg(score[keep], pseudo_outcome[keep])
  calibration_function <- stats::as.stepfun(fit_iso)

  list(
    tau_calibrated = as.numeric(calibration_function(score)),
    calibration_function = calibration_function,
    iso_reg_fit = fit_iso
  )
}

predict_iso <- function(calibrator, score) {
  as.numeric(calibrator$calibration_function(score))
}

effective_threshold <- function(calibrator, score, threshold = 0) {
  score <- sort(unique(as.numeric(score[is.finite(score)])))
  if (length(score) == 0L) {
    return(NA_real_)
  }

  span <- diff(range(score))
  if (!is.finite(span) || span <= 0) {
    span <- 1
  }

  grid <- sort(unique(c(min(score) - span, score, threshold, max(score) + span)))
  values <- as.numeric(calibrator$calibration_function(grid))
  positive <- which(values > threshold)

  if (length(positive) == 0L) {
    return(Inf)
  }
  if (positive[1L] == 1L) {
    return(-Inf)
  }

  grid[positive[1L]]
}
