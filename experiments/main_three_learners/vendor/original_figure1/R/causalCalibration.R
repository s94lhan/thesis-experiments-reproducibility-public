causalCalibrate <- function(tau, A, Y, EY1, EY0, pA1,  tau_pred = tau) {

  EY <- ifelse(A==1, EY1, EY0)
  pA <- ifelse(A==1, pA1, 1-pA1)

  pseudo_outcome <- EY1 - EY0 + (2*A-1)/pA * (Y - EY)

  fit_iso <- isoreg(tau, pseudo_outcome)

  calibration_function <- as.stepfun(fit_iso)

  return(list( tau_calibrated = calibration_function(tau_pred), calibration_function = calibration_function, iso_reg_fit = fit_iso  ))
}
