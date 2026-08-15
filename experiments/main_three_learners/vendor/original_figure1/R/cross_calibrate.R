cross_calibrate <- function(output, new_tau_mat) {
  calibration_function <- output$calibration_function
  tau_mat_cal <- apply(new_tau_mat, 2, calibration_function)

  tau_cal <- as.vector(apply(tau_mat_cal, 1, quantile, type = 1, probs = 0.5))
  return(tau_cal)
}
