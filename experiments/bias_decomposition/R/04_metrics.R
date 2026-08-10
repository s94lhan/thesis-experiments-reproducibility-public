safe_spearman <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3L || stats::sd(x[ok]) == 0 || stats::sd(y[ok]) == 0) {
    return(NA_real_)
  }
  suppressWarnings(stats::cor(x[ok], y[ok], method = "spearman"))
}

policy_value <- function(d, q0, tau) {
  mean(q0 + d * tau, na.rm = TRUE)
}

calibration_bin_mse <- function(score, target, n_bins = 10) {
  ok <- is.finite(score) & is.finite(target)
  if (sum(ok) < 2L) {
    return(NA_real_)
  }

  score_ok <- score[ok]
  target_ok <- target[ok]
  ord <- order(score_ok)
  n_ok <- length(score_ok)
  n_bins <- min(n_bins, n_ok)
  bin <- cut(seq_len(n_ok), breaks = seq(0, n_ok, length.out = n_bins + 1L),
             include.lowest = TRUE, labels = FALSE)

  mean_score <- tapply(score_ok[ord], bin, mean, na.rm = TRUE)
  mean_target <- tapply(target_ok[ord], bin, mean, na.rm = TRUE)
  bin_n <- as.numeric(table(bin))

  sum((bin_n / sum(bin_n)) * (mean_score - mean_target)^2, na.rm = TRUE)
}

evaluate_policy_metrics <- function(dat, score_raw, score_iso, gamma_target, t_theta) {
  d_oracle <- as.integer(dat$tau > 0)
  d_raw <- as.integer(score_raw > 0)
  d_iso <- as.integer(score_iso > 0)
  raw_treated <- d_raw == 1L
  iso_treated <- d_iso == 1L

  flip <- d_raw != d_iso
  flip_contribution <- (d_iso - d_raw) * dat$tau
  beneficial <- flip & flip_contribution > 0
  harmful <- flip & flip_contribution < 0

  data.frame(
    v_oracle = policy_value(d_oracle, dat$q0, dat$tau),
    v_raw = policy_value(d_raw, dat$q0, dat$tau),
    v_iso = policy_value(d_iso, dat$q0, dat$tau),
    delta_v = policy_value(d_iso, dat$q0, dat$tau) - policy_value(d_raw, dat$q0, dat$tau),
    treat_rate_oracle = mean(d_oracle, na.rm = TRUE),
    treat_rate_raw = mean(d_raw, na.rm = TRUE),
    treat_rate_iso = mean(d_iso, na.rm = TRUE),
    raw_treated_true_positive_share = ifelse(sum(raw_treated) > 0, mean(dat$tau[raw_treated] > 0, na.rm = TRUE), NA_real_),
    raw_treated_false_positive_share = ifelse(sum(raw_treated) > 0, mean(dat$tau[raw_treated] <= 0, na.rm = TRUE), NA_real_),
    iso_treated_true_positive_share = ifelse(sum(iso_treated) > 0, mean(dat$tau[iso_treated] > 0, na.rm = TRUE), NA_real_),
    iso_treated_false_positive_share = ifelse(sum(iso_treated) > 0, mean(dat$tau[iso_treated] <= 0, na.rm = TRUE), NA_real_),
    mse_raw = mean((score_raw - dat$tau)^2, na.rm = TRUE),
    mse_iso = mean((score_iso - dat$tau)^2, na.rm = TRUE),
    cal_raw = calibration_bin_mse(score_raw, gamma_target),
    cal_iso = calibration_bin_mse(score_iso, gamma_target),
    t_theta = t_theta,
    flip_rate = mean(flip, na.rm = TRUE),
    flip_count = sum(flip, na.rm = TRUE),
    beneficial_flip_rate = mean(beneficial, na.rm = TRUE),
    harmful_flip_rate = mean(harmful, na.rm = TRUE),
    beneficial_flip_count = sum(beneficial, na.rm = TRUE),
    harmful_flip_count = sum(harmful, na.rm = TRUE),
    beneficial_sign_share = ifelse(sum(flip) > 0, mean(beneficial[flip]), NA_real_),
    harmful_sign_share = ifelse(sum(flip) > 0, mean(harmful[flip]), NA_real_),
    beneficial_value = sum(pmax(flip_contribution, 0), na.rm = TRUE) / length(flip_contribution),
    harmful_value = sum(pmin(flip_contribution, 0), na.rm = TRUE) / length(flip_contribution),
    mean_tau_flip_region = ifelse(sum(flip) > 0, mean(dat$tau[flip], na.rm = TRUE), NA_real_),
    mean_abs_tau_flip_region = ifelse(sum(flip) > 0, mean(abs(dat$tau[flip]), na.rm = TRUE), NA_real_),
    global_spearman = safe_spearman(score_raw, dat$tau),
    flip_region_spearman = ifelse(sum(flip) >= 3L, safe_spearman(score_raw[flip], dat$tau[flip]), NA_real_)
  )
}

summarise_mechanism_results <- function(dt, by) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Please install data.table first: install.packages('data.table')")
  }

  dt <- data.table::as.data.table(dt)
  numeric_cols <- names(dt)[vapply(dt, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, c("repeat_id", by))

  finite_mean <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0L) {
      return(NA_real_)
    }
    mean(x, na.rm = TRUE)
  }

  summary <- dt[, c(
    lapply(.SD, finite_mean),
    list(n_reps = .N)
  ), by = by, .SDcols = numeric_cols]

  summary[]
}
