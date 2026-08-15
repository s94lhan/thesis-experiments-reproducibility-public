stable_seed <- function(seed_base, ...) {
  key <- paste(..., collapse = "_")
  ints <- utf8ToInt(key)
  val <- sum(ints * seq_along(ints))
  as.integer((seed_base + val) %% .Machine$integer.max)
}

bias_decomposition_options <- function(
  n = 5000,
  reps = 300,
  seed_base = 20260616L,
  run_id = "bias_decomposition_n5000_reps300_final_v1",
  project_root = getwd(),
  sorting_sigmas = c(0, 0.02, 0.05, 0.08, 0.12),
  additive_grid = c(-0.50, -0.25, -0.10, 0, 0.10, 0.25, 0.50),
  interaction_sigmas = sorting_sigmas,
  interaction_additive_grid = c(-0.10, 0.10),
  slope_grid = c(0.25, 0.50, 0.75, 1.00, 1.25, 1.50, 2.00)
) {
  list(
    n = n,
    reps = reps,
    seed_base = seed_base,
    run_id = run_id,
    project_root = project_root,
    sorting_sigmas = sorting_sigmas,
    additive_grid = additive_grid,
    interaction_sigmas = interaction_sigmas,
    interaction_additive_grid = interaction_additive_grid,
    slope_grid = slope_grid
  )
}

ensure_bias_decomposition_paths <- function(opts) {
  base <- file.path(opts$project_root, "runs", opts$run_id)
  paths <- list(
    base = base,
    results = file.path(base, "results"),
    plots = file.path(base, "plots")
  )
  invisible(lapply(paths, dir.create, recursive = TRUE, showWarnings = FALSE))
  paths
}

fit_oracle_calibrator <- function(cal_dat, score_cal) {
  causal_calibrate_oracle(
    score = score_cal,
    A = cal_dat$A,
    Y = cal_dat$Y,
    EY1 = cal_dat$q1,
    EY0 = cal_dat$q0,
    pA1 = cal_dat$g
  )
}

evaluate_with_calibrator <- function(eval_dat, score_eval, calibrator) {
  score_iso <- predict_iso(calibrator, score_eval)
  gamma_eval <- oracle_dr_pseudo_outcome(eval_dat)
  t_theta <- effective_threshold(calibrator, score_eval)

  evaluate_policy_metrics(
    dat = eval_dat,
    score_raw = score_eval,
    score_iso = score_iso,
    gamma_target = gamma_eval,
    t_theta = t_theta
  )
}

run_one_sorting <- function(repeat_id, sigma, opts) {
  pair <- generate_cal_eval_pair(n = opts$n, repeat_id = repeat_id, seed_base = opts$seed_base)

  score_cal <- construct_sorting_score(
    tau = pair$cal$tau,
    sigma = sigma,
    seed = stable_seed(opts$seed_base, "sorting", "cal", repeat_id, sigma)
  )
  calibrator <- fit_oracle_calibrator(pair$cal, score_cal)
  score_eval <- construct_sorting_score(
    tau = pair$eval$tau,
    sigma = sigma,
    seed = stable_seed(opts$seed_base, "sorting", "eval", repeat_id, sigma)
  )

  cbind(
    data.frame(
      experiment = "sorting",
      repeat_id = repeat_id,
      sigma = sigma
    ),
    evaluate_with_calibrator(pair$eval, score_eval, calibrator)
  )
}

run_one_additive_bias <- function(repeat_id, a, opts) {
  pair <- generate_cal_eval_pair(n = opts$n, repeat_id = repeat_id, seed_base = opts$seed_base)

  score_cal <- construct_additive_bias_score(pair$cal$tau, a = a)
  calibrator <- fit_oracle_calibrator(pair$cal, score_cal)
  score_eval <- construct_additive_bias_score(pair$eval$tau, a = a)

  cbind(
    data.frame(
      experiment = "additive_bias",
      repeat_id = repeat_id,
      a = a,
      direction = ifelse(a > 0, "score_high_bias", ifelse(a < 0, "score_low_bias", "no_bias"))
    ),
    evaluate_with_calibrator(pair$eval, score_eval, calibrator)
  )
}

run_one_sorting_additive_bias <- function(repeat_id, sigma, a, opts) {
  pair <- generate_cal_eval_pair(n = opts$n, repeat_id = repeat_id, seed_base = opts$seed_base)

  score_cal <- construct_sorting_additive_bias_score(
    tau = pair$cal$tau,
    sigma = sigma,
    a = a,
    seed = stable_seed(opts$seed_base, "sorting_additive", "cal", repeat_id, sigma)
  )
  calibrator <- fit_oracle_calibrator(pair$cal, score_cal)
  score_eval <- construct_sorting_additive_bias_score(
    tau = pair$eval$tau,
    sigma = sigma,
    a = a,
    seed = stable_seed(opts$seed_base, "sorting_additive", "eval", repeat_id, sigma)
  )

  cbind(
    data.frame(
      experiment = "sorting_additive_bias",
      repeat_id = repeat_id,
      sigma = sigma,
      a = a,
      direction = ifelse(a > 0, "score_high_bias", ifelse(a < 0, "score_low_bias", "no_bias"))
    ),
    evaluate_with_calibrator(pair$eval, score_eval, calibrator)
  )
}

run_one_slope_bias <- function(repeat_id, b, opts) {
  pair <- generate_cal_eval_pair(n = opts$n, repeat_id = repeat_id, seed_base = opts$seed_base)

  score_cal <- construct_slope_bias_score(pair$cal$tau, a = 0, b = b)
  calibrator <- fit_oracle_calibrator(pair$cal, score_cal)
  score_eval <- construct_slope_bias_score(pair$eval$tau, a = 0, b = b)

  cbind(
    data.frame(
      experiment = "slope_bias",
      repeat_id = repeat_id,
      b = b,
      scale_state = ifelse(b > 1, "expanded", ifelse(b < 1, "compressed", "identity_scale"))
    ),
    evaluate_with_calibrator(pair$eval, score_eval, calibrator)
  )
}

run_sorting_experiments <- function(opts = bias_decomposition_options()) {
  results <- list()
  k <- 1L
  total <- opts$reps * length(opts$sorting_sigmas)

  for (r in seq_len(opts$reps)) {
    for (sigma in opts$sorting_sigmas) {
      message(sprintf("[sorting %d/%d] r=%d, sigma=%.3f", k, total, r, sigma))
      results[[k]] <- run_one_sorting(r, sigma, opts)
      k <- k + 1L
    }
  }

  data.table::rbindlist(results, fill = TRUE)
}

run_additive_bias_experiments <- function(opts = bias_decomposition_options()) {
  results <- list()
  k <- 1L
  total <- opts$reps * length(opts$additive_grid)

  for (r in seq_len(opts$reps)) {
    for (a in opts$additive_grid) {
      message(sprintf("[additive bias %d/%d] r=%d, a=%.3f", k, total, r, a))
      results[[k]] <- run_one_additive_bias(r, a, opts)
      k <- k + 1L
    }
  }

  data.table::rbindlist(results, fill = TRUE)
}

run_sorting_additive_bias_experiments <- function(opts = bias_decomposition_options()) {
  results <- list()
  k <- 1L
  total <- opts$reps * length(opts$interaction_sigmas) * length(opts$interaction_additive_grid)

  for (r in seq_len(opts$reps)) {
    for (sigma in opts$interaction_sigmas) {
      for (a in opts$interaction_additive_grid) {
        message(sprintf(
          "[sorting x additive bias %d/%d] r=%d, sigma=%.3f, a=%.3f",
          k, total, r, sigma, a
        ))
        results[[k]] <- run_one_sorting_additive_bias(r, sigma, a, opts)
        k <- k + 1L
      }
    }
  }

  data.table::rbindlist(results, fill = TRUE)
}

run_slope_bias_experiments <- function(opts = bias_decomposition_options()) {
  results <- list()
  k <- 1L
  total <- opts$reps * length(opts$slope_grid)

  for (r in seq_len(opts$reps)) {
    for (b in opts$slope_grid) {
      message(sprintf("[slope bias %d/%d] r=%d, b=%.3f", k, total, r, b))
      results[[k]] <- run_one_slope_bias(r, b, opts)
      k <- k + 1L
    }
  }

  data.table::rbindlist(results, fill = TRUE)
}

run_sorting_additive_bias_module <- function(opts = bias_decomposition_options()) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Please install data.table first: install.packages('data.table')")
  }

  paths <- ensure_bias_decomposition_paths(opts)
  opts$results_dir <- paths$results
  opts$plots_dir <- paths$plots

  detail <- run_sorting_additive_bias_experiments(opts)
  summary <- summarise_mechanism_results(detail, by = c("sigma", "a", "direction"))

  data.table::fwrite(detail, file.path(paths$results, "sorting_additive_bias_detail.csv"))
  data.table::fwrite(summary, file.path(paths$results, "sorting_additive_bias_summary.csv"))

  if (exists("make_sorting_additive_bias_plots", mode = "function")) {
    make_sorting_additive_bias_plots(opts)
  }

  invisible(list(
    sorting_additive_detail = detail,
    sorting_additive_summary = summary,
    paths = paths
  ))
}

run_all_bias_decomposition_experiments <- function(opts = bias_decomposition_options()) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Please install data.table first: install.packages('data.table')")
  }

  paths <- ensure_bias_decomposition_paths(opts)
  opts$results_dir <- paths$results
  opts$plots_dir <- paths$plots

  sorting_detail <- run_sorting_experiments(opts)
  additive_detail <- run_additive_bias_experiments(opts)
  sorting_additive_detail <- run_sorting_additive_bias_experiments(opts)
  slope_detail <- run_slope_bias_experiments(opts)

  sorting_summary <- summarise_mechanism_results(sorting_detail, by = c("sigma"))
  additive_summary <- summarise_mechanism_results(additive_detail, by = c("a", "direction"))
  sorting_additive_summary <- summarise_mechanism_results(
    sorting_additive_detail,
    by = c("sigma", "a", "direction")
  )
  slope_summary <- summarise_mechanism_results(slope_detail, by = c("b", "scale_state"))

  data.table::fwrite(sorting_detail, file.path(paths$results, "sorting_detail.csv"))
  data.table::fwrite(sorting_summary, file.path(paths$results, "sorting_summary.csv"))
  data.table::fwrite(additive_detail, file.path(paths$results, "additive_bias_detail.csv"))
  data.table::fwrite(additive_summary, file.path(paths$results, "additive_bias_summary.csv"))
  data.table::fwrite(sorting_additive_detail, file.path(paths$results, "sorting_additive_bias_detail.csv"))
  data.table::fwrite(sorting_additive_summary, file.path(paths$results, "sorting_additive_bias_summary.csv"))
  data.table::fwrite(slope_detail, file.path(paths$results, "slope_bias_detail.csv"))
  data.table::fwrite(slope_summary, file.path(paths$results, "slope_bias_summary.csv"))

  if (exists("make_bias_decomposition_plots", mode = "function")) {
    make_bias_decomposition_plots(opts)
  }

  invisible(list(
    sorting_detail = sorting_detail,
    sorting_summary = sorting_summary,
    additive_detail = additive_detail,
    additive_summary = additive_summary,
    sorting_additive_detail = sorting_additive_detail,
    sorting_additive_summary = sorting_additive_summary,
    slope_detail = slope_detail,
    slope_summary = slope_summary,
    paths = paths
  ))
}
