fit_nuisance_for_dgp <- function(dat, idx, seed, outcome_type, dgp) {
  formal_names <- names(formals(fit_nuisance))
  args <- list(dat = dat, idx = idx, seed = seed)
  if ("outcome_type" %in% formal_names) {
    args$outcome_type <- outcome_type
  }
  if ("dgp" %in% formal_names) {
    args$dgp <- dgp
  }
  do.call(fit_nuisance, args)
}

fit_all_learners_for_cache <- function(dat, learner_set, folds_list, n, outcome_type = "binomial", dgp = "scenario1") {
  n_obs <- nrow(dat$W)
  tau_oof <- matrix(NA_real_, nrow = n_obs, ncol = length(learner_set))
  colnames(tau_oof) <- learner_set
  q1_oof <- numeric(n_obs)
  q0_oof <- numeric(n_obs)
  g_oof <- numeric(n_obs)
  fold_models <- setNames(vector("list", length(learner_set)), learner_set)
  for (learner in learner_set) {
    fold_models[[learner]] <- vector("list", length(folds_list))
  }

  for (fold_id in seq_along(folds_list)) {
    valid <- folds_list[[fold_id]]
    train <- setdiff(seq_len(n_obs), valid)
    message("  nuisance fold ", fold_id, "/", length(folds_list))
    nuisance_fit <- fit_nuisance_for_dgp(
      dat,
      train,
      seed = 50000 + 1000 * fold_id + n,
      outcome_type = outcome_type,
      dgp = dgp
    )
    train_nuisance <- predict_nuisance(nuisance_fit, dat$W[train, , drop = FALSE])
    valid_nuisance <- predict_nuisance(nuisance_fit, dat$W[valid, , drop = FALSE])
    zeta_train <- make_pseudo_outcome(
      dat$A[train],
      dat$Y[train],
      train_nuisance$q1,
      train_nuisance$q0,
      train_nuisance$g
    )
    q1_oof[valid] <- valid_nuisance$q1
    q0_oof[valid] <- valid_nuisance$q0
    g_oof[valid] <- valid_nuisance$g

    for (learner in learner_set) {
      message("    CATE learner: ", learner)
      set.seed(60000 + 1000 * fold_id + n + 100 * match(learner, learner_set))
      model <- fit_learner(learner, dat$W[train, , drop = FALSE], zeta_train)
      tau_oof[valid, learner] <- predict_learner(model, learner, dat$W[valid, , drop = FALSE])
      fold_models[[learner]][[fold_id]] <- model
    }
  }

  list(
    tau_oof = tau_oof,
    q1_oof = q1_oof,
    q0_oof = q0_oof,
    g_oof = g_oof,
    fold_models = fold_models
  )
}

train_one_cached_task <- function(task, opts) {
  task <- as.list(task)
  path <- dr_cache_path(task, opts)
  signature <- train_signature(task, opts)
  if (!opts$force_train && valid_cache(path, signature)) {
    return(list(task = task_label(task), status = "cached", path = path))
  }

  n <- as.integer(task$sample_size)
  repeat_id <- as.integer(task$repeat_id)
  seed <- 2026 + 100000 * repeat_id + n
  message("[train ", task_label(task), "]")
  dgp <- opts_dgp(opts)
  dat <- generate_dgp_sample(dgp, n, seed = seed)
  folds_list <- make_folds(n, opts$folds, seed = seed + 11)
  fold_id <- integer(n)
  for (fold in seq_along(folds_list)) {
    fold_id[folds_list[[fold]]] <- fold
  }
  fitted <- fit_all_learners_for_cache(
    dat,
    opts$learner_set,
    folds_list,
    n,
    outcome_type = dgp_outcome_type(opts),
    dgp = dgp
  )
  test <- generate_dgp_sample(dgp, opts$test_n, seed = seed + 37, draw_outcome = FALSE)
  gamma <- generate_dgp_sample(dgp, opts$gamma_n, seed = seed + 61, draw_outcome = FALSE)

  predictions <- setNames(vector("list", length(opts$learner_set)), opts$learner_set)
  for (learner in opts$learner_set) {
    fit <- list(fold_models = fitted$fold_models[[learner]])
    predictions[[learner]] <- list(
      tau_mat_test = predict_fold_matrix(fit, learner, test$W),
      tau_mat_gamma = predict_fold_matrix(fit, learner, gamma$W)
    )
  }

  cache <- list(
    signature = signature,
    task = task,
    seed = seed,
    train = list(
      W = dat$W,
      A = dat$A,
      Y = dat$Y,
      Y1 = dat$Y1,
      Y0 = dat$Y0,
      Q1 = dat$Q1,
      Q0 = dat$Q0,
      g = dat$g,
      tau = dat$tau,
      fold_id = fold_id,
      tau_oof = fitted$tau_oof,
      q1_oof = fitted$q1_oof,
      q0_oof = fitted$q0_oof,
      g_oof = fitted$g_oof
    ),
    test = list(
      W = test$W,
      A = test$A,
      Y = test$Y,
      Y1 = test$Y1,
      Y0 = test$Y0,
      Q1 = test$Q1,
      Q0 = test$Q0,
      g = test$g,
      tau = test$tau
    ),
    gamma = list(
      W = gamma$W,
      A = gamma$A,
      Y = gamma$Y,
      Y1 = gamma$Y1,
      Y0 = gamma$Y0,
      Q1 = gamma$Q1,
      Q0 = gamma$Q0,
      g = gamma$g,
      tau = gamma$tau
    ),
    predictions = predictions
  )
  atomic_save_rds(cache, path)
  rm(fitted, predictions, cache)
  gc()
  list(task = task_label(task), status = "trained", path = path)
}

calibrate_one_cached_task <- function(task, opts) {
  task <- as.list(task)
  dr_path <- dr_cache_path(task, opts)
  if (!valid_cache(dr_path, train_signature(task, opts))) {
    stop("Missing or incompatible DR cache: ", dr_path)
  }
  path <- calibration_cache_path(task, opts)
  signature <- calibration_signature(task, opts)
  if (!opts$force_calibration && valid_cache(path, signature)) {
    return(list(task = task_label(task), status = "cached", path = path))
  }

  message("[calibrate ", task_label(task), "]")
  dr <- readRDS(dr_path)
  metrics <- list()
  policy <- list()
  threshold_diagnostics <- list()
  flip_decomposition <- list()
  calibration_maps <- list()
  eval_predictions <- list()

  test <- list(Q0 = dr$test$Q0, Q1 = dr$test$Q1, tau = dr$test$tau)

  for (learner in opts$learner_set) {
    message("  ", learner, " / ", opts$method)
    tau_oof <- dr$train$tau_oof[, learner]
    tau_mat_test <- dr$predictions[[learner]]$tau_mat_test
    tau_mat_gamma <- dr$predictions[[learner]]$tau_mat_gamma
    raw_test <- lower_median(tau_mat_test)
    raw_gamma <- lower_median(tau_mat_gamma)

    calibrator <- fit_causal_iso_calibrator(
      tau = tau_oof,
      A = dr$train$A,
      Y = dr$train$Y,
      EY1 = dr$train$q1_oof,
      EY0 = dr$train$q0_oof,
      pA1 = dr$train$g_oof
    )
    calibrated_test <- predict_causal_iso(calibrator, tau_mat_test)
    calibrated_gamma <- predict_causal_iso(calibrator, tau_mat_gamma)

    raw_gamma_hat <- NULL
    calibrated_gamma_hat <- NULL
    if (!opts$skip_cal) {
      raw_gamma_model <- fit_cal_gamma_model(
        raw_gamma,
        dr$gamma$tau,
        seed = dr$seed + 71 + 10 * match(learner, opts$learner_set),
        options = opts
      )
      calibrated_gamma_model <- fit_cal_gamma_model(
        calibrated_gamma,
        dr$gamma$tau,
        seed = dr$seed + 81 + 10 * match(learner, opts$learner_set),
        options = opts
      )
      raw_gamma_hat <- predict(
        raw_gamma_model,
        newdata = data.frame(pred = raw_test),
        n.trees = raw_gamma_model$n.trees
      )
      calibrated_gamma_hat <- predict(
        calibrated_gamma_model,
        newdata = data.frame(pred = calibrated_test),
        n.trees = calibrated_gamma_model$n.trees
      )
    }

    id_columns <- list(
      sample_size = as.integer(task$sample_size),
      repeat_id = as.integer(task$repeat_id),
      learner = learner
    )
    add_ids <- function(dt) {
      dt[, `:=`(
        sample_size = id_columns$sample_size,
        repeat_id = id_columns$repeat_id,
        learner = id_columns$learner
      )]
      data.table::setcolorder(
        dt,
        c("sample_size", "repeat_id", "learner", setdiff(names(dt), c("sample_size", "repeat_id", "learner")))
      )
      dt
    }

    metrics[[learner]] <- add_ids(iso_metric_rows(
      method = opts$method,
      raw = raw_test,
      calibrated = calibrated_test,
      test = test,
      raw_gamma_hat = raw_gamma_hat,
      calibrated_gamma_hat = calibrated_gamma_hat
    ))
    policy[[learner]] <- add_ids(policy_rows(
      raw = raw_test,
      calibrated = calibrated_test,
      test = test,
      method = opts$method,
      threshold = opts$threshold
    )[, threshold := opts$threshold])
    threshold_diagnostics[[learner]] <- add_ids(threshold_diagnostic_rows(
      calibrator = calibrator,
      tau_oof = tau_oof,
      raw = raw_test,
      calibrated = calibrated_test,
      tau0 = test$tau,
      method = opts$method,
      threshold = opts$threshold
    ))
    flip_decomposition[[learner]] <- add_ids(flip_decomposition_rows(
      raw = raw_test,
      calibrated = calibrated_test,
      tau0 = test$tau,
      method = opts$method,
      threshold = opts$threshold
    ))
    calibration_maps[[learner]] <- add_ids(
      calibration_map_rows(calibrator, tau_oof)[, calibration_method := opts$method]
    )
    eval_predictions[[learner]] <- add_ids(make_eval_prediction_rows(
      raw = raw_test,
      calibrated = calibrated_test,
      tau0 = test$tau,
      method = opts$method,
      threshold = opts$threshold
    ))
  }

  cache <- list(
    signature = signature,
    task = task,
    metrics = data.table::rbindlist(metrics, use.names = TRUE),
    policy = data.table::rbindlist(policy, use.names = TRUE),
    threshold_diagnostics = data.table::rbindlist(threshold_diagnostics, use.names = TRUE),
    flip_decomposition = data.table::rbindlist(flip_decomposition, use.names = TRUE),
    calibration_maps = data.table::rbindlist(calibration_maps, use.names = TRUE),
    eval_predictions = data.table::rbindlist(eval_predictions, use.names = TRUE)
  )
  atomic_save_rds(cache, path)
  rm(dr, cache)
  gc()
  list(task = task_label(task), status = "calibrated", path = path)
}

worker_initialize <- function(project_root, opts, stage) {
  options(three_calibrators.project_root = project_root)
  source(file.path(project_root, "R", "load_original.R"))
  source(file.path(project_root, "R", "pipeline_common.R"))
  source(file.path(project_root, "R", "pipeline_stages.R"))
  if (stage == "train") {
    source(file.path(project_root, "R", "sl3_optimized_learners.R"))
  } else {
    source(file.path(project_root, "R", "iso_calibrator.R"))
    source(file.path(project_root, "R", "diagnostics_and_plots.R"))
  }
  load_original_reproduction()
  if (stage == "train") {
    activate_sl3_paper_engine(opts)
  }
  invisible(TRUE)
}

run_task_stage <- function(tasks, opts, stage) {
  worker_fun <- if (stage == "train") train_one_cached_task else calibrate_one_cached_task
  workers <- min(opts$mc_workers, nrow(tasks))
  if (workers <= 1L) {
    worker_initialize(opts$project_root, opts, stage)
    return(lapply(seq_len(nrow(tasks)), function(i) worker_fun(tasks[i, ], opts)))
  }

  cluster <- parallel::makeCluster(workers)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  parallel::clusterExport(cluster, c("opts", "stage"), envir = environment())
  parallel::clusterExport(cluster, "worker_initialize", envir = environment())
  parallel::clusterEvalQ(cluster, worker_initialize(opts$project_root, opts, stage))
  parallel::clusterExport(cluster, "worker_fun", envir = environment())
  parallel::parLapplyLB(cluster, seq_len(nrow(tasks)), function(i) worker_fun(tasks[i, ], opts))
}

pending_tasks <- function(opts, stage) {
  tasks <- task_table(opts)
  keep <- vapply(seq_len(nrow(tasks)), function(i) {
    task <- as.list(tasks[i, ])
    if (stage == "train") {
      opts$force_train || !valid_cache(dr_cache_path(task, opts), train_signature(task, opts))
    } else {
      opts$force_calibration || !valid_cache(
        calibration_cache_path(task, opts),
        calibration_signature(task, opts)
      )
    }
  }, logical(1))
  tasks[keep, , drop = FALSE]
}

run_training_stage <- function(opts) {
  tasks <- pending_tasks(opts, "train")
  if (nrow(tasks) == 0L) {
    message("Training stage: all DR caches are complete.")
    return(invisible(list()))
  }
  message("Training stage: ", nrow(tasks), " pending task(s), ", opts$mc_workers, " worker(s).")
  results <- run_task_stage(tasks, opts, "train")
  print(results)
  invisible(results)
}

consolidate_calibration_results <- function(opts) {
  tasks <- task_table(opts)
  caches <- lapply(seq_len(nrow(tasks)), function(i) {
    task <- as.list(tasks[i, ])
    path <- calibration_cache_path(task, opts)
    if (!valid_cache(path, calibration_signature(task, opts))) {
      stop("Calibration cache is missing or incompatible: ", path)
    }
    readRDS(path)
  })

  metrics_raw <- data.table::rbindlist(lapply(caches, `[[`, "metrics"), use.names = TRUE)
  policy_raw <- data.table::rbindlist(lapply(caches, `[[`, "policy"), use.names = TRUE)
  threshold_raw <- data.table::rbindlist(lapply(caches, `[[`, "threshold_diagnostics"), use.names = TRUE)
  flip_raw <- data.table::rbindlist(lapply(caches, `[[`, "flip_decomposition"), use.names = TRUE)
  maps_raw <- data.table::rbindlist(lapply(caches, `[[`, "calibration_maps"), use.names = TRUE)
  eval_raw <- data.table::rbindlist(lapply(caches, `[[`, "eval_predictions"), use.names = TRUE)

  metrics_summary <- summarize_metric_results(metrics_raw)
  policy_summary <- summarize_policy_results(policy_raw)
  threshold_summary <- summarize_threshold_diagnostics(threshold_raw)
  flip_summary <- summarize_flip_decomposition(flip_raw)
  raw_bin_fliprate <- make_raw_bin_fliprate(eval_raw, opts$raw_bin_count)

  results_dir <- run_paths(opts)$results
  tables <- list(
    metrics_raw = metrics_raw,
    metrics_summary = metrics_summary,
    policy_raw = policy_raw,
    policy_summary = policy_summary,
    threshold_diagnostics = threshold_raw,
    threshold_diagnostics_summary = threshold_summary,
    flip_decomposition = flip_raw,
    flip_decomposition_summary = flip_summary,
    calibration_maps = maps_raw,
    eval_predictions = eval_raw,
    raw_bin_fliprate = raw_bin_fliprate
  )
  for (name in names(tables)) {
    data.table::fwrite(tables[[name]], file.path(results_dir, paste0(name, ".csv")))
  }
  saveRDS(opts, file.path(results_dir, "run_config.rds"))
  save_full_analysis_data(opts, result_tables = tables)
  invisible(tables)
}

run_calibration_stage <- function(opts) {
  train_missing <- pending_tasks(opts, "train")
  if (nrow(train_missing) > 0L) {
    stop("Calibration requires complete DR caches. Missing: ", nrow(train_missing), " task(s).")
  }
  tasks <- pending_tasks(opts, "calibrate")
  if (nrow(tasks) > 0L) {
    message("Calibration stage: ", nrow(tasks), " pending task(s), ", opts$mc_workers, " worker(s).")
    print(run_task_stage(tasks, opts, "calibrate"))
  } else {
    message("Calibration stage: all causal-isotonic caches are complete.")
  }
  consolidate_calibration_results(opts)
}

run_plot_stage <- function(opts) {
  paths <- run_paths(opts)
  required <- c(
    "metrics_raw.csv",
    "policy_raw.csv",
    "threshold_diagnostics.csv",
    "eval_predictions.csv"
  )
  missing <- required[!file.exists(file.path(paths$results, required))]
  if (length(missing) > 0L) {
    stop("Plot stage requires calibration results. Missing: ", paste(missing, collapse = ", "))
  }

  metrics_raw <- data.table::fread(file.path(paths$results, "metrics_raw.csv"))
  policy_raw <- data.table::fread(file.path(paths$results, "policy_raw.csv"))
  threshold <- data.table::fread(file.path(paths$results, "threshold_diagnostics.csv"))
  eval_predictions <- data.table::fread(file.path(paths$results, "eval_predictions.csv"))

  plot_score_error_spearman_delta_v(
    metrics_raw,
    threshold,
    eval_predictions,
    file.path(paths$plots, "score_error_spearman_deltaV_four_panel.png")
  )

  plot_raw_score_quintile_boxplots(
    eval_predictions,
    file.path(paths$plots, "raw_score_quintile_boxplots_raw_iso_true_n5000.png"),
    preferred_n = 5000L
  )

  plot_threshold_treatment_two_panel(
    threshold,
    policy_raw,
    file.path(paths$plots, "threshold_and_treatment_rate_two_panel_publication.png")
  )

  plot_raw_score_flip_value_publication(
    eval_predictions,
    file.path(paths$plots, "raw_score_flipvalue_three_learners_n5000.png"),
    preferred_n = 5000L,
    summary_path = file.path(paths$plots, "flip_value_contribution_by_raw_score_bins_n5000.csv")
  )

  invisible(paths$plots)
}
