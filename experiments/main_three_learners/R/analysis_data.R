analysis_data_dir <- function(project_root, run_id) {
  file.path(project_root, "runs", run_id, "analysis_data")
}

analysis_archive_path <- function(project_root, run_id) {
  file.path(analysis_data_dir(project_root, run_id), "all_plot_data.rds")
}

load_analysis_data <- function(
  run_id = "three_learners_iso_reps300_reduced_nuisance_sl_v1",
  project_root = getOption("three_calibrators.project_root", normalizePath(".", winslash = "/"))
) {
  path <- analysis_archive_path(project_root, run_id)
  if (!file.exists(path)) {
    stop(
      "Reusable plot data was not found: ", path, "\n",
      "Run calibrate_from_cache() or plot_from_results() first."
    )
  }
  readRDS(path)
}

load_analysis_table <- function(
  table,
  run_id = "three_learners_iso_reps300_reduced_nuisance_sl_v1",
  project_root = getOption("three_calibrators.project_root", normalizePath(".", winslash = "/")),
  format = c("rds", "csv")
) {
  format <- match.arg(format)
  path <- file.path(analysis_data_dir(project_root, run_id), paste0(table, ".", format))
  if (!file.exists(path)) {
    stop("Analysis table was not found: ", path)
  }
  if (format == "csv") {
    return(data.table::fread(path))
  }
  readRDS(path)
}

list_analysis_data <- function(
  run_id = "three_learners_iso_reps300_reduced_nuisance_sl_v1",
  project_root = getOption("three_calibrators.project_root", normalizePath(".", winslash = "/"))
) {
  path <- file.path(analysis_data_dir(project_root, run_id), "manifest.csv")
  if (!file.exists(path)) {
    stop(
      "Analysis manifest was not found: ", path, "\n",
      "Run calibrate_from_cache() or plot_from_results() first."
    )
  }
  data.table::fread(path)
}

analysis_result_table_names <- function() {
  c(
    "metrics_raw",
    "metrics_summary",
    "policy_raw",
    "policy_summary",
    "threshold_diagnostics",
    "threshold_diagnostics_summary",
    "flip_decomposition",
    "flip_decomposition_summary",
    "calibration_maps",
    "eval_predictions",
    "raw_bin_fliprate"
  )
}

read_analysis_result_tables <- function(opts) {
  results_dir <- run_paths(opts)$results
  tables <- list()
  for (name in analysis_result_table_names()) {
    path <- file.path(results_dir, paste0(name, ".csv"))
    if (file.exists(path)) {
      tables[[name]] <- data.table::fread(path)
    }
  }
  tables
}

matrix_columns <- function(mat, prefix) {
  mat <- as.matrix(mat)
  out <- data.table::as.data.table(mat)
  data.table::setnames(out, sprintf("%s_%03d", prefix, seq_len(ncol(mat))))
  out
}

calibrated_fold_matrix <- function(calibrator, mat) {
  mat <- as.matrix(mat)
  out <- apply(mat, 2L, function(x) as.numeric(calibrator$calibration_function(x)))
  if (is.null(dim(out))) {
    out <- matrix(out, ncol = 1L)
  }
  dimnames(out) <- dimnames(mat)
  out
}

task_seed_value <- function(task, dr = NULL) {
  if (!is.null(dr) && !is.null(dr$seed)) {
    return(as.integer(dr$seed))
  }
  as.integer(2026 + 100000 * as.integer(task$repeat_id) + as.integer(task$sample_size))
}

task_fold_membership <- function(task, opts, seed) {
  n <- as.integer(task$sample_size)
  folds <- make_folds(n, opts$folds, seed = seed + 11L)
  fold_id <- integer(n)
  for (i in seq_along(folds)) {
    fold_id[folds[[i]]] <- i
  }
  fold_id
}

merge_generated_sample <- function(stored, generated) {
  if (is.null(stored)) {
    return(generated)
  }
  for (name in intersect(names(generated), names(stored))) {
    if (!is.null(stored[[name]])) {
      generated[[name]] <- stored[[name]]
    }
  }
  generated
}

restore_task_samples <- function(task, opts, dr) {
  seed <- task_seed_value(task, dr)
  dgp <- opts_dgp(opts)
  generated <- list(
    train = generate_dgp_sample(dgp, as.integer(task$sample_size), seed = seed),
    test = generate_dgp_sample(dgp, opts$test_n, seed = seed + 37L, draw_outcome = FALSE),
    gamma = generate_dgp_sample(dgp, opts$gamma_n, seed = seed + 61L, draw_outcome = FALSE)
  )
  list(
    train = merge_generated_sample(dr$train, generated$train),
    test = merge_generated_sample(dr$test, generated$test),
    gamma = merge_generated_sample(dr$gamma, generated$gamma)
  )
}

sample_table <- function(sample, id_col) {
  w <- data.table::as.data.table(sample$W)
  out <- data.table::data.table(row_id = seq_len(nrow(w)))
  data.table::setnames(out, "row_id", id_col)
  out <- cbind(out, w)

  fields <- c("A", "Y", "Y1", "Y0", "Q0", "Q1", "g", "tau")
  for (field in fields) {
    if (!is.null(sample[[field]])) {
      column <- if (field == "tau") "tau0" else field
      out[, (column) := as.numeric(sample[[field]])]
    }
  }
  out
}

calibration_pseudo_outcome <- function(A, Y, EY1, EY0, pA1) {
  EY <- ifelse(A == 1, EY1, EY0)
  pA <- ifelse(A == 1, pA1, 1 - pA1)
  EY1 - EY0 + (2 * A - 1) / pA * (Y - EY)
}

add_task_columns <- function(dt, task, learner, method) {
  dt[, `:=`(
    sample_size = as.integer(task$sample_size),
    repeat_id = as.integer(task$repeat_id),
    learner = learner,
    calibration_method = method
  )]
  data.table::setcolorder(
    dt,
    c(
      "sample_size",
      "repeat_id",
      "learner",
      "calibration_method",
      setdiff(names(dt), c("sample_size", "repeat_id", "learner", "calibration_method"))
    )
  )
  dt
}

make_train_analysis_rows <- function(task, opts, dr, samples, fold_id, learner, calibrator) {
  tau_oof <- as.numeric(dr$train$tau_oof[, learner])
  tau_iso_oof <- as.numeric(calibrator$calibration_function(tau_oof))
  pseudo <- calibration_pseudo_outcome(
    A = dr$train$A,
    Y = dr$train$Y,
    EY1 = dr$train$q1_oof,
    EY0 = dr$train$q0_oof,
    pA1 = dr$train$g_oof
  )

  out <- sample_table(samples$train, "train_id")
  out[, `:=`(
    fold_id = as.integer(fold_id),
    q1_oof = as.numeric(dr$train$q1_oof),
    q0_oof = as.numeric(dr$train$q0_oof),
    g_oof = as.numeric(dr$train$g_oof),
    tau_raw_oof = tau_oof,
    tau_iso_oof = tau_iso_oof,
    calibration_pseudo_outcome = as.numeric(pseudo),
    tau_raw_oof_error = tau_oof - tau0,
    tau_iso_oof_error = tau_iso_oof - tau0
  )]
  add_task_columns(out, task, learner, opts$method)
}

make_prediction_analysis_rows <- function(
  task,
  opts,
  sample,
  sample_role,
  id_col,
  learner,
  calibrator,
  raw_mat
) {
  raw_mat <- as.matrix(raw_mat)
  iso_mat <- calibrated_fold_matrix(calibrator, raw_mat)
  tau_raw <- lower_median(raw_mat)
  tau_iso <- lower_median(iso_mat)

  out <- sample_table(sample, id_col)
  out[, `:=`(
    sample_role = sample_role,
    tau_raw = as.numeric(tau_raw),
    tau_iso = as.numeric(tau_iso),
    tau_raw_error = as.numeric(tau_raw - tau0),
    tau_iso_error = as.numeric(tau_iso - tau0),
    d_raw = as.integer(tau_raw > opts$threshold),
    d_iso = as.integer(tau_iso > opts$threshold),
    d_oracle = as.integer(tau0 > opts$threshold)
  )]
  if ("Q0" %in% names(out)) {
    out[, `:=`(
      policy_value_raw = Q0 + d_raw * tau0,
      policy_value_iso = Q0 + d_iso * tau0,
      policy_value_oracle = Q0 + d_oracle * tau0
    )]
  }

  out <- cbind(
    out,
    matrix_columns(raw_mat, "tau_raw_fold"),
    matrix_columns(iso_mat, "tau_iso_fold")
  )
  add_task_columns(out, task, learner, opts$method)
}

make_task_analysis_tables <- function(task, opts) {
  task <- as.list(task)
  dr_path <- dr_cache_path(task, opts)
  if (!valid_cache(dr_path, train_signature(task, opts))) {
    stop("Missing or incompatible DR cache for analysis export: ", dr_path)
  }
  dr <- readRDS(dr_path)
  seed <- task_seed_value(task, dr)
  samples <- restore_task_samples(task, opts, dr)
  fold_id <- dr$train$fold_id
  if (is.null(fold_id)) {
    fold_id <- task_fold_membership(task, opts, seed)
  }

  train_rows <- list()
  test_rows <- list()
  gamma_rows <- list()
  calibration_objects <- list()

  for (learner in opts$learner_set) {
    calibrator <- fit_causal_iso_calibrator(
      tau = dr$train$tau_oof[, learner],
      A = dr$train$A,
      Y = dr$train$Y,
      EY1 = dr$train$q1_oof,
      EY0 = dr$train$q0_oof,
      pA1 = dr$train$g_oof
    )

    key <- paste(task_label(task), gsub("[^A-Za-z0-9]+", "", learner), sep = "__")
    calibration_objects[[key]] <- list(
      sample_size = as.integer(task$sample_size),
      repeat_id = as.integer(task$repeat_id),
      learner = learner,
      calibration_method = opts$method,
      calibrator = calibrator
    )

    train_rows[[learner]] <- make_train_analysis_rows(
      task = task,
      opts = opts,
      dr = dr,
      samples = samples,
      fold_id = fold_id,
      learner = learner,
      calibrator = calibrator
    )
    test_rows[[learner]] <- make_prediction_analysis_rows(
      task = task,
      opts = opts,
      sample = samples$test,
      sample_role = "test",
      id_col = "eval_id",
      learner = learner,
      calibrator = calibrator,
      raw_mat = dr$predictions[[learner]]$tau_mat_test
    )
    gamma_rows[[learner]] <- make_prediction_analysis_rows(
      task = task,
      opts = opts,
      sample = samples$gamma,
      sample_role = "gamma",
      id_col = "gamma_id",
      learner = learner,
      calibrator = calibrator,
      raw_mat = dr$predictions[[learner]]$tau_mat_gamma
    )
  }

  list(
    train_oof_predictions = data.table::rbindlist(train_rows, use.names = TRUE),
    test_predictions = data.table::rbindlist(test_rows, use.names = TRUE),
    gamma_predictions = data.table::rbindlist(gamma_rows, use.names = TRUE),
    calibration_objects = calibration_objects
  )
}

build_analysis_tables <- function(opts) {
  tasks <- task_table(opts)
  pieces <- lapply(seq_len(nrow(tasks)), function(i) {
    make_task_analysis_tables(tasks[i, ], opts)
  })

  list(
    tables = list(
      train_oof_predictions = data.table::rbindlist(
        lapply(pieces, `[[`, "train_oof_predictions"),
        use.names = TRUE
      ),
      test_predictions = data.table::rbindlist(
        lapply(pieces, `[[`, "test_predictions"),
        use.names = TRUE
      ),
      gamma_predictions = data.table::rbindlist(
        lapply(pieces, `[[`, "gamma_predictions"),
        use.names = TRUE
      )
    ),
    calibration_objects = do.call(c, lapply(pieces, `[[`, "calibration_objects"))
  )
}

analysis_table_descriptions <- function() {
  c(
    train_oof_predictions = "Training observations with W, true DGP quantities, OOF nuisance estimates, raw OOF CATE, and isotonic OOF CATE.",
    test_predictions = "Test observations with W, true DGP quantities, raw/isotonic CATE, decisions, policy values, and fold-level predictions.",
    gamma_predictions = "Independent gamma observations with W, true DGP quantities, raw/isotonic CATE, decisions, policy values, and fold-level predictions.",
    metrics_raw = "Per-repeat MSE/CAL rows.",
    metrics_summary = "Monte Carlo summaries of MSE/CAL.",
    policy_raw = "Per-repeat policy value rows.",
    policy_summary = "Monte Carlo summaries of policy value rows.",
    threshold_diagnostics = "Per-repeat effective-threshold and decision-flip diagnostics.",
    threshold_diagnostics_summary = "Monte Carlo summaries of threshold diagnostics.",
    flip_decomposition = "Per-repeat decision-flip value decomposition.",
    flip_decomposition_summary = "Monte Carlo summaries of decision-flip value decomposition.",
    calibration_maps = "Raw score to isotonic score map points.",
    eval_predictions = "Compact test-level prediction table used by the original plot functions.",
    raw_bin_fliprate = "Raw-score bin flip-rate table used by the original plot functions."
  )
}

write_analysis_manifest <- function(tables, out_dir) {
  descriptions <- analysis_table_descriptions()
  manifest <- data.table::rbindlist(lapply(names(tables), function(name) {
    description <- unname(descriptions[name])
    if (is.na(description)) {
      description <- ""
    }
    data.table::data.table(
      object = name,
      rows = nrow(tables[[name]]),
      columns = ncol(tables[[name]]),
      csv_file = paste0(name, ".csv"),
      rds_file = paste0(name, ".rds"),
      description = description
    )
  }), use.names = TRUE)

  manifest <- data.table::rbindlist(list(
    manifest,
    data.table::data.table(
      object = "all_plot_data",
      rows = NA_integer_,
      columns = NA_integer_,
      csv_file = NA_character_,
      rds_file = "all_plot_data.rds",
      description = "One RDS list containing run_config plus every table in this manifest."
    ),
    data.table::data.table(
      object = "calibration_objects",
      rows = NA_integer_,
      columns = NA_integer_,
      csv_file = NA_character_,
      rds_file = "calibration_objects.rds",
      description = "RDS list of fitted causal isotonic calibrator objects keyed by task and learner."
    )
  ), use.names = TRUE)

  data.table::fwrite(manifest, file.path(out_dir, "manifest.csv"))
  invisible(manifest)
}

write_analysis_readme <- function(run_id, out_dir) {
  lines <- c(
    "Reusable plotting data for this run.",
    "",
    "Load everything in R:",
    "source('R/analysis_data.R')",
    sprintf("dat <- load_analysis_data('%s')", run_id),
    "",
    "Inspect available tables:",
    sprintf("list_analysis_data('%s')", run_id),
    "",
    "Load one table:",
    sprintf("test_dt <- load_analysis_table('test_predictions', '%s')", run_id)
  )
  writeLines(lines, file.path(out_dir, "README.txt"), useBytes = TRUE)
}

save_full_analysis_data <- function(opts, result_tables = NULL) {
  paths <- ensure_run_paths(opts)
  out_dir <- paths$analysis_data
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  message("Saving reusable plot data to: ", out_dir)
  detailed <- build_analysis_tables(opts)
  if (is.null(result_tables)) {
    result_tables <- read_analysis_result_tables(opts)
  }
  tables <- c(detailed$tables, result_tables)

  for (name in names(tables)) {
    data.table::fwrite(tables[[name]], file.path(out_dir, paste0(name, ".csv")))
    saveRDS(tables[[name]], file.path(out_dir, paste0(name, ".rds")), compress = FALSE)
  }

  saveRDS(
    c(list(run_config = opts), tables),
    file.path(out_dir, "all_plot_data.rds"),
    compress = FALSE
  )
  saveRDS(
    detailed$calibration_objects,
    file.path(out_dir, "calibration_objects.rds"),
    compress = FALSE
  )
  write_analysis_manifest(tables, out_dir)
  write_analysis_readme(opts$run_id, out_dir)
  invisible(file.path(out_dir, "all_plot_data.rds"))
}

ensure_full_analysis_data <- function(opts) {
  path <- file.path(run_paths(opts)$analysis_data, "all_plot_data.rds")
  if (!file.exists(path) || isTRUE(opts$force_plot)) {
    return(save_full_analysis_data(opts))
  }
  invisible(path)
}
