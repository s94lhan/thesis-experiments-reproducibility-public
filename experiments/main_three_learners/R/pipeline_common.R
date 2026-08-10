iso_defaults <- function(project_root) {
  list(
    stage = "all",
    dgp = "scenario1",
    run_id = "three_learners_iso_reps300_reduced_nuisance_sl_v1",
    sample_sizes = c(1000, 2000, 5000),
    repeats = 300L,
    learners = "GLMnet,GBRT_8,RF",
    methods = "Causal_Isotonic",
    folds = 10L,
    test_n = 3000L,
    gamma_n = 5000L,
    sl3_gbm_trees = 2000L,
    sl3_gbm_shrinkage = 0.005,
    sl3_rf_trees = 300L,
    nuisance_library_id = "reduced_nuisance_sl_v1_q_glmnet_gbrt8_rf__g_glm_glmnet_gbrt2",
    threshold = 0,
    cal_mode = "trend",
    cal_gbm_trees = 200L,
    cal_gbm_depth = 3L,
    skip_cal = FALSE,
    plot_threshold_band = 0.05,
    raw_bin_count = 30L,
    mc_workers = 3L,
    force_train = FALSE,
    force_calibration = FALSE,
    force_plot = FALSE,
    project_root = project_root
  )
}

parse_iso_args <- function(args, project_root) {
  opts <- iso_defaults(project_root)
  value_keys <- c(
    "--stage",
    "--dgp",
    "--run-id",
    "--sample-sizes",
    "--repeats",
    "--learners",
    "--methods",
    "--folds",
    "--test-n",
    "--gamma-n",
    "--sl3-gbm-trees",
    "--sl3-gbm-shrinkage",
    "--sl3-rf-trees",
    "--threshold",
    "--cal-mode",
    "--cal-gbm-trees",
    "--cal-gbm-depth",
    "--plot-threshold-band",
    "--raw-bin-count",
    "--mc-workers"
  )

  i <- 1L
  while (i <= length(args)) {
    key <- args[[i]]
    if (key %in% c("--force-train", "--force-calibration", "--force-plot", "--skip-cal")) {
      option_name <- gsub("-", "_", sub("^--", "", key))
      opts[[option_name]] <- TRUE
      i <- i + 1L
      next
    }
    if (!key %in% value_keys || i == length(args)) {
      stop("Unknown or incomplete argument: ", key)
    }
    value <- args[[i + 1L]]
    if (key == "--stage") opts$stage <- tolower(value)
    if (key == "--dgp") opts$dgp <- tolower(value)
    if (key == "--run-id") opts$run_id <- value
    if (key == "--sample-sizes") opts$sample_sizes <- parse_numeric_csv(value, integer = TRUE)
    if (key == "--repeats") opts$repeats <- as.integer(value)
    if (key == "--learners") opts$learners <- value
    if (key == "--methods") opts$methods <- value
    if (key == "--folds") opts$folds <- as.integer(value)
    if (key == "--test-n") opts$test_n <- as.integer(value)
    if (key == "--gamma-n") opts$gamma_n <- as.integer(value)
    if (key == "--sl3-gbm-trees") opts$sl3_gbm_trees <- as.integer(value)
    if (key == "--sl3-gbm-shrinkage") opts$sl3_gbm_shrinkage <- as.numeric(value)
    if (key == "--sl3-rf-trees") opts$sl3_rf_trees <- as.integer(value)
    if (key == "--threshold") opts$threshold <- as.numeric(value)
    if (key == "--cal-mode") opts$cal_mode <- tolower(value)
    if (key == "--cal-gbm-trees") opts$cal_gbm_trees <- as.integer(value)
    if (key == "--cal-gbm-depth") opts$cal_gbm_depth <- as.integer(value)
    if (key == "--plot-threshold-band") opts$plot_threshold_band <- as.numeric(value)
    if (key == "--raw-bin-count") opts$raw_bin_count <- as.integer(value)
    if (key == "--mc-workers") opts$mc_workers <- as.integer(value)
    i <- i + 2L
  }

  if (!opts$stage %in% c("all", "train", "calibrate", "plot")) {
    stop("Stage must be one of: all, train, calibrate, plot.")
  }
  if (!opts$dgp %in% c("scenario1")) {
    stop("DGP must be scenario1.")
  }
  if (!opts$cal_mode %in% c("paper", "trend")) {
    stop("CAL mode must be one of: paper, trend.")
  }
  if (any(!is.finite(opts$sample_sizes)) || any(opts$sample_sizes < 2L)) {
    stop("All sample sizes must be integers of at least 2.")
  }
  if (!is.finite(opts$repeats) || opts$repeats < 1L) {
    stop("Repeats must be at least 1.")
  }
  if (!is.finite(opts$folds) || opts$folds < 2L) {
    stop("Cross-fitting folds must be at least 2.")
  }
  if (opts$folds > min(opts$sample_sizes)) {
    stop("Cross-fitting folds cannot exceed the smallest sample size.")
  }
  opts$cal_gbm_trees <- max(1L, opts$cal_gbm_trees)
  opts$cal_gbm_depth <- max(1L, opts$cal_gbm_depth)
  opts$mc_workers <- max(1L, opts$mc_workers)
  opts$raw_bin_count <- max(5L, opts$raw_bin_count)
  opts
}

parse_numeric_csv <- function(value, integer = FALSE) {
  output <- as.numeric(strsplit(value, ",", fixed = TRUE)[[1]])
  if (integer) {
    output <- as.integer(output)
  }
  output
}

normalize_iso_learners <- function(learner_arg) {
  allowed <- c("GLMnet", "GBRT 8", "RF")
  if (identical(tolower(trimws(learner_arg)), "all")) {
    return(allowed)
  }
  learner_set <- trimws(strsplit(learner_arg, ",", fixed = TRUE)[[1]])
  learner_set <- gsub("_", " ", learner_set, fixed = TRUE)
  bad <- setdiff(learner_set, allowed)
  if (length(bad) > 0) {
    stop("This project only allows GLMnet, GBRT 8, and RF. Unknown learners: ", paste(bad, collapse = ", "))
  }
  unique(learner_set)
}

normalize_iso_method <- function(method_arg) {
  normalized <- tolower(gsub("[ _-]", "", trimws(method_arg)))
  if (!normalized %in% c("causalisotonic", "iso", "isotonic")) {
    stop("This project only allows Causal Isotonic calibration.")
  }
  "Causal Isotonic"
}

opts_dgp <- function(opts) {
  dgp <- opts$dgp
  if (is.null(dgp) || !nzchar(dgp)) {
    dgp <- "scenario1"
  }
  tolower(dgp)
}

dgp_signature_label <- function(opts) {
  dgp <- opts_dgp(opts)
  if (dgp == "scenario1") {
    return("scenario1_plus_1.5_intercept")
  }
  stop("Unknown DGP: ", dgp)
}

dgp_outcome_type <- function(opts) {
  if (opts_dgp(opts) != "scenario1") {
    stop("Unknown DGP: ", opts_dgp(opts))
  }
  "binomial"
}

generate_dgp_sample <- function(dgp, n, seed, draw_outcome = TRUE) {
  dgp <- tolower(dgp)
  if (dgp == "scenario1") {
    return(scenario1(n, seed = seed, draw_outcome = draw_outcome))
  }
  stop("Unknown DGP: ", dgp)
}

learner_file_label <- function(learner) {
  gsub(" ", "", learner, fixed = TRUE)
}

run_paths <- function(opts) {
  run_root <- file.path(opts$project_root, "runs", opts$run_id)
  list(
    root = run_root,
    dr = file.path(run_root, "cache", "dr"),
    calibration = file.path(run_root, "cache", "calibration"),
    results = file.path(run_root, "results"),
    analysis_data = file.path(run_root, "analysis_data"),
    plots = file.path(run_root, "plots"),
    shared = file.path(opts$project_root, "cache", "shared")
  )
}

ensure_run_paths <- function(opts) {
  paths <- run_paths(opts)
  for (path in paths) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  paths
}

task_table <- function(opts) {
  expand.grid(
    sample_size = opts$sample_sizes,
    repeat_id = seq_len(opts$repeats),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
}

task_label <- function(task) {
  sprintf("n%s_r%03d", task$sample_size, task$repeat_id)
}

dr_cache_path <- function(task, opts) {
  file.path(run_paths(opts)$dr, paste0("dr_", task_label(task), ".rds"))
}

calibration_cache_path <- function(task, opts) {
  file.path(run_paths(opts)$calibration, paste0("calibration_", task_label(task), ".rds"))
}

train_signature <- function(task, opts) {
  list(
    version = "three_learner_iso_dr_cache_v2_reduced_nuisance_sl",
    dgp = dgp_signature_label(opts),
    sample_size = as.integer(task$sample_size),
    repeat_id = as.integer(task$repeat_id),
    learners = opts$learner_set,
    folds = opts$folds,
    test_n = opts$test_n,
    gamma_n = opts$gamma_n,
    gbm_trees = opts$sl3_gbm_trees,
    gbm_shrinkage = opts$sl3_gbm_shrinkage,
    rf_trees = opts$sl3_rf_trees,
    nuisance_library_id = opts$nuisance_library_id
  )
}

calibration_signature <- function(task, opts) {
  list(
    version = "three_learner_iso_calibration_cache_v1",
    train = train_signature(task, opts),
    method = opts$method,
    threshold = opts$threshold,
    cal_mode = opts$cal_mode,
    cal_gbm_trees = opts$cal_gbm_trees,
    cal_gbm_depth = opts$cal_gbm_depth,
    skip_cal = opts$skip_cal,
    raw_bin_count = opts$raw_bin_count
  )
}

valid_cache <- function(path, signature) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  cached <- tryCatch(readRDS(path), error = function(e) NULL)
  !is.null(cached) && identical(cached$signature, signature)
}

atomic_save_rds <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  saveRDS(object, temp, compress = FALSE)
  if (file.exists(path)) {
    unlink(path)
  }
  if (!file.rename(temp, path)) {
    stop("Could not move cache file into place: ", path)
  }
  invisible(path)
}
