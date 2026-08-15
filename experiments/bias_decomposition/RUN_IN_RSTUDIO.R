project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(project_root, "R", "01_dgp.R"), encoding = "UTF-8")
source(file.path(project_root, "R", "02_calibration.R"), encoding = "UTF-8")
source(file.path(project_root, "R", "03_score_construction.R"), encoding = "UTF-8")
source(file.path(project_root, "R", "04_metrics.R"), encoding = "UTF-8")
source(file.path(project_root, "R", "05_run_experiments.R"), encoding = "UTF-8")
source(file.path(project_root, "R", "06_plots.R"), encoding = "UTF-8")

.set_formal_option <- function(opts, candidates, value, label) {
  matched <- intersect(candidates, names(opts))
  if (length(matched) == 0L) {
    stop(
      sprintf(
        "Cannot configure %s: none of [%s] is present in bias_decomposition_options().",
        label,
        paste(candidates, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  for (name in matched) {
    opts[[name]] <- value
  }
  opts
}

.configure_formal_run <- function(
    opts,
    run_id,
    repeats,
    n,
    workers) {
  opts <- .set_formal_option(opts, "run_id", as.character(run_id), "run ID")
  opts <- .set_formal_option(
    opts,
    c("reps", "repeats", "n_reps", "mc_reps", "R"),
    as.integer(repeats),
    "Monte Carlo repetitions"
  )
  opts <- .set_formal_option(
    opts,
    c("n", "sample_size", "evaluation_n"),
    as.integer(n),
    "sample size"
  )

  worker_fields <- intersect(c("workers", "mc_workers"), names(opts))
  for (name in worker_fields) {
    opts[[name]] <- as.integer(workers)
  }

  opts
}

run_formal_bias_decomposition_experiment <- function(
    project_root = getwd(),
    run_id = "bias_decomposition_n5000_reps300_final_v1",
    repeats = 300L,
    n = 5000L,
    workers = 3L) {
  opts <- bias_decomposition_options(project_root = project_root)
  opts <- .configure_formal_run(opts, run_id, repeats, n, workers)

  message("Starting the complete bias-decomposition experiment")
  message("  run_id: ", run_id)
  message("  sample size: ", n)
  message("  Monte Carlo repetitions: ", repeats)
  message("  requested workers: ", workers)
  message("  output directory: ", file.path(project_root, "runs", run_id))

  run_all_bias_decomposition_experiments(opts)
}

run_formal_sorting_additive_bias_module <- function(
    project_root = getwd(),
    run_id = "sorting_additive_bias_n5000_reps300_final_v1",
    repeats = 300L,
    n = 5000L,
    workers = 3L) {
  opts <- bias_decomposition_options(project_root = project_root)
  opts <- .configure_formal_run(opts, run_id, repeats, n, workers)
  run_sorting_additive_bias_module(opts)
}

show_formal_run_settings <- function(
    project_root = getwd(),
    run_id = "bias_decomposition_n5000_reps300_final_v1",
    repeats = 300L,
    n = 5000L,
    workers = 3L) {
  opts <- bias_decomposition_options(project_root = project_root)
  opts <- .configure_formal_run(opts, run_id, repeats, n, workers)
  print(opts)
  invisible(opts)
}

redraw_formal_plots <- function(
    project_root = getwd(),
    run_id = "bias_decomposition_n5000_reps300_final_v1",
    repeats = 300L,
    n = 5000L) {
  opts <- bias_decomposition_options(project_root = project_root)
  opts <- .configure_formal_run(opts, run_id, repeats, n, workers = 1L)
  paths <- ensure_bias_decomposition_paths(opts)
  opts$results_dir <- paths$results
  opts$plots_dir <- paths$plots
  make_bias_decomposition_plots(opts)
}

run_smoke_test <- function(
    project_root = getwd(),
    run_id = "bias_decomposition_smoke_test") {
  opts <- bias_decomposition_options(
    n = 200L,
    reps = 1L,
    run_id = run_id,
    project_root = project_root,
    sorting_sigmas = c(0, 0.05),
    additive_grid = c(-0.10, 0, 0.10),
    interaction_sigmas = c(0, 0.05),
    interaction_additive_grid = c(-0.10, 0.10),
    slope_grid = c(0.50, 1.00, 2.00)
  )
  run_all_bias_decomposition_experiments(opts)
}

message("Loaded bias decomposition mechanism experiment.")
message("Formal run: run_formal_bias_decomposition_experiment(project_root = getwd())")
message("Formal sorting x additive module only: run_formal_sorting_additive_bias_module(project_root = getwd())")
message("Plot only: redraw_formal_plots(project_root = getwd())")
message("Quick smoke test: run_smoke_test(project_root = getwd())")
