project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "scripts", "run_pipeline.R"))) {
  stop("Open ThreeLearnersIsoCrossCalibration.Rproj before sourcing RUN_IN_RSTUDIO.R.")
}
rscript <- file.path(R.home("bin"), "Rscript.exe")
pipeline_script <- file.path(project_root, "scripts", "run_pipeline.R")

experiment_args <- c(
  "--dgp", "scenario1",
  "--run-id", "three_learners_iso_reps300_reduced_nuisance_sl_v1",
  "--sample-sizes", "1000,2000,5000",
  "--repeats", "300",
  "--learners", "GLMnet,GBRT_8,RF",
  "--methods", "Causal_Isotonic",
  "--folds", "10",
  "--test-n", "3000",
  "--gamma-n", "5000",
  "--sl3-gbm-trees", "2000",
  "--sl3-gbm-shrinkage", "0.005",
  "--sl3-rf-trees", "300",
  "--threshold", "0",
  "--cal-mode", "trend",
  "--cal-gbm-trees", "200",
  "--cal-gbm-depth", "3",
  "--plot-threshold-band", "0.05",
  "--raw-bin-count", "30",
  "--mc-workers", "3"
)

run_stage <- function(stage, force = FALSE, skip_cal = FALSE, args_override = experiment_args) {
  force_arg <- switch(
    stage,
    train = "--force-train",
    calibrate = "--force-calibration",
    plot = "--force-plot",
    NULL
  )
  args <- c(shQuote(pipeline_script), "--stage", stage, args_override)
  if (force && !is.null(force_arg)) {
    args <- c(args, force_arg)
  }
  if (skip_cal) {
    args <- c(args, "--skip-cal")
  }
  message("Starting stage: ", stage)
  status <- system2(rscript, args = args, wait = TRUE)
  if (!identical(status, 0L)) {
    stop("Stage failed with exit code ", status, ".")
  }
  invisible(status)
}

train_learners <- function(force = FALSE) {
  run_stage("train", force = force)
}

calibrate_from_cache <- function(force = FALSE, skip_cal = FALSE) {
  run_stage("calibrate", force = force, skip_cal = skip_cal)
}

plot_from_results <- function(force = FALSE, skip_cal = FALSE) {
  run_stage("plot", force = force, skip_cal = skip_cal)
}

run_full_experiment <- function(skip_cal = FALSE) {
  train_learners()
  calibrate_from_cache(skip_cal = skip_cal)
  plot_from_results(skip_cal = skip_cal)
  invisible(show_run_status())
}

show_run_status_for <- function(run_id, repeats) {
  run_root <- file.path(project_root, "runs", run_id)
  expected <- 3L * as.integer(repeats)
  dr_count <- length(list.files(file.path(run_root, "cache", "dr"), pattern = "[.]rds$"))
  calibration_count <- length(list.files(file.path(run_root, "cache", "calibration"), pattern = "[.]rds$"))
  analysis_dir <- file.path(run_root, "analysis_data")
  analysis_files <- if (dir.exists(analysis_dir)) {
    length(list.files(analysis_dir, pattern = "[.](csv|rds|txt)$"))
  } else {
    0L
  }
  cat(
    "DR caches:", dr_count, "/", expected, "\n",
    "Iso calibration caches:", calibration_count, "/", expected, "\n",
    "Results:", file.path(run_root, "results"), "\n",
    "Reusable plot data:", analysis_dir, "(", analysis_files, "files)\n",
    "Plots:", file.path(run_root, "plots"), "\n"
  )
  invisible(list(dr = dr_count, calibration = calibration_count, expected = expected))
}

show_run_status <- function() {
  show_run_status_for("three_learners_iso_reps300_reduced_nuisance_sl_v1", 300L)
}

check_formal_settings <- function() {
  value_after <- function(flag) {
    position <- match(flag, experiment_args)
    if (is.na(position) || position == length(experiment_args)) {
      stop("Missing formal argument: ", flag)
    }
    experiment_args[[position + 1L]]
  }

  expected <- c(
    "--run-id" = "three_learners_iso_reps300_reduced_nuisance_sl_v1",
    "--sample-sizes" = "1000,2000,5000",
    "--repeats" = "300",
    "--learners" = "GLMnet,GBRT_8,RF",
    "--methods" = "Causal_Isotonic",
    "--folds" = "10",
    "--test-n" = "3000",
    "--gamma-n" = "5000",
    "--sl3-gbm-trees" = "2000",
    "--sl3-gbm-shrinkage" = "0.005",
    "--sl3-rf-trees" = "300",
    "--threshold" = "0",
    "--cal-mode" = "trend",
    "--cal-gbm-trees" = "200",
    "--cal-gbm-depth" = "3"
  )
  actual <- vapply(names(expected), value_after, character(1))
  stopifnot(identical(unname(actual), unname(expected)))
  message("Formal main-experiment settings match the thesis release configuration.")
  invisible(actual)
}

load_saved_plot_table <- function(table, run_id = "three_learners_iso_reps300_reduced_nuisance_sl_v1", format = "rds") {
  source(file.path(project_root, "R", "analysis_data.R"))
  load_analysis_table(table = table, run_id = run_id, project_root = project_root, format = format)
}

cat(
  "Ready.\n",
  "1. train_learners()\n",
  "2. calibrate_from_cache()\n",
  "3. plot_from_results()\n",
  "4. show_run_status()\n",
  "5. check_formal_settings()\n",
  "Or run_full_experiment() to execute the three stages in order.\n"
)
