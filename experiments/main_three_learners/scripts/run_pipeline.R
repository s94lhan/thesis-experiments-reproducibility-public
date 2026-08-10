get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/"))
  }
  normalizePath("scripts/run_pipeline.R", winslash = "/", mustWork = FALSE)
}

project_root <- normalizePath(file.path(dirname(get_script_path()), ".."), winslash = "/")
options(three_calibrators.project_root = project_root)
source(file.path(project_root, "R", "load_original.R"))
source(file.path(project_root, "R", "pipeline_common.R"))
source(file.path(project_root, "R", "pipeline_stages.R"))
source(file.path(project_root, "R", "iso_calibrator.R"))
source(file.path(project_root, "R", "diagnostics_and_plots.R"))
source(file.path(project_root, "R", "analysis_data.R"))

main <- function() {
  opts <- parse_iso_args(commandArgs(trailingOnly = TRUE), project_root)
  ensure_run_paths(opts)
  load_original_reproduction()
  opts$learner_set <- normalize_iso_learners(opts$learners)
  opts$method <- normalize_iso_method(opts$methods)

  message("Three-learner Iso project: ", project_root)
  message("Run ID: ", opts$run_id)
  message("DGP: ", opts_dgp(opts))
  message("Stage: ", opts$stage)
  message("Sample sizes: ", paste(opts$sample_sizes, collapse = ", "))
  message("Repeats: ", opts$repeats)
  message("Learners: ", paste(opts$learner_set, collapse = ", "))
  message("Nuisance SL library: ", opts$nuisance_library_id)
  message("Calibration method: ", opts$method)
  message("Monte Carlo workers: ", opts$mc_workers)
  message(
    "GBM: n.trees=", opts$sl3_gbm_trees,
    ", shrinkage=", opts$sl3_gbm_shrinkage,
    "; RF trees=", opts$sl3_rf_trees
  )
  message(
    "CAL: ",
    if (opts$skip_cal) {
      "skipped"
    } else if (opts$cal_mode == "trend") {
      paste0("trend (fixed GBM depth=", opts$cal_gbm_depth, ", trees=", opts$cal_gbm_trees, ")")
    } else {
      "paper (5-fold depth CV)"
    }
  )

  if (opts$stage %in% c("all", "train")) {
    run_training_stage(opts)
  }
  if (opts$stage %in% c("all", "calibrate")) {
    run_calibration_stage(opts)
  }
  if (opts$stage %in% c("all", "plot")) {
    run_plot_stage(opts)
  }
  message("Done.")
}

main()
