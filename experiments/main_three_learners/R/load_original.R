resolve_paper_project <- function() {
  project_root <- getOption("three_calibrators.project_root", normalizePath(".", winslash = "/"))
  bundled_copy <- file.path(project_root, "vendor", "original_figure1")
  if (file.exists(file.path(bundled_copy, "reproduce_figure1_full.R"))) {
    return(bundled_copy)
  }

  stop("Could not find vendor/original_figure1/reproduce_figure1_full.R in this project.")
}

load_original_reproduction <- function(project_dir = resolve_paper_project()) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(project_dir)

  lines <- readLines("reproduce_figure1_full.R", warn = FALSE)
  stop_line <- grep("^scale_dat <-", lines)[1]
  if (is.na(stop_line)) {
    stop("Could not find the main-run boundary in reproduce_figure1_full.R")
  }

  eval(parse(text = paste(lines[seq_len(stop_line - 1)], collapse = "\n")), envir = .GlobalEnv)
}
