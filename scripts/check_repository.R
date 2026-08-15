get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this check with Rscript scripts/check_repository.R")
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), winslash = "/")
}

repo_root <- normalizePath(file.path(dirname(get_script_path()), ".."), winslash = "/")
main_root <- file.path(repo_root, "experiments", "main_three_learners")
bias_root <- file.path(repo_root, "experiments", "bias_decomposition")

required <- c(
  file.path(repo_root, "README.md"),
  file.path(repo_root, ".gitignore"),
  file.path(repo_root, "THIRD_PARTY_NOTICE.md"),
  file.path(main_root, "ThreeLearnersIsoCrossCalibration.Rproj"),
  file.path(main_root, "RUN_IN_RSTUDIO.R"),
  file.path(main_root, "renv.lock"),
  file.path(bias_root, "BiasDecompositionExperiment.Rproj"),
  file.path(bias_root, "RUN_IN_RSTUDIO.R"),
  file.path(bias_root, "renv.lock")
)
missing <- required[!file.exists(required)]
if (length(missing) > 0L) {
  stop("Missing required repository files:\n", paste(missing, collapse = "\n"))
}

r_files <- list.files(
  file.path(repo_root, "experiments"),
  pattern = "[.]R$",
  recursive = TRUE,
  full.names = TRUE
)
r_file_paths <- gsub("\\\\", "/", r_files)
r_files <- r_files[!grepl("/(renv/library|runs|cache)/", r_file_paths)]
comment_hits <- character()
for (file in c(r_files, get_script_path())) {
  parsed <- parse(file = file, keep.source = TRUE)
  parse_data <- getParseData(parsed, includeText = TRUE)
  if (any(parse_data$token == "COMMENT")) {
    comment_hits <- c(comment_hits, file)
  }
}
if (length(comment_hits) > 0L) {
  stop("Code comments remain in:\n", paste(comment_hits, collapse = "\n"))
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)

setwd(main_root)
source("RUN_IN_RSTUDIO.R", encoding = "UTF-8")
check_formal_settings()

bias_env <- new.env(parent = globalenv())
sys.source(
  file.path(bias_root, "R", "05_run_experiments.R"),
  envir = bias_env,
  keep.source = FALSE
)
bias_opts <- bias_env$bias_decomposition_options()
stopifnot(
  identical(as.integer(bias_opts$reps), 300L),
  identical(as.integer(bias_opts$n), 5000L)
)

bias_runner <- readLines(
  file.path(bias_root, "RUN_IN_RSTUDIO.R"),
  warn = FALSE,
  encoding = "UTF-8"
)
stopifnot(
  any(grepl('run_id = "bias_decomposition_n5000_reps300_final_v1"', bias_runner, fixed = TRUE)),
  any(grepl("repeats = 300L", bias_runner, fixed = TRUE)),
  any(grepl("n = 5000L", bias_runner, fixed = TRUE))
)

reference_pngs <- list.files(
  file.path(repo_root, "experiments"),
  pattern = "[.]png$",
  recursive = TRUE,
  full.names = TRUE
)
stopifnot(length(reference_pngs) == 9L)
stopifnot(all(file.info(reference_pngs)$size > 0))

portable_files <- list.files(
  repo_root,
  pattern = "[.](R|md|txt)$",
  recursive = TRUE,
  full.names = TRUE
)
portable_paths <- gsub("\\\\", "/", portable_files)
portable_files <- portable_files[
  !grepl("/(renv/library|runs|cache)/", portable_paths)
]
absolute_path_hits <- unlist(lapply(portable_files, function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  drive_path_pattern <- "(^|[\\\"' (])[A-Za-z]:[/\\\\]"
  if (any(grepl(drive_path_pattern, lines))) path else character()
}), use.names = FALSE)
if (length(absolute_path_hits) > 0L) {
  stop("Unexpected absolute path(s):\n", paste(absolute_path_hits, collapse = "\n"))
}

cat(
  "Repository checks passed.\n",
  "Parsed R files: ", length(r_files), "\n",
  "Formal Monte Carlo replications: 300 per experiment\n",
  "Reference figures: ", length(reference_pngs), "\n",
  "Code comments: 0\n",
  "No simulation was run.\n",
  sep = ""
)
