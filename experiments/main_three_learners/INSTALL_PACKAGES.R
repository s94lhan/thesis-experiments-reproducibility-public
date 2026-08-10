cran_packages <- c(
  "data.table",
  "ggplot2",
  "mgcv",
  "randomForest",
  "earth",
  "glmnet",
  "gbm",
  "xgboost",
  "nnls",
  "origami",
  "ranger",
  "remotes"
)

missing_cran <- cran_packages[!vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_cran) > 0L) {
  install.packages(missing_cran)
}

if (!requireNamespace("sl3", quietly = TRUE)) {
  remotes::install_github("tlverse/sl3@0e8f2365bcbe54010b8120c04a7a2dcfc8119227")
}

cat("Package installation check complete.\n")
