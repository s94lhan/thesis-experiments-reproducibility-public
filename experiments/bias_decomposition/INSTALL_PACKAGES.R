required_packages <- c("data.table", "ggplot2")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

message("Required packages are available: ", paste(required_packages, collapse = ", "))
