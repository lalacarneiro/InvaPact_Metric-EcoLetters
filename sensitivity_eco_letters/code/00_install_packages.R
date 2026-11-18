required_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "ggplot2",
  "jsonlite", "tibble", "scales", "patchwork", "truncnorm"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

cat("Package check complete.\n")
