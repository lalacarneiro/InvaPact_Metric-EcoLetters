required_packages <- c(
  "dplyr", "ggplot2", "grid", "tidyr", "scales", "Hmisc"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

cat("Package check complete.\n")
