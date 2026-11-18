suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(grid)
  library(tidyr)
  library(scales)
})

set.seed(20260430)

new_context <- function(root = "data_analysis") {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  list(
    root = root,
    data_dir = file.path(root, "data"),
    code_dir = file.path(root, "code"),
    fig_dir = file.path(root, "output", "figures"),
    table_dir = file.path(root, "output", "tables")
  )
}

ensure_dirs <- function(ctx) {
  for (d in c(ctx$fig_dir, ctx$table_dir)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
}

write_session_info <- function(ctx) {
  pkgs <- c("dplyr", "ggplot2", "tidyr", "scales", "Hmisc")
  versions <- vapply(pkgs, function(p) as.character(packageVersion(p)), character(1))
  writeLines(c(R.version.string, paste(pkgs, versions)),
             file.path(ctx$table_dir, "session_info.txt"))
}
