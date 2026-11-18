source("sensitivity_eco_letters/code/00_helpers.R")
source("sensitivity_eco_letters/code/01_figures_S1_S2_S3_S5.R")
source("sensitivity_eco_letters/code/02_figure_S4.R")

ctx <- new_context()
ensure_dirs(ctx)

intermediates <- build_figures_S1_S2_S3_S5(ctx)
build_figure_S4(ctx, intermediates)

pkgs <- c("readxl", "dplyr", "tidyr", "stringr", "ggplot2", "jsonlite",
          "tibble", "scales", "patchwork", "truncnorm")
versions <- vapply(pkgs, function(p) as.character(packageVersion(p)), character(1))
writeLines(c(R.version.string, paste(pkgs, versions)),
           file.path(ctx$table_dir, "session_info.txt"))
