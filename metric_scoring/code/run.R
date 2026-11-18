source("metric_scoring/code/score_records.R")
source("metric_scoring/code/variance_analysis.R")
source("metric_scoring/code/figure_components.R")

pkgs <- c("dplyr", "tidyr", "ggplot2", "scales")
versions <- vapply(pkgs, function(p) as.character(packageVersion(p)), character(1))
writeLines(c(R.version.string, paste(pkgs, versions)),
           "metric_scoring/output/tables/session_info.txt")
