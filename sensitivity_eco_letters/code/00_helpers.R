suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(jsonlite)
  library(tibble)
  library(scales)
  library(patchwork)
  library(truncnorm)
})

set.seed(20260221)


new_context <- function(root = "sensitivity_eco_letters") {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  list(
    root        = root,
    raw_dir     = file.path(root, "data", "raw"),
    derived_dir = file.path(root, "data", "derived"),
    fig_dir     = file.path(root, "output", "figures"),
    table_dir   = file.path(root, "output", "tables")
  )
}

ensure_dirs <- function(ctx) {
  for (d in c(ctx$derived_dir, ctx$fig_dir, ctx$table_dir))
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

save_letter_figure <- function(ctx, plot_obj, name, width = 10, height = 6, dpi = 300, units = "in") {
  ggsave(file.path(ctx$fig_dir, name), plot = plot_obj,
    width = width, height = height, dpi = dpi, units = units)
}

apply_impact_type_mapping <- function(data, mapping_file) {
  mapping <- jsonlite::fromJSON(mapping_file)
  rename_vector <- setNames(mapping$impact_type_key, mapping$imp_types)
  cols_to_rename <- intersect(names(data), rename_vector)
  if (length(cols_to_rename) > 0) {
    rename_subset <- rename_vector[rename_vector %in% cols_to_rename]
    data <- data %>% rename(!!!rename_subset)
  }
  data
}

make_S_matrices <- function(lambda = 0.02, theta = 3.5, digits = 4) {
  R_vals <- 0:4
  R_names <- c("mc", "mn", "mo", "mr", "mv")
  L_vals <- 1:3
  L_names <- c("population", "assemblage", "ecosystem")

  f <- function(R) R + lambda * R^2
  d <- function(R) ifelse(R == 0, 0, 1 + (R - 1) / ((R - 1) + theta))
  S_r <- function(R, L) ifelse(R == 0, 0, f(R) + (L - 1) * d(R))

  S_raw <- outer(R_vals, L_vals, FUN = S_r)
  dimnames(S_raw) <- list(R_names, L_names)

  Smin <- S_raw["mn", "population"]
  Smax <- S_raw["mv", "ecosystem"]

  S <- S_raw
  S[R_names[R_vals >= 1], ] <- 1 + 9 * (S_raw[R_names[R_vals >= 1], ] - Smin) / (Smax - Smin)
  S["mc", ] <- 0

  list(lambda = lambda, theta = theta, S_raw = round(S_raw, digits), S_norm = round(S, digits), Smin = Smin, Smax = Smax)
}

invapact <- function(S, T) 10 * sqrt(pmax(S, 0) * pmax(T, 0))

eco_level_order <- c(
  "Individual", "Population", "Species",
  "Assemblage", "Ecosystem function/service", "Abiotic environment"
)

eco_level_map <- c(
  imp_fitness_reproduction = "Individual",
  imp_health_phenotype = "Individual",
  imp_behaviour = "Individual",
  imp_population_size = "Population",
  imp_genetic_diversity = "Population",
  imp_range_change = "Species",
  imp_species_persistence = "Species",
  imp_assemblage_structure = "Assemblage",
  imp_successional_patterns = "Assemblage",
  imp_soundscape = "Assemblage",
  imp_primary_production = "Ecosystem function/service",
  imp_ecological_function = "Ecosystem function/service",
  imp_food_web = "Ecosystem function/service",
  imp_habitat_refugia = "Ecosystem function/service",
  imp_hydrology_water_quality = "Abiotic environment",
  imp_nutrients_pool_fluxes = "Abiotic environment",
  imp_fire_regime = "Abiotic environment",
  imp_soil_sediment = "Abiotic environment",
  imp_climate = "Abiotic environment"
)

eco_colours <- c(
  "Individual" = "mediumpurple4",
  "Population" = "firebrick4",
  "Species" = "darkorange2",
  "Assemblage" = "olivedrab4",
  "Ecosystem function/service" = "darkgreen",
  "Abiotic environment" = "deepskyblue2"
)

eeicat_rowcol_map <- list(
  imp_fitness_reproduction = list(rows = c("mc", "mn", "mo"), col = "population"),
  imp_health_phenotype = list(rows = c("mc", "mn", "mo"), col = "population"),
  imp_behaviour = list(rows = c("mc", "mn", "mo"), col = "population"),
  imp_population_size = list(rows = c("mc", "mo", "mr", "mv"), col = "population"),
  imp_genetic_diversity = list(rows = c("mc", "mo", "mr", "mv"), col = "population"),
  imp_range_change = list(rows = c("mc", "mn", "mo", "mr"), col = "assemblage"),
  imp_species_persistence = list(rows = c("mc", "mo", "mr", "mv"), col = "assemblage"),
  imp_assemblage_structure = list(rows = c("mc", "mo", "mr", "mv"), col = "assemblage"),
  imp_successional_patterns = list(rows = c("mc", "mo", "mr", "mv"), col = "assemblage"),
  imp_soundscape = list(rows = c("mc", "mo", "mr", "mv"), col = "assemblage"),
  imp_primary_production = list(rows = c("mc", "mo", "mr", "mv"), col = "ecosystem"),
  imp_ecological_function = list(rows = c("mc", "mo", "mr", "mv"), col = "ecosystem"),
  imp_food_web = list(rows = c("mc", "mo", "mr", "mv"), col = "ecosystem"),
  imp_habitat_refugia = list(rows = c("mc", "mo", "mr", "mv"), col = "ecosystem"),
  imp_hydrology_water_quality = list(rows = c("mc", "mn", "mo", "mr", "mv"), col = "ecosystem"),
  imp_nutrients_pool_fluxes = list(rows = c("mc", "mn", "mo", "mr", "mv"), col = "ecosystem"),
  imp_fire_regime = list(rows = c("mc", "mn", "mo", "mr", "mv"), col = "ecosystem"),
  imp_soil_sediment = list(rows = c("mc", "mn", "mo", "mr", "mv"), col = "ecosystem"),
  imp_climate = list(rows = c("mc", "mn", "mo", "mr", "mv"), col = "ecosystem")
)

get_max_S <- function(Snorm_mat, impact_type_name) {
  rc <- eeicat_rowcol_map[[impact_type_name]]
  if (is.null(rc)) return(NA_real_)
  nonmc <- rc$rows[rc$rows != "mc"]
  if (length(nonmc) == 0) return(0)
  max(Snorm_mat[nonmc, rc$col])
}
