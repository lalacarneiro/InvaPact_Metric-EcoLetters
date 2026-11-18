suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(grid)
  library(tidyr)
})

normalize_token <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("_", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

ecosystem_rules <- c(
  Wetlands   = "wetland|marsh|swamp|mangrove|peatland|bog",
  Marine     = "marine|ocean|coral|reef|shoreline|intertidal|subtidal|kelp|seagrass|estuar|lagoon|delta|benthic",
  Freshwater = "fresh|river|stream|lake|pond|reservoir|canal|floodplain|riparian",
  Savanna    = "savanna|grassland|desert|arid|shrubland|steppe",
  Forest     = "forest|woodland"
)

bin_ecosystem <- function(ecosystem) {
  vapply(ecosystem, function(cell) {
    if (is.na(cell) || trimws(cell) == "") return("Mixed")
    tokens <- normalize_token(unlist(strsplit(cell, ";")))
    tokens <- tokens[tokens != ""]
    bins <- vapply(tokens, function(tok) {
      hit <- names(ecosystem_rules)[vapply(ecosystem_rules, grepl, logical(1), x = tok)]
      if (length(hit) > 0) hit[1] else NA_character_
    }, character(1))
    bins <- unique(na.omit(bins))
    if (length(bins) == 1) bins else "Mixed"
  }, character(1), USE.NAMES = FALSE)
}

GeomHalfViolinLeft <- ggproto(
  "GeomHalfViolinLeft",
  GeomViolin,
  draw_group = function(self, data, panel_params, coord, ...) {
    data <- transform(
      data,
      xminv = x,
      xmaxv = x - violinwidth * (x - xmin)
    )
    newdata <- rbind(
      dplyr::arrange(transform(data, x = xminv), y),
      dplyr::arrange(transform(data, x = xmaxv), -y)
    )
    newdata <- rbind(newdata, newdata[1, ])
    GeomPolygon$draw_panel(newdata, panel_params, coord)
  }
)

geom_half_violin_left <- function(mapping = NULL, data = NULL,
                                  stat = "ydensity", position = "identity",
                                  ..., trim = TRUE, scale = "area",
                                  na.rm = FALSE, show.legend = NA,
                                  inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHalfViolinLeft,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(trim = trim, scale = scale, na.rm = na.rm, ...)
  )
}

draw_panel_custom <- function(plot_a, plot_b, plot_c, plot_d, out_path,
                              labels = c("A", "B", "C", "D"),
                              width_in = 173 / 25.4,
                              height_in = 173 / 25.4,
                              dpi = 600) {
  grDevices::png(out_path, width = width_in, height = height_in, units = "in", res = dpi, bg = "white")
  grid::grid.newpage()

  lay <- grid::grid.layout(
    nrow = 2,
    ncol = 2,
    widths = grid::unit(c(1, 1.08), "null"),
    heights = grid::unit(c(1, 1), "null")
  )
  grid::pushViewport(grid::viewport(layout = lay))

  plot_list <- list(plot_a, plot_b, plot_c, plot_d)
  for (i in seq_along(plot_list)) {
    row_idx <- ((i - 1) %/% 2) + 1
    col_idx <- ((i - 1) %% 2) + 1

    grid::pushViewport(grid::viewport(layout.pos.row = row_idx, layout.pos.col = col_idx))
    panel_width <- 0.97
    panel_height <- 0.90
    if (i == 3) {
      panel_width <- 0.92
      panel_height <- 0.84
    }
    print(plot_list[[i]], vp = grid::viewport(x = 0.5, y = 0.54, width = panel_width, height = panel_height))
    grid::grid.text(
      labels[[i]],
      x = grid::unit(0.02, "npc"),
      y = grid::unit(0.98, "npc"),
      just = c("left", "top"),
      gp = grid::gpar(fontsize = 13, fontface = "bold", col = "black")
    )
    grid::popViewport()
  }

  grid::popViewport()
  grDevices::dev.off()
}

generate_figure_panel <- function(input_path, mechanism_path, output_path) {
  df <- read.csv(input_path, stringsAsFactors = FALSE)
  mech <- read.csv(mechanism_path, stringsAsFactors = FALSE)

  required_cols <- c(
    "invasive_species_name", "invapact_score", "start_sampling_year",
    "impact_type", "eco_level", "tax_group_plot", "ecosystem"
  )
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns in papers_dataset.csv: %s", paste(missing_cols, collapse = ", ")))
  }

  required_mech_cols <- c("tax_group_mech", "invapact_score", "mechanism_clean", "mechanism_weight")
  missing_mech_cols <- setdiff(required_mech_cols, names(mech))
  if (length(missing_mech_cols) > 0) {
    stop(sprintf("Missing required columns in papers_dataset_mechanisms.csv: %s", paste(missing_mech_cols, collapse = ", ")))
  }

  eco_palette <- c(
    "Individual" = "mediumpurple4",
    "Population" = "firebrick4",
    "Species" = "darkorange3",
    "Assemblage" = "olivedrab4",
    "Ecosystem Function" = "darkgreen",
    "Abiotic" = "deepskyblue1"
  )

  tax_palette <- c(
    "Vertebrates" = "#A83220",
    "Plants" = "darkgoldenrod",
    "Microorganisms" = "#4B9E8E",
    "Invertebrates" = "mediumpurple4",
    "Unknown" = "#9E9E9E"
  )

  PANEL_TEXT_SIZE_PT <- 8.8
  PANEL_AXIS_TITLE_PT <- 9.8
  PANEL_LEGEND_TITLE_PT <- 8.8
  PANEL_LEGEND_TEXT_PT <- 8.0

  common_theme <- theme(
    axis.text = element_text(size = PANEL_TEXT_SIZE_PT),
    axis.title = element_text(size = PANEL_AXIS_TITLE_PT),
    legend.title = element_text(size = PANEL_LEGEND_TITLE_PT, face = "bold"),
    legend.text = element_text(size = PANEL_LEGEND_TEXT_PT),
    plot.title = element_text(size = PANEL_LEGEND_TITLE_PT, face = "bold"),
    plot.margin = margin(t = 5, r = 6, b = 6, l = 6)
  )

  df_box <- df %>%
    filter(!is.na(invapact_score), !is.na(tax_group_plot)) %>%
    mutate(
      tax_group_plot = factor(
        tax_group_plot,
        levels = c("Invertebrates", "Plants", "Vertebrates", "Microorganisms", "Unknown")
      )
    ) %>%
    filter(!is.na(tax_group_plot), tax_group_plot != "Microorganisms", tax_group_plot != "Unknown")

  if (nrow(df_box) == 0) stop("No rows available to build panel A.")

  p_a <- ggplot(df_box, aes(x = tax_group_plot, y = invapact_score)) +
    geom_half_violin_left(
      aes(fill = tax_group_plot),
      trim = FALSE,
      alpha = 0.35,
      color = NA,
      width = 0.92
    ) +
    geom_boxplot(
      aes(fill = tax_group_plot),
      outlier.shape = NA,
      width = 0.20,
      alpha = 0.95,
      linewidth = 0.40
    ) +
    scale_fill_manual(values = tax_palette, guide = "none") +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    scale_x_discrete(expand = expansion(mult = c(0.02, 0.02))) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(x = "Taxonomic group", y = "InvaPact score") +
    theme_classic(base_size = 14) +
    common_theme +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, size = PANEL_TEXT_SIZE_PT))

  expanded <- mech %>%
    filter(
      !is.na(invapact_score),
      tax_group_mech %in% c("Invertebrates", "Plants", "Vertebrates"),
      !is.na(mechanism_clean),
      !is.na(mechanism_weight)
    )

  if (nrow(expanded) == 0) stop("No rows available in papers_dataset_mechanisms.csv for panel B.")

  top4 <- expanded %>%
    group_by(mechanism_clean) %>%
    summarise(weighted_n = sum(mechanism_weight, na.rm = TRUE), raw_occurrences = n(), .groups = "drop") %>%
    arrange(desc(weighted_n), desc(raw_occurrences), mechanism_clean) %>%
    slice_head(n = 4)

  expanded_top4 <- expanded %>% filter(mechanism_clean %in% top4$mechanism_clean)

  ci_df <- expanded_top4 %>%
    group_by(tax_group_mech, mechanism_clean) %>%
    summarise(
      n = n(),
      mean_score = weighted.mean(invapact_score, mechanism_weight, na.rm = TRUE),
      se = sqrt(Hmisc::wtd.var(invapact_score, mechanism_weight, normwt = TRUE) / n),
      .groups = "drop"
    )

  mech_order <- ci_df %>%
    group_by(mechanism_clean) %>%
    summarise(overall_mean = mean(mean_score), .groups = "drop") %>%
    arrange(overall_mean) %>%
    pull(mechanism_clean)

  ci_df <- ci_df %>%
    mutate(
      ci_lo = pmax(0, mean_score - qt(0.975, df = pmax(n - 1, 1)) * se),
      ci_hi = pmin(100, mean_score + qt(0.975, df = pmax(n - 1, 1)) * se),
      mechanism_clean = factor(mechanism_clean, levels = mech_order),
      tax_group_mech = factor(tax_group_mech, levels = c("Invertebrates", "Plants", "Vertebrates"))
    )

  p_b <- ggplot(ci_df, aes(x = mechanism_clean, y = mean_score, fill = tax_group_mech)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.88) +
    geom_errorbar(
      aes(ymin = ci_lo, ymax = ci_hi),
      position = position_dodge(width = 0.75),
      width = 0.22,
      linewidth = 0.70,
      color = "grey30"
    ) +
    scale_fill_manual(values = tax_palette[c("Invertebrates", "Plants", "Vertebrates")], name = NULL) +
    scale_x_discrete(labels = function(x) tools::toTitleCase(as.character(x))) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), expand = expansion(mult = c(0, 0.03))) +
    labs(x = "Mechanism", y = "InvaPact score") +
    theme_classic(base_size = 14) +
    common_theme +
    guides(fill = guide_legend(ncol = 1, byrow = TRUE, title = NULL)) +
    theme(
      axis.text.x = element_text(angle = 15, hjust = 1, size = PANEL_TEXT_SIZE_PT),
      legend.position = "inside",
      legend.position.inside = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.direction = "vertical",
      legend.background = element_rect(fill = scales::alpha("white", 0.86), color = NA),
      legend.key.height = unit(0.20, "cm"),
      legend.key.width = unit(0.38, "cm")
    )

  eco_level_map <- c(
    "Individual" = "Individual",
    "Population" = "Population",
    "Species" = "Species",
    "Assemblage" = "Assemblage",
    "Ecosystem function/service" = "Ecosystem Function",
    "Abiotic environment" = "Abiotic"
  )

  yearly_ecolevel <- df %>%
    filter(!is.na(start_sampling_year), start_sampling_year >= 1950, start_sampling_year <= 2025) %>%
    mutate(eco_level_panel = dplyr::recode(eco_level, !!!eco_level_map, .default = NA_character_)) %>%
    filter(!is.na(eco_level_panel)) %>%
    group_by(start_sampling_year, eco_level_panel) %>%
    summarise(mean_score = mean(invapact_score, na.rm = TRUE), n = n(), .groups = "drop") %>%
    filter(n >= 2) %>%
    mutate(
      eco_level_panel = factor(
        eco_level_panel,
        levels = c("Individual", "Population", "Species", "Assemblage", "Ecosystem Function", "Abiotic")
      )
    )

  if (nrow(yearly_ecolevel) == 0) stop("No valid data available for panel C.")

  year_breaks <- seq(
    floor(min(yearly_ecolevel$start_sampling_year, na.rm = TRUE) / 10) * 10,
    ceiling(max(yearly_ecolevel$start_sampling_year, na.rm = TRUE) / 10) * 10,
    by = 10
  )

  p_c <- ggplot(yearly_ecolevel, aes(x = start_sampling_year, y = mean_score, color = eco_level_panel, size = n)) +
    geom_point(alpha = 0.70) +
    geom_smooth(
      data = yearly_ecolevel,
      aes(x = start_sampling_year, y = mean_score, color = eco_level_panel, weight = n, group = eco_level_panel),
      inherit.aes = FALSE,
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      linewidth = 0.90,
      fill = "grey95",
      alpha = 0.10
    ) +
    scale_color_manual(values = eco_palette, name = NULL) +
    scale_size_continuous(range = c(0.8, 2.4), guide = "none") +
    scale_x_continuous(breaks = year_breaks, expand = expansion(mult = c(0.06, 0.02))) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    labs(x = "Start of sampling year", y = "InvaPact score") +
    theme_classic(base_size = 14) +
    common_theme +
    guides(color = guide_legend(ncol = 2, byrow = TRUE, title = NULL), size = "none") +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, size = PANEL_TEXT_SIZE_PT),
      legend.position = "inside",
      legend.position.inside = c(0.91, 0.98),
      legend.justification = c(1, 1),
      legend.direction = "vertical",
      legend.box = "vertical",
      legend.background = element_rect(fill = scales::alpha("white", 0.86), color = NA),
      legend.key.height = unit(0.16, "cm"),
      legend.key.width = unit(0.28, "cm")
    )

  impact_type_order <- c(
    "imp_fitness_reproduction", "imp_health_phenotype", "imp_behaviour", "imp_population_size", "imp_genetic_diversity",
    "imp_range_change", "imp_species_persistence", "imp_assemblage_structure", "imp_successional_patterns", "imp_soundscape",
    "imp_primary_production", "imp_ecological_function", "imp_food_web", "imp_habitat_refugia",
    "imp_hydrology_water_quality", "imp_nutrients_pool_fluxes", "imp_fire_regime", "imp_soil_sediment", "imp_climate"
  )

  build_heatmap_panel <- function(data_in, x_col, x_axis_label, palette_colors, legend_name = NULL) {
    heatmap_df <- data_in %>%
      filter(!is.na(impact_type), !is.na(.data[[x_col]])) %>%
      group_by(impact_type, .data[[x_col]]) %>%
      summarise(mean_invapact_score = mean(invapact_score, na.rm = TRUE), .groups = "drop") %>%
      mutate(impact_type_plot = impact_type, x_plot = .data[[x_col]]) %>%
      select(impact_type_plot, x_plot, mean_invapact_score)

    impact_present <- unique(heatmap_df$impact_type_plot)
    impact_order <- c(intersect(impact_type_order, impact_present), sort(setdiff(impact_present, impact_type_order)))
    impact_labels_index <- setNames(seq_along(impact_order), impact_order)

    x_order <- c("Freshwater", "Marine", "Wetlands", "Savanna", "Forest", "Mixed")

    heatmap_df <- heatmap_df %>%
      mutate(
        impact_type_plot = factor(impact_type_plot, levels = impact_order),
        x_plot = factor(x_plot, levels = x_order)
      )

    ggplot(heatmap_df, aes(x = x_plot, y = impact_type_plot, fill = mean_invapact_score)) +
      geom_tile(width = 0.90, height = 0.90, color = "white", linewidth = 0.20) +
      scale_y_discrete(limits = rev(impact_order), labels = function(x) as.character(impact_labels_index[x])) +
      scale_x_discrete(labels = x_order) +
      scale_fill_gradientn(
        colors = palette_colors,
        values = scales::rescale(c(0, 15, 35, 58, 80, 100)),
        limits = c(0, 100),
        breaks = seq(0, 100, 20),
        name = legend_name
      ) +
      labs(x = x_axis_label, y = "Impact type index") +
      theme_minimal(base_size = 14) +
      common_theme +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1, size = PANEL_TEXT_SIZE_PT - 1.2),
        axis.text.y = element_text(size = PANEL_TEXT_SIZE_PT),
        axis.title = element_text(face = "plain", size = PANEL_AXIS_TITLE_PT),
        legend.position = "right",
        legend.justification = c(0.5, 0.5),
        legend.title = element_text(size = PANEL_TEXT_SIZE_PT - 1.8, face = "plain", margin = margin(b = 3)),
        legend.text = element_text(size = PANEL_TEXT_SIZE_PT - 0.8),
        legend.key.height = unit(0.28, "cm"),
        legend.key.width = unit(0.26, "cm")
      ) +
      guides(fill = guide_colorbar(title = legend_name, barheight = unit(3.1, "cm"), barwidth = unit(0.34, "cm")))
  }

  df_for_ecosystem_heatmap <- df %>%
    mutate(ecosystem_group_plot = bin_ecosystem(ecosystem))

  eco_heat_palette <- c("#E7D7F6", "#C7A1C8", "#A66F9A", "#B57A6A", "#D79A4A", "#F2BC4C")

  p_d_ecosystem <- build_heatmap_panel(
    df_for_ecosystem_heatmap,
    "ecosystem_group_plot",
    "Ecosystem type",
    palette_colors = eco_heat_palette,
    legend_name = "InvaPact\nscore"
  )

  draw_panel_custom(p_a, p_b, p_d_ecosystem, p_c, output_path, labels = c("A", "B", "C", "D"))
}
