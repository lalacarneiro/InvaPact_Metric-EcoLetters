suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
})

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0 || any(!is.finite(rng))) return(rep(1, length(x)))
  (x - rng[1]) / diff(rng)
}

generate_figure_priority <- function(input_path, output_path, table_path = NULL) {
  df <- read.csv(input_path, stringsAsFactors = FALSE)

  required_cols <- c(
    "invasive_species_name",
    "invapact_score",
    "sampling_country",
    "impact_type"
  )
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns in papers_dataset.csv: %s", paste(missing_cols, collapse = ", ")))
  }

  species_table <- df %>%
    group_by(invasive_species_name) %>%
    summarise(
      avg_invapact_score = mean(invapact_score, na.rm = TRUE),
      sd_invapact_score = sd(invapact_score, na.rm = TRUE),
      n_rows = n(),
      n_unique_impact_types = n_distinct(impact_type, na.rm = TRUE),
      n_countries = n_distinct(sampling_country, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(avg_invapact_score), desc(n_rows), invasive_species_name)

  species_working <- species_table %>%
    filter(n_rows > 3)

  if (nrow(species_working) == 0) {
    stop("No species with n_rows > 3 available for priority matrix.")
  }

  species_working <- species_working %>%
    mutate(
      score_scaled = rescale01(avg_invapact_score),
      evidence_scaled = rescale01(log1p(n_rows)),
      country_scaled = rescale01(n_countries),
      impact_scaled = if (all(is.na(n_unique_impact_types))) {
        1
      } else {
        rescale01(n_unique_impact_types)
      },
      priority_score = 100 * (
        0.40 * score_scaled +
        0.35 * evidence_scaled +
        0.15 * country_scaled +
        0.10 * impact_scaled
      )
    )

  rows_cutoff <- median(species_working$n_rows, na.rm = TRUE)
  score_cutoff <- median(species_working$avg_invapact_score, na.rm = TRUE)

  species_working <- species_working %>%
    mutate(
      decision_group = case_when(
        avg_invapact_score >= score_cutoff & n_rows >= rows_cutoff ~ "high score / high evidence",
        avg_invapact_score >= score_cutoff & n_rows < rows_cutoff ~ "high score / lower evidence",
        avg_invapact_score < score_cutoff & n_rows >= rows_cutoff ~ "low score / high evidence",
        TRUE ~ "low score / low evidence"
      ),
      decision_group = factor(
        decision_group,
        levels = c(
          "high score / high evidence",
          "high score / lower evidence",
          "low score / high evidence",
          "low score / low evidence"
        )
      ),
      size_countries = ifelse(is.na(n_countries), 1, n_countries)
    )

  country_breaks <- sort(unique(round(seq(
    min(species_working$size_countries, na.rm = TRUE),
    max(species_working$size_countries, na.rm = TRUE),
    length.out = 4
  ))))

  priority_x_min <- min(species_working$n_rows, na.rm = TRUE)
  priority_x_max <- max(species_working$n_rows, na.rm = TRUE) * 1.15
  quadrant_shades <- data.frame(
    xmin = c(priority_x_min, rows_cutoff, priority_x_min, rows_cutoff),
    xmax = c(rows_cutoff, priority_x_max, rows_cutoff, priority_x_max),
    ymin = c(0, 0, score_cutoff, score_cutoff),
    ymax = c(score_cutoff, score_cutoff, 100, 100),
    fill = c("#DCE7F1", "#FFF1CC", "#EAF4D9", "#F7D8C1"),
    stringsAsFactors = FALSE
  )

  decision_colors <- c(
    "high score / high evidence" = "#CE6601",
    "high score / lower evidence" = "#FFB90C",
    "low score / high evidence" = "#A1CD5A",
    "low score / low evidence" = "#33638B"
  )

  p_priority <- ggplot(species_working, aes(x = n_rows, y = avg_invapact_score)) +
    geom_rect(
      data = quadrant_shades,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = quadrant_shades$fill,
      alpha = 0.16,
      color = NA
    ) +
    geom_vline(xintercept = rows_cutoff, linetype = "dashed", color = "gray55") +
    geom_hline(yintercept = score_cutoff, linetype = "dashed", color = "gray55") +
    geom_point(aes(size = size_countries, color = decision_group), alpha = 0.85) +
    scale_color_manual(values = decision_colors) +
    scale_size_continuous(
      range = c(1.4, 4.8),
      breaks = country_breaks,
      labels = scales::label_number(accuracy = 1)
    ) +
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    labs(
      x = "Number of impact events",
      y = "InvaPact score",
      color = "Decision group",
      size = "n of countries"
    ) +
    theme_classic(base_size = 8) +
    theme(
      axis.text = element_text(size = 5.2),
      axis.title = element_text(size = 5.1),
      legend.text = element_text(size = 4.8),
      legend.title = element_text(size = 5.2)
    ) +
    expand_limits(x = priority_x_max)

  ggsave(output_path, p_priority, width = 82 / 25.4, height = (82 * 0.84) / 25.4, dpi = 600, bg = "white")

  if (!is.null(table_path)) {
    write.csv(species_working, table_path, row.names = FALSE)
  }
}
