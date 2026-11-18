suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

scoring_S <- read.csv("metric_scoring/data/scoring_S.csv")
scoring_T <- read.csv("metric_scoring/data/scoring_T.csv")
fig_dir   <- "metric_scoring/output/figures"

combos <- inner_join(scoring_S, scoring_T, by = c("impact_type", "eco_level"),
                     relationship = "many-to-many") |>
  mutate(I_raw = 10 * sqrt(S_norm * T))
I_max  <- max(combos$I_raw)
combos <- mutate(combos, score = 100 * I_raw / I_max)

score_of <- function(S, T) 100 * 10 * sqrt(S * T) / I_max

examples <- combos |>
  filter(Environment == "Terrestrial", Group == "Invertebrates",
         (impact_type == "imp_fire_regime"          & eeicat_raw == "moderate") |
         (impact_type == "imp_fitness_reproduction" & eeicat_raw == "minor")) |>
  arrange(desc(S_norm)) |>
  mutate(label = c("A", "B"))

score_ramp <- c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c",
                "#fc4e2a", "#e31a1c", "#bd0026", "#800026")

fig_theme <- theme_minimal(base_size = 9) +
  theme(axis.title         = element_text(colour = "grey20"),
        axis.text          = element_text(size = rel(0.9), colour = "grey35"),
        axis.line          = element_line(colour = "grey55", linewidth = 0.3),
        axis.ticks         = element_line(colour = "grey55", linewidth = 0.3),
        axis.ticks.length  = unit(2, "pt"),
        panel.grid         = element_blank(),
        legend.title       = element_text(size = rel(0.9), colour = "grey20"),
        legend.text        = element_text(size = rel(0.85), colour = "grey35"),
        legend.position    = "right",
        legend.box.spacing = unit(1.5, "mm"),
        legend.margin      = margin(0, 0, 0, 0),
        legend.key.height  = unit(14, "mm"),
        legend.key.width   = unit(2.5, "mm"),
        plot.margin        = margin(3, 3, 3, 3))

score_fill <- scale_fill_gradientn(colours = score_ramp, limits = c(0, 100),
                                   breaks = seq(0, 100, 25), oob = scales::squish,
                                   na.value = "grey85", name = "Score")

example_layers <- list(
  geom_point(data = examples, aes(T, S_norm), shape = 21, size = 2.4,
             fill = "white", colour = "black", stroke = 0.8),
  geom_text(data = examples, aes(T, S_norm, label = label),
            size = 2.6, fontface = "bold", nudge_x = 0.35, nudge_y = 0.35)
)

axes <- list(
  scale_x_continuous(breaks = 1:10, expand = c(0, 0)),
  scale_y_continuous(breaks = 0:10, expand = c(0, 0)),
  coord_fixed(xlim = c(1, 10), ylim = c(0, 10), clip = "off"),
  labs(x = "Type importance (T)", y = "Severity (S)")
)

save_fig <- function(plot, name) {
  for (ext in c("png", "pdf")) {
    ggsave(file.path(fig_dir, paste0(name, ".", ext)), plot,
           width = 95, height = 85, units = "mm", dpi = 600, bg = "white")
  }
}


bins <- expand_grid(T = seq(1.5, 9.5, by = 1), S = seq(0.5, 9.5, by = 1))

bin_means <- combos |>
  transmute(T = pmin(floor(T), 9) + 0.5, S = pmin(floor(S_norm), 9) + 0.5, score) |>
  group_by(T, S) |>
  summarise(score = mean(score), .groups = "drop")

bins <- left_join(bins, bin_means, by = c("T", "S"))

p_heatmap <- ggplot() +
  geom_tile(data = bins, aes(T, S, fill = score),
            width = 1, height = 1, colour = "white", linewidth = 0.4) +
  example_layers + score_fill + axes + fig_theme

save_fig(p_heatmap, "Figure_heatmap_mean_real")


surface <- expand_grid(T = seq(1, 10, length.out = 400),
                       S = seq(0, 10, length.out = 400)) |>
  mutate(score = score_of(S, T))

contours <- lapply(c(20, 40, 60, 80), function(level) {
  k <- (level * I_max / 1000)^2
  S <- seq(k / 10, min(k, 10), length.out = 400)
  tibble(level = level, S = S, T = k / S)
}) |>
  bind_rows() |>
  filter(S <= 10)

contour_labels <- contours |>
  group_by(level) |>
  slice_min(abs(T - 9.4), n = 1, with_ties = FALSE) |>
  ungroup()

p_curves <- ggplot() +
  geom_raster(data = surface, aes(T, S, fill = score), interpolate = TRUE) +
  geom_point(data = combos, aes(T, S_norm), colour = "grey15", alpha = 0.25,
             size = 0.35, stroke = 0) +
  geom_path(data = contours, aes(T, S, group = level),
            colour = "white", linewidth = 0.4, linetype = "22") +
  geom_label(data = contour_labels, aes(T, S, label = level),
             size = 2.2, colour = "grey20", fill = "white", alpha = 0.85,
             linewidth = 0, label.padding = unit(0.6, "mm")) +
  example_layers + score_fill + axes + fig_theme

save_fig(p_curves, "Figure_curves_attainable")
