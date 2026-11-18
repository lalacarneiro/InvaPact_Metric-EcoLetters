build_figure_S4 <- function(ctx, intermediates) {

  D1_results <- intermediates$D1_results
  cross_long <- intermediates$cross_long
  cross_heat <- intermediates$cross_heat

  eco_abbr_map <- c(
    "Individual"                 = "Ind",
    "Population"                 = "Pop",
    "Species"                    = "Sp",
    "Assemblage"                 = "Asm",
    "Ecosystem function/service" = "EFS",
    "Abiotic environment"        = "Abi"
  )

  pA <- D1_results %>%
    mutate(eco_level = factor(eco_level, levels = eco_level_order)) %>%
    ggplot(aes(x = IP_D1_baseline, y = IP_D1_T1_mean, colour = eco_level)) +
    geom_errorbar(aes(ymin = IP_D1_T1_q025, ymax = IP_D1_T1_q975),
                  alpha = 0.18, width = 0.4) +
    geom_point(alpha = 0.55, size = 1.0) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    scale_colour_manual(values = eco_colours, name = "Level", labels = eco_abbr_map) +
    scale_x_continuous(limits = c(0, 100)) +
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text    = element_text(size = 10),
      axis.title   = element_text(size = 11),
      legend.text  = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.position = "inside",
      legend.position.inside = c(0.98, 0.02),
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = scales::alpha("white", 0.7), colour = NA)
    ) +
    labs(
      x = "Baseline InvaPact (T1)",
      y = "InvaPact under S perturbation\n(mean \u00b1 95% CI)"
    )

  cross_long_T1 <- cross_long %>%
    filter(T_var == "T1 \u2014 EMM") %>%
    mutate(
      S_scen = ifelse(as.character(S_scen) == "D1 (cell noise mean)", "RN", as.character(S_scen)),
      S_scen = factor(S_scen, levels = c("canonical", "alt1", "alt2", "RN")),
      eco_abbr = factor(eco_abbr_map[as.character(eco_level)],
                        levels = eco_abbr_map[eco_level_order])
    )

  pB <- ggplot(cross_long_T1,
               aes(x = eco_abbr, y = mean_IP, colour = S_scen, group = S_scen)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.4) +
    scale_colour_brewer(palette = "Set1", name = NULL) +
    guides(colour = guide_legend(ncol = 2, byrow = TRUE)) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text    = element_text(size = 10),
      axis.title   = element_text(size = 11),
      legend.text  = element_text(size = 9),
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.direction = "horizontal",
      legend.background = element_blank(),
      axis.text.x  = element_text(angle = 20, hjust = 1)
    ) +
    labs(x = "Ecological level", y = "Mean InvaPact")

  cross_heat_T1 <- cross_heat %>%
    filter(T_var == "T1 \u2014 EMM") %>%
    mutate(
      S_scen = ifelse(as.character(S_scen) == "D1", "RN", as.character(S_scen)),
      S_scen = factor(S_scen, levels = c("canonical", "alt1", "alt2", "RN")),
      eco_abbr = factor(eco_abbr_map[as.character(eco_level)],
                        levels = eco_abbr_map[eco_level_order])
    )

  pC <- ggplot(cross_heat_T1, aes(x = S_scen, y = eco_abbr, fill = mean_IP)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = round(mean_IP, 1)), size = 3.5, colour = "grey10") +
    scale_fill_gradientn(
      colours = c("#2C7BB6", "#FFFFBF", "#B2182B"),
      limits  = c(0, 100),
      name    = "Mean\nInvaPact"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text    = element_text(size = 10),
      axis.title   = element_text(size = 11),
      legend.text  = element_text(size = 9),
      legend.title = element_text(size = 10)
    ) +
    labs(x = "S scenario", y = "Ecological level")

  p_S4 <- pA / (pB | pC) +
    patchwork::plot_layout(widths = c(1.25, 1.0), heights = c(1, 1.05)) +
    patchwork::plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(face = "bold", size = 12))

  save_letter_figure(ctx, p_S4, "Figure_S4.png", width = 8.2, height = 7.6)
}
