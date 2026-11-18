build_figures_S1_S2_S3_S5 <- function(ctx) {
  raw_dir <- ctx$raw_dir
  derived_dir <- ctx$derived_dir

  path_survey <- file.path(raw_dir, "surveydata19082024.xlsx")
  path_emm_sc <- file.path(raw_dir, "emmeansscaled_T.csv")
  path_combis <- file.path(raw_dir, "imp_to_eeicatcombi.json")
  path_imp_map <- file.path(raw_dir, "impact_type_map.json")

  emm <- read.csv(path_emm_sc, stringsAsFactors = FALSE) %>%
    mutate(eco_level = factor(eco_level, levels = eco_level_order))

  lm_scale <- lm(emmean_1_10 ~ emmean, data = emm)
  scale_a <- coef(lm_scale)[["emmean"]]
  scale_b <- coef(lm_scale)[["(Intercept)"]]

  data_raw <- readxl::read_excel(path_survey)
  data_raw <- apply_impact_type_mapping(data_raw, path_imp_map)

  id_cols <- c("ID", "Experience", "Environment", "Group")
  impact_type_cols <- setdiff(names(data_raw), id_cols)

  data_long <- data_raw %>%
    pivot_longer(cols = all_of(impact_type_cols), names_to = "impact_type", values_to = "impact_score") %>%
    mutate(impact_score = suppressWarnings(as.numeric(impact_score))) %>%
    filter(!is.na(impact_score))

  data_long$obs_id <- seq_len(nrow(data_long))

  data_expanded <- data_long %>%
    mutate(
      Environment = ifelse(grepl("Any environment", Environment, ignore.case = TRUE),
        "Terrestrial,Freshwater,Marine", Environment
      ),
      Group = ifelse(grepl("Any group", Group, ignore.case = TRUE),
        "Plants,Invertebrates,Vertebrates,Fungi,Microorganisms", Group
      )
    ) %>%
    separate_rows(Environment, sep = ",") %>%
    separate_rows(Group, sep = ",") %>%
    mutate(
      Environment = trimws(factor(Environment, levels = c("Freshwater", "Terrestrial", "Marine"))),
      Group = trimws(factor(Group, levels = c("Plants", "Invertebrates", "Vertebrates", "Fungi", "Microorganisms")))
    ) %>%
    group_by(obs_id) %>%
    mutate(weight = 1 / n()) %>%
    ungroup()

  survey_cell_stats <- data_expanded %>%
    filter(!is.na(Environment), !is.na(Group)) %>%
    group_by(impact_type, Environment, Group) %>%
    summarise(n_obs = n(), raw_mean = weighted.mean(impact_score, weight), .groups = "drop") %>%
    mutate(
      T2_scaled = scale_a * raw_mean + scale_b,
      eco_level = factor(eco_level_map[impact_type], levels = eco_level_order)
    )

  can <- make_S_matrices(0.02, 3.50)
  alt1 <- make_S_matrices(0.08, 1.00)
  alt2 <- make_S_matrices(0.045, 2.28)

  combis_raw <- jsonlite::fromJSON(path_combis)
  valid_S_list <- lapply(seq_len(nrow(combis_raw)), function(i) {
    vals_str <- combis_raw$EEICAT_possibility[i]
    vals <- as.numeric(strsplit(gsub("\\[|\\]", "", as.character(vals_str)), ",")[[1]])
    sort(unique(vals[!is.na(vals)]))
  })
  names(valid_S_list) <- combis_raw$imp_types

  cell_T <- emm %>%
    select(impact_type, Environment, Group, eco_level,
      T1 = emmean_1_10,
      T1_SE = SE,
      T1_lcl = LCL_1_10, T1_ucl = UCL_1_10
    ) %>%
    left_join(
      survey_cell_stats %>% select(impact_type, Environment, Group, T2 = T2_scaled, n_obs_T2 = n_obs),
      by = c("impact_type", "Environment", "Group")
    ) %>%
    mutate(S_canonical = sapply(impact_type, function(it) get_max_S(can$S_norm, it)))

  write.csv(cell_T, file.path(derived_dir, "cell_T.csv"), row.names = FALSE)

  t_long <- cell_T %>%
    select(eco_level, T1, T2) %>%
    pivot_longer(cols = c(T1, T2), names_to = "variant", values_to = "T_value") %>%
    filter(!is.na(T_value))

  matched_cells <- cell_T %>% filter(!is.na(T2))
  write.csv(t_long, file.path(derived_dir, "S1_t_long.csv"), row.names = FALSE)
  write.csv(matched_cells, file.path(derived_dir, "S1_matched_cells.csv"), row.names = FALSE)

  pA <- t_long %>%
    mutate(variant = factor(variant, levels = c("T1", "T2"), labels = c("T1 (EMM)", "T2 (raw mean)"))) %>%
    ggplot(aes(x = T_value, colour = eco_level, linetype = variant)) +
    geom_density(linewidth = 0.8, fill = NA) +
    scale_colour_manual(values = eco_colours, name = "Level") +
    scale_linetype_manual(values = c("solid", "dashed"), name = "T variant") +
    coord_cartesian(xlim = c(1, 10)) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), legend.key.width = grid::unit(1.2, "cm")) +
    labs(x = "T (1-10)", y = "Density")

  r_val <- cor(matched_cells$T1, matched_cells$T2, method = "pearson")
  pB <- matched_cells %>%
    ggplot(aes(x = T1, y = T2, colour = eco_level)) +
    geom_rug(alpha = 0.25, linewidth = 0.3) +
    geom_point(alpha = 0.55, size = 1.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    annotate("text", x = 1.4, y = 9.6, label = sprintf("r = %.3f", r_val), hjust = 0, size = 3.5, colour = "grey20") +
    scale_colour_manual(values = eco_colours, name = "Level") +
    coord_equal(xlim = c(1, 10), ylim = c(1, 10)) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank()) +
    labs(x = "T1 (EMM)", y = "T2 (raw mean)")

  p_S1 <- (pA | pB) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(tag_levels = "A") &
    theme(legend.position = "bottom", legend.box = "horizontal")

  save_letter_figure(ctx, p_S1, "Figure_S1.png", width = 12, height = 6)

  lambda_grid <- seq(0.02, 0.08, length.out = 12)
  theta_grid <- seq(1.00, 3.50, length.out = 12)

  param_grid <- expand.grid(lambda = lambda_grid, theta = theta_grid) %>% as_tibble()

  famB_grid <- param_grid %>%
    rowwise() %>%
    mutate(
      Smat = list(make_S_matrices(lambda, theta)$S_norm),
      cell_IP = list({
        cell_T %>% mutate(S_g = sapply(impact_type, function(it) get_max_S(Smat, it)), IP = invapact(S_g, T1))
      }),
      mean_IP = mean(cell_IP$IP),
      cor_can = cor(
        cell_IP$IP,
        invapact(sapply(cell_T$impact_type, function(it) get_max_S(can$S_norm, it)), cell_T$T1),
        method = "spearman"
      )
    ) %>%
    ungroup() %>%
    select(-Smat, -cell_IP)

  write.csv(famB_grid, file.path(derived_dir, "famB_grid.csv"), row.names = FALSE)

  defined_points <- tibble(
    lambda = c(0.02, 0.08, 0.045),
    theta = c(3.50, 1.00, 2.28),
    label = c("canonical", "alt1", "alt2"),
    label_x = lambda + c(0.003, -0.003, 0.003),
    hjust = c(0, 1, 0)
  )

  p_famB_heatmap <- ggplot(famB_grid, aes(x = lambda, y = theta, fill = mean_IP)) +
    geom_tile() +
    geom_point(data = defined_points, aes(x = lambda, y = theta), colour = "white", shape = 21, size = 4, stroke = 1.5, inherit.aes = FALSE) +
    geom_text(data = defined_points, aes(x = label_x, y = theta, label = label, hjust = hjust), colour = "black", size = 3, inherit.aes = FALSE) +
    scale_fill_gradientn(colours = c("#2C7BB6", "#FFFFBF", "#B2182B"), name = "Mean InvaPact") +
    theme_minimal(base_size = 12) +
    labs(title = "Mean IP", x = expression(lambda), y = expression(theta))

  p_famB_cor <- ggplot(famB_grid, aes(x = lambda, y = theta, fill = cor_can)) +
    geom_tile() +
    geom_point(data = defined_points, aes(x = lambda, y = theta), colour = "white", shape = 21, size = 4, stroke = 1.5, inherit.aes = FALSE) +
    geom_text(data = defined_points, aes(x = label_x, y = theta, label = label, hjust = hjust), colour = "black", size = 3, inherit.aes = FALSE) +
    scale_fill_gradientn(
      colours = c("#2C7BB6", "#FFFFBF", "#B2182B"),
      limits = c(min(famB_grid$cor_can, na.rm = TRUE), 1),
      name = "Spearman rho\nvs canonical"
    ) +
    theme_minimal(base_size = 12) +
    labs(title = "Rank rho", x = expression(lambda), y = expression(theta))

  p_S2 <- (p_famB_heatmap | p_famB_cor) +
    patchwork::plot_layout(guides = "keep") +
    patchwork::plot_annotation(tag_levels = "A")

  save_letter_figure(ctx, p_S2, "Figure_S2.png", width = 12, height = 5.5)

  defined_params <- tibble(
    param_name = c("canonical", "alt1", "alt2"),
    lambda = c(0.02, 0.08, 0.045),
    theta = c(3.50, 1.00, 2.28)
  )

  defined_ip <- defined_params %>%
    rowwise() %>%
    mutate(
      Smat = list(make_S_matrices(lambda, theta)$S_norm),
      cells = list({
        cell_T %>% mutate(
          S_val = sapply(impact_type, function(it) get_max_S(Smat, it)),
          IP_T1 = invapact(S_val, T1),
          IP_T2 = invapact(S_val, pmax(T2, 0, na.rm = FALSE))
        )
      })
    ) %>%
    ungroup()

  defined_summary <- defined_ip %>%
    select(param_name, cells) %>%
    unnest(cells) %>%
    group_by(param_name, eco_level) %>%
    summarise(
      mean_IP_T1 = mean(IP_T1, na.rm = TRUE),
      mean_IP_T2 = mean(IP_T2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(param_name = factor(param_name, levels = c("canonical", "alt1", "alt2")))

  write.csv(defined_summary, file.path(derived_dir, "defined_summary.csv"), row.names = FALSE)

  defined_panel_T1 <- defined_summary %>%
    select(param_name, eco_level, mean_IP_T1) %>%
    mutate(eco_level = factor(eco_level, levels = eco_level_order))

  defined_panel_pct <- defined_summary %>%
    mutate(
      pct_dev = (mean_IP_T2 - mean_IP_T1) / mean_IP_T1 * 100,
      eco_level = factor(eco_level, levels = eco_level_order)
    )

  shared_theme <- list(
    scale_colour_brewer(palette = "Set1", name = "Set"),
    theme_minimal(base_size = 12),
    theme(
      axis.text.x = element_text(angle = 20, hjust = 1),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

  pA_11 <- ggplot(defined_panel_T1, aes(x = eco_level, y = mean_IP_T1, colour = param_name, group = param_name)) +
    geom_point(size = 2) +
    geom_line(linewidth = 0.9) +
    scale_y_continuous(limits = c(0, 100)) +
    shared_theme +
    labs(x = "Ecological level", y = "InvaPact (T1)")

  pB_11 <- ggplot(defined_panel_pct, aes(x = eco_level, y = pct_dev, colour = param_name, group = param_name)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey45", linewidth = 0.5) +
    geom_point(size = 2) +
    geom_line(linewidth = 0.9) +
    scale_y_continuous(limits = c(-5, 5)) +
    shared_theme +
    labs(x = "Ecological level", y = "T2 deviation from T1 (%)")

  heat_t1 <- defined_summary %>%
    transmute(param_name, eco_level, value = mean_IP_T1) %>%
    mutate(param_name = factor(param_name, levels = c("canonical", "alt1", "alt2")), eco_level = factor(eco_level, levels = eco_level_order))

  heat_abs_delta <- defined_summary %>%
    transmute(param_name, eco_level, value = abs(mean_IP_T2 - mean_IP_T1)) %>%
    mutate(param_name = factor(param_name, levels = c("canonical", "alt1", "alt2")), eco_level = factor(eco_level, levels = eco_level_order))

  eco_level_abbr <- c(
    "Individual" = "Ind",
    "Population" = "Pop",
    "Species" = "Sp",
    "Assemblage" = "Asm",
    "Ecosystem function/service" = "EFS",
    "Abiotic environment" = "Abi"
  )

  pA_14 <- pA_11 +
    scale_x_discrete(labels = eco_level_abbr) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 10),
      legend.position = "bottom",
      legend.justification = "center",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 7)
    ) +
    labs(x = "Eco level")

  pC_14 <- pB_11 +
    scale_x_discrete(labels = eco_level_abbr) +
    guides(colour = "none") +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 10)
    ) +
    labs(x = "Eco level")

  p12A_panel <- ggplot(heat_t1, aes(x = param_name, y = eco_level, fill = value)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_text(aes(label = round(value, 2)), size = 2.8, colour = "black") +
    scale_y_discrete(labels = eco_level_abbr) +
    scale_fill_gradientn(colours = c("#2C7BB6", "#FFFFBF", "#B2182B"), limits = c(0, 100), name = "InvaPact") +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA)
    ) +
    labs(x = "Set", y = "Eco level")

  p12B_panel <- ggplot(heat_abs_delta, aes(x = param_name, y = eco_level, fill = value)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_text(aes(label = round(value, 2)), size = 2.8, colour = "black") +
    scale_y_discrete(labels = eco_level_abbr) +
    scale_fill_gradientn(colours = c("#2C7BB6", "#FFFFBF", "#B2182B"), limits = c(0, 5), oob = scales::squish, name = "|T2 - T1|") +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA)
    ) +
    labs(x = "Set", y = "Eco level")

  p_S3 <- (pA_14 | p12A_panel) / (pC_14 | p12B_panel) +
    patchwork::plot_layout(widths = c(1.05, 0.95), heights = c(1, 1)) +
    patchwork::plot_annotation(tag_levels = "A")

  save_letter_figure(ctx, p_S3, "Figure_S3.png", width = 173, height = 125, units = "mm")

  sev_rows <- c("mn", "mo", "mr", "mv")
  eco_cols <- c("population", "assemblage", "ecosystem")
  Sc <- can$S_norm

  compute_cell_sigma <- function(Smat) {
    sigma_mat <- matrix(0, nrow = 5, ncol = 3,
      dimnames = list(c("mc", sev_rows), eco_cols))
    for (col in eco_cols) {
      vals <- Smat[sev_rows, col]
      for (i in seq_along(vals)) {
        gaps <- c()
        if (i > 1) gaps <- c(gaps, vals[i] - vals[i - 1])
        if (i < length(vals)) gaps <- c(gaps, vals[i + 1] - vals[i])
        sigma_mat[sev_rows[i], col] <- min(gaps) / 2
      }
    }
    sigma_mat
  }

  sigma_cells <- compute_cell_sigma(Sc)
  N_D1 <- 5000
  set.seed(20260221)

  message("  Sampling ", N_D1, " perturbed S matrices (D1)...")
  S_draws <- array(0, dim = c(length(sev_rows), length(eco_cols), N_D1),
    dimnames = list(sev_rows, eco_cols, NULL))
  for (ci in seq_along(eco_cols)) {
    for (ri in seq_along(sev_rows)) {
      S_draws[ri, ci, ] <- truncnorm::rtruncnorm(N_D1, a = 0, b = 10,
        mean = Sc[sev_rows[ri], eco_cols[ci]],
        sd   = sigma_cells[sev_rows[ri], eco_cols[ci]])
    }
  }

  unique_its <- unique(cell_T$impact_type)
  maxS_D1_per_type <- setNames(lapply(unique_its, function(it) {
    rc <- eeicat_rowcol_map[[it]]
    if (is.null(rc)) return(rep(NA_real_, N_D1))
    nonmc <- rc$rows[rc$rows != "mc"]
    if (length(nonmc) == 0) return(rep(0, N_D1))
    col_i <- which(eco_cols == rc$col)
    row_i <- which(sev_rows %in% nonmc)
    apply(S_draws[row_i, col_i, , drop = FALSE], 3, max)
  }), unique_its)

  D1_results <- cell_T %>%
    rowwise() %>%
    mutate(
      .S_d1  = list(pmax(maxS_D1_per_type[[impact_type]], 0)),
      IP_D1_draws    = list(invapact(.S_d1, T1)),
      IP_D1_T2_draws = list(if (is.na(T2)) rep(NA_real_, N_D1) else
        invapact(.S_d1, pmax(T2, 0))),
      IP_D1_T1_mean = mean(IP_D1_draws),
      IP_D1_T1_sd   = sd(IP_D1_draws),
      IP_D1_T1_q025 = quantile(IP_D1_draws, 0.025),
      IP_D1_T1_q975 = quantile(IP_D1_draws, 0.975),
      IP_D1_T2_mean = if (is.na(T2)) NA_real_ else mean(IP_D1_T2_draws),
      IP_D1_T2_q025 = if (is.na(T2)) NA_real_ else quantile(IP_D1_T2_draws, 0.025),
      IP_D1_T2_q975 = if (is.na(T2)) NA_real_ else quantile(IP_D1_T2_draws, 0.975),
      IP_D1_baseline = invapact(pmax(get_max_S(Sc, impact_type), 0), T1)
    ) %>%
    select(-.S_d1) %>%
    ungroup()

  write.csv(
    D1_results %>% select(-IP_D1_draws, -IP_D1_T2_draws),
    file.path(derived_dir, "D1_results.csv"), row.names = FALSE
  )

  cross_cells <- cell_T %>%
    mutate(
      S_can  = sapply(impact_type, function(it) get_max_S(can$S_norm,  it)),
      S_alt1 = sapply(impact_type, function(it) get_max_S(alt1$S_norm, it)),
      S_alt2 = sapply(impact_type, function(it) get_max_S(alt2$S_norm, it))
    ) %>%
    left_join(
      D1_results %>% select(impact_type, Environment, Group,
        S_D1_T1_mean = IP_D1_T1_mean, S_D1_T2_mean = IP_D1_T2_mean),
      by = c("impact_type", "Environment", "Group")
    ) %>%
    mutate(
      IP_T1_can  = invapact(S_can,  T1),
      IP_T1_alt1 = invapact(S_alt1, T1),
      IP_T1_alt2 = invapact(S_alt2, T1),
      IP_T1_D1   = S_D1_T1_mean,
      IP_T2_can  = invapact(S_can,  pmax(T2, 0, na.rm = FALSE)),
      IP_T2_alt1 = invapact(S_alt1, pmax(T2, 0, na.rm = FALSE)),
      IP_T2_alt2 = invapact(S_alt2, pmax(T2, 0, na.rm = FALSE)),
      IP_T2_D1   = S_D1_T2_mean
    )

  cross_long <- cross_cells %>%
    select(impact_type, eco_level, starts_with("IP_T")) %>%
    pivot_longer(cols = starts_with("IP_T"), names_to = "scenario", values_to = "InvaPact") %>%
    filter(!is.na(InvaPact)) %>%
    mutate(
      T_var  = ifelse(grepl("^IP_T1", scenario), "T1 \u2014 EMM", "T2 \u2014 Raw mean"),
      S_scen = case_when(
        grepl("_can$",  scenario) ~ "canonical",
        grepl("_alt1$", scenario) ~ "alt1",
        grepl("_alt2$", scenario) ~ "alt2",
        grepl("_D1$",   scenario) ~ "D1 (cell noise mean)",
        TRUE ~ scenario
      ),
      S_scen    = factor(S_scen, levels = c("canonical", "alt1", "alt2", "D1 (cell noise mean)")),
      eco_level = factor(eco_level, levels = eco_level_order)
    ) %>%
    group_by(eco_level, T_var, S_scen) %>%
    summarise(mean_IP = mean(InvaPact, na.rm = TRUE), .groups = "drop")

  cross_heat <- cross_cells %>%
    select(impact_type, eco_level, starts_with("IP_T")) %>%
    pivot_longer(cols = starts_with("IP_T"), names_to = "scenario", values_to = "InvaPact") %>%
    filter(!is.na(InvaPact)) %>%
    mutate(
      T_var  = ifelse(grepl("^IP_T1", scenario), "T1 \u2014 EMM", "T2 \u2014 Raw mean"),
      S_scen = case_when(
        grepl("_can$",  scenario) ~ "canonical",
        grepl("_alt1$", scenario) ~ "alt1",
        grepl("_alt2$", scenario) ~ "alt2",
        grepl("_D1$",   scenario) ~ "D1",
        TRUE ~ scenario
      )
    ) %>%
    group_by(T_var, S_scen, eco_level) %>%
    summarise(mean_IP = mean(InvaPact, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      eco_level = factor(eco_level, levels = eco_level_order),
      S_scen    = factor(S_scen, levels = c("canonical", "alt1", "alt2", "D1"))
    )

  write.csv(cross_long, file.path(derived_dir, "cross_long.csv"), row.names = FALSE)
  write.csv(cross_heat, file.path(derived_dir, "cross_heat.csv"), row.names = FALSE)

  all_S_vals <- sort(unique(unlist(valid_S_list)))
  all_T_vals <- sort(unique(round(emm$emmean_1_10, 4)))

  n_T_bins <- 10
  T_breaks <- seq(floor(min(all_T_vals) * 10) / 10, ceiling(max(all_T_vals) * 10) / 10, length.out = n_T_bins + 1)
  T_bin_mids <- round((T_breaks[-length(T_breaks)] + T_breaks[-1]) / 2, 2)

  ip_grid_binned <- expand.grid(S = all_S_vals, T = all_T_vals) %>%
    as_tibble() %>%
    mutate(
      IP = invapact(S, pmax(T, 0)),
      T_bin = cut(
        T,
        breaks = T_breaks,
        labels = paste0("[", round(T_breaks[-length(T_breaks)], 2), "-", round(T_breaks[-1], 2), ")"),
        include.lowest = TRUE,
        right = FALSE
      ),
      T_mid = T_bin_mids[as.integer(T_bin)]
    ) %>%
    group_by(S, T_bin, T_mid) %>%
    summarise(IP_mean = mean(IP), .groups = "drop")

  write.csv(ip_grid_binned, file.path(derived_dir, "ip_grid_binned.csv"), row.names = FALSE)

  p_S5 <- ggplot(ip_grid_binned, aes(x = factor(round(S, 2)), y = T_bin, fill = IP_mean)) +
    geom_tile(colour = "white", linewidth = 0.25) +
    geom_text(aes(label = round(IP_mean, 1)), size = 3.8, colour = "black") +
    scale_fill_gradientn(
      colours = c("#2C7BB6", "#FFFFBF", "#B2182B"),
      limits = c(0, 100),
      oob = scales::squish,
      name = NULL
    ) +
    scale_y_discrete(
      name = expression(paste("T (EMM scaled, binned into ", textstyle(10), " intervals)")),
      limits = rev(levels(ip_grid_binned$T_bin))
    ) +
    scale_x_discrete(name = "S (canonical valid values)") +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
      axis.text.y = element_text(size = 13),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      plot.title = element_blank(),
      plot.subtitle = element_text(size = 13),
      legend.title = element_blank(),
      legend.text = element_text(size = 13),
      legend.position = "right"
    ) +
    labs(subtitle = paste0("T binned into ", n_T_bins, " equal-width intervals; cells show mean InvaPact within each bin"))

  save_letter_figure(ctx, p_S5, "Figure_S5.png", width = 12, height = 6)

  list(
    D1_results = D1_results,
    cross_long = cross_long,
    cross_heat = cross_heat
  )
}
