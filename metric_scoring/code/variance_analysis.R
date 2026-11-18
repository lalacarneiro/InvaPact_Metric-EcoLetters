suppressPackageStartupMessages(library(dplyr))

scoring_S <- read.csv("metric_scoring/data/scoring_S.csv")
scoring_T <- read.csv("metric_scoring/data/scoring_T.csv")
studies   <- read.csv("metric_scoring/output/tables/impact_records_scored.csv")
out_dir   <- "metric_scoring/output/tables"

omega <- inner_join(scoring_S, scoring_T, by = c("impact_type", "eco_level"),
                    relationship = "many-to-many")
omega$I_raw <- 10 * sqrt(omega$S_norm * omega$T)
I_MAX <- max(omega$I_raw)
omega$I <- 100 * omega$I_raw / I_MAX

cat("Combinations:", nrow(omega), " I_max:", round(I_MAX, 4), "\n")


observed <- select(studies, I = invapact_score, S, T)

decompose <- function(d, label) {
  m    <- summary(lm(I ~ S + T, data = d))
  r2_S <- summary(lm(I ~ S, data = d))$r.squared
  r2_T <- summary(lm(I ~ T, data = d))$r.squared
  data.frame(population = label,
             n          = nrow(d),
             R2_S_alone = r2_S,
             R2_T_alone = r2_T,
             R2_both    = m$r.squared,
             unique_S   = m$r.squared - r2_T,
             unique_T   = m$r.squared - r2_S,
             slope_S    = m$coefficients["S", "Estimate"],
             slope_T    = m$coefficients["T", "Estimate"])
}

omega_ST <- select(omega, I, S = S_norm, T)

variance_table <- rbind(
  decompose(omega_ST, "Omega"),
  decompose(filter(omega_ST, S > 0), "Omega, excluding S = 0"),
  decompose(observed, "Study records"),
  decompose(filter(observed, S > 0), "Study records, excluding S = 0"))

print(variance_table, digits = 3)

log_fit <- lm(log(I) ~ log(S_norm) + log(T), data = filter(omega, S_norm > 0))
print(coef(log_fit))


ceilings <- omega |>
  group_by(impact_type) |>
  summarise(S_ceiling = max(S_norm),
            T_max     = max(T),
            I_ceiling = max(I),
            .groups = "drop") |>
  arrange(desc(I_ceiling))

print(as.data.frame(ceilings), digits = 3)


cells <- select(omega, impact_type, eeicat_code, Environment, Group, S = S_norm, T, I)

reversals <- cross_join(cells, rename_with(cells, ~ paste0(.x, "_b"))) |>
  filter(S < S_b, I > I_b)

n_comparable <- sum(outer(cells$S, cells$S, "<"))
cat("Rank reversals:", nrow(reversals), "of", n_comparable, "pairs",
    sprintf("(%.1f%%)", 100 * nrow(reversals) / n_comparable), "\n")

reversals |>
  group_by(impact_type, eeicat_code, S, impact_type_b, eeicat_code_b, S_b) |>
  summarise(max_gap = max(I - I_b), .groups = "drop") |>
  arrange(desc(max_gap)) |>
  head(5) |>
  as.data.frame() |>
  print(digits = 3)


T_BAR <- mean(scoring_T$T)

observed$I_tneutral <- 100 * 10 * sqrt(observed$S * T_BAR) / I_MAX
diff_tn <- observed$I - observed$I_tneutral

cat("T fixed at", round(T_BAR, 4),
    "| Spearman", round(cor(observed$I, observed$I_tneutral, method = "spearman"), 4),
    "| Kendall", round(cor(observed$I, observed$I_tneutral, method = "kendall"), 4),
    "| mean |change|", round(mean(abs(diff_tn)), 2),
    "| max |change|", round(max(abs(diff_tn)), 2), "\n")

groups <- studies |>
  mutate(score_tneutral = 100 * 10 * sqrt(S * T_BAR) / I_MAX) |>
  group_by(eco_level) |>
  summarise(n = n(),
            mean_standard = mean(invapact_score),
            mean_tneutral = mean(score_tneutral),
            .groups = "drop") |>
  mutate(rank_standard = rank(-mean_standard),
         rank_tneutral = rank(-mean_tneutral)) |>
  arrange(rank_standard)

print(as.data.frame(groups), digits = 4)


write.csv(variance_table, file.path(out_dir, "variance_decomposition.csv"), row.names = FALSE)
write.csv(ceilings, file.path(out_dir, "score_ceilings_by_type.csv"), row.names = FALSE)
write.csv(groups, file.path(out_dir, "importance_neutral_groups.csv"), row.names = FALSE)
