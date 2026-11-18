suppressPackageStartupMessages(library(dplyr))

scoring_S <- read.csv("metric_scoring/data/scoring_S.csv")
scoring_T <- read.csv("metric_scoring/data/scoring_T.csv")
records   <- read.csv("metric_scoring/data/impact_records.csv")

omega <- inner_join(scoring_S, scoring_T, by = c("impact_type", "eco_level"),
                    relationship = "many-to-many")
I_MAX <- max(10 * sqrt(omega$S_norm * omega$T))

scored <- records |>
  inner_join(select(scoring_S, impact_type, eeicat_code, S = S_norm),
             by = c("impact_type", "eeicat_code")) |>
  inner_join(select(scoring_T, impact_type, environment = Environment,
                    tax_group_plot = Group, T),
             by = c("impact_type", "environment", "tax_group_plot")) |>
  mutate(I_raw = 10 * sqrt(S * T),
         invapact_score = round(100 * I_raw / I_MAX, 4))

stopifnot(nrow(scored) == nrow(records))

write.csv(scored, "metric_scoring/output/tables/impact_records_scored.csv", row.names = FALSE)

papers <- select(scored, invasive_species_name, invapact_score, start_sampling_year,
                 impact_type, eco_level, tax_group_plot, mechanisms, ecosystem,
                 sampling_continent, sampling_country)
write.csv(papers, "data_analysis/data/papers_dataset.csv", row.names = FALSE)
