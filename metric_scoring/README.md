# metric_scoring

Scores the impact records and looks at how severity (S) and importance (T) contribute to the score.

## How a record is scored

```
I_raw = 10 * sqrt(S * T)
score = 100 * I_raw / I_max
```

S comes from `scoring_S.csv` (impact type × EEICAT class), and T comes from `scoring_T.csv` (impact type × environment × taxonomic group). `I_max` = 97.9623 is the highest `I_raw` among the 1,170 possible combinations. If an impact type × EEICAT class pair is not in `scoring_S.csv`, the record can't be scored.

## Scripts

- `score_records.R` scores the records. It writes `output/tables/impact_records_scored.csv` and `data_analysis/data/papers_dataset.csv`.
- `variance_analysis.R` does the variance decomposition, the score ceilings by impact type, the rank reversals and the comparison with T held constant.
- `figure_components.R` makes the two S–T figures: `Figure_heatmap_mean_real` and `Figure_curves_attainable`.

Run everything with `source("metric_scoring/code/run.R")` from the repository root.

Packages: dplyr, tidyr, ggplot2, scales.

## Data

`scoring_S.csv`

| Column | Description |
|---|---|
| `impact_type` | impact type |
| `eeicat_raw`, `eeicat_code` | EEICAT magnitude class (full name and code: MC, MN, MO, MR, MV) |
| `eco_level` | ecological level |
| `S_norm` | severity, 0–10 |
| `lambda`, `theta` | parameters used to build S (0.02 and 3.5) |

`scoring_T.csv`

| Column | Description |
|---|---|
| `impact_type` | impact type |
| `Environment` | Terrestrial, Freshwater or Marine |
| `Group` | taxonomic group |
| `eco_level` | ecological level |
| `T`, `T_lcl`, `T_ucl` | importance (1–10) from the expert survey, with its 95% confidence limits |

`impact_records.csv` (1,018 records)

| Column | Description |
|---|---|
| `doi` | source publication |
| `invasive_species_name` | introduced species |
| `start_sampling_year` | first year of sampling |
| `ecosystem` | ecosystem type(s), separated by `;` |
| `sampling_country`, `sampling_continent` | where the impact was recorded |
| `mechanisms` | impact mechanism(s), separated by `;` |
| `impact_type`, `eco_level` | impact type and its ecological level |
| `eeicat_code` | EEICAT magnitude class |
| `environment`, `tax_group_plot` | environment and taxonomic group used to look up T |

The scored file adds `S`, `T`, `I_raw` and `invapact_score`.
