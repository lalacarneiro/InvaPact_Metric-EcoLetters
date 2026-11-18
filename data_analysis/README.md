# data_analysis

Makes the two main figures of the paper:

- `output/figures/Figure_panel.png`: scores by taxonomic group, mechanism, ecosystem type and year
- `output/figures/Figure_priority.png`: the species priority matrix, with its table in `output/tables/priority_matrix_species_table.csv`

## Run

From the repository root:

```r
source("data_analysis/code/run.R")
```

The input, `data/papers_dataset.csv`, is created by `metric_scoring/code/score_records.R`. `data/papers_dataset_mechanisms.csv` is rebuilt from it every time the code runs.

Packages: dplyr, ggplot2, tidyr, scales, Hmisc.
