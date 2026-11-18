# sensitivity_eco_letters

Makes the supplementary figures S1–S5, which test how sensitive the score is to the way S and T are built.

## Run

From the repository root:

```r
source("sensitivity_eco_letters/code/03_run_all.R")
```

Figures go to `output/figures/`. Intermediate tables go to `data/derived/` and are rewritten on every run. The inputs are in `data/raw/`, described in `data/raw/README.md`.

Packages: readxl, dplyr, tidyr, stringr, ggplot2, jsonlite, tibble, scales, patchwork, truncnorm.
