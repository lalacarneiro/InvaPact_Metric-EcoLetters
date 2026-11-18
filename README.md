# InvaPact_Metric

Code and data for the InvaPact metric paper.

## Folders

- `metric_scoring/` scores the impact records and runs the analyses of the two score components (severity S and importance T). It also makes the S–T figures.
- `data_analysis/` makes the main figures (Figure_panel and Figure_priority).
- `sensitivity_eco_letters/` makes the supplementary figures S1–S5.

Each folder has its own `code/`, `data/` and `output/`.

## How to run

Open R in this folder and run:

```r
source("metric_scoring/code/00_install_packages.R")
source("data_analysis/code/00_install_packages.R")
source("sensitivity_eco_letters/code/00_install_packages.R")

source("run_all.R")
```

`metric_scoring` has to run before `data_analysis`, because it creates the dataset used by the main figures. `run_all.R` already runs them in that order.

## Citation
- Carneiro, L., Bradshaw, C., Pincheira-Donoso, D., Caetano, G. H. d. O., Cuthbert, R., Tieo, S., Wang, S., Liu, C., Arbieu, U., Pili, A., Desiderato, A., Cooke, S., Tonkin, J., South, J., Firth, J., Dallas, T., & Courchamp, F. (2026). A standardized metric for quantifying the ecological impacts of biological invasions. Ecology Letters.
- Code and data: https://doi.org/10.6084/m9.figshare.32141293
- Carneiro, L., Bradshaw, C., Pincheira-Donoso, D., Caetano, G. H. d. O., Cuthbert, R., Tieo, S., Wang, S., Liu, C., Arbieu, U., Pili, A., Desiderato, A., Cooke, S., Tonkin, J., South, J., Firth, J., Dallas, T., & Courchamp, F. (2026). InvaPact: a metric to quantify the ecological impacts of biological invasions. ARPHA Conference Abstracts, 9. https://doi.org/10.3897/aca.9.e202558

## License

MIT, see `LICENSE`.
