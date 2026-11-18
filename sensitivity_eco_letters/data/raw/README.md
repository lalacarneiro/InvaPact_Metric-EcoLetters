# Raw data

- `surveydata19082024.xlsx`: expert survey answers. One row per respondent, with their experience, environment and taxonomic group, and a score for each impact type. Blank cells are impact types the respondent did not score.
- `emmeansscaled_T.csv`: importance T for each impact type × environment × taxonomic group, estimated from the survey and rescaled to 1–10 (`emmean_1_10`, with limits `LCL_1_10` and `UCL_1_10`).
- `impact_type_map.json`: links the survey column names to the impact type names used in the code.
- `imp_to_eeicatcombi.json`: the EEICAT classes allowed for each impact type.
- `08_robustness_*.csv`: robustness results from an earlier run, kept for reference. The code does not read them.
