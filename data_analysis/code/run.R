source("data_analysis/code/00_helpers.R")
source("data_analysis/code/fig_panel.R")
source("data_analysis/code/fig_priority.R")
source("data_analysis/code/prepare_mechanisms.R")

ctx <- new_context()
ensure_dirs(ctx)

input_csv <- file.path(ctx$data_dir, "papers_dataset.csv")
input_mechanisms_csv <- file.path(ctx$data_dir, "papers_dataset_mechanisms.csv")

build_mechanism_dataset(
  input_path = input_csv,
  output_path = input_mechanisms_csv
)

generate_figure_panel(
  input_path = input_csv,
  mechanism_path = input_mechanisms_csv,
  output_path = file.path(ctx$fig_dir, "Figure_panel.png")
)

generate_figure_priority(
  input_path = input_csv,
  output_path = file.path(ctx$fig_dir, "Figure_priority.png"),
  table_path = file.path(ctx$table_dir, "priority_matrix_species_table.csv")
)

write_session_info(ctx)
