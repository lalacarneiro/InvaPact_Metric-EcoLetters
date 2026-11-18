suppressPackageStartupMessages({
  library(dplyr)
})

split_mechanisms_weighted <- function(x) {
  if (is.na(x)) return(character())

  parts <- unlist(strsplit(tolower(x), ";", fixed = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  if (length(parts) == 0) return(character())

  parts <- gsub("\\s+", " ", parts)
  parts <- gsub("^na$", "unknown", parts)
  parts <- gsub("^unkonwn$", "unknown", parts)
  parts <- gsub("^phisical$", "physical", parts)
  parts <- gsub("^physial$", "physical", parts)
  parts <- gsub("^phyiscal$", "physical", parts)
  parts <- gsub("^indirec$", "indirect", parts)
  parts <- gsub("^structural$", "structure", parts)
  parts <- gsub("^herbivory$", "grazing/herbivory/browsing", parts)

  expanded <- unlist(lapply(parts, function(tok) {
    if (tok == "chemical/phyiscal/structural") return(c("chemical", "physical", "structure"))
    if (tok == "predation/herbivory/indirect") return(c("predation", "grazing/herbivory/browsing", "indirect"))
    if (grepl(",", tok, fixed = TRUE)) {
      out <- trimws(unlist(strsplit(tok, ",", fixed = TRUE), use.names = FALSE))
      return(out[out != ""])
    }
    tok
  }), use.names = FALSE)

  unique(expanded[expanded != ""])
}

normalize_tax_mechanism <- function(x, species_name = NULL) {
  algae_excluded <- c(
    "caulerpa racemosa", "caulerpa taxifolia", "caulerpa cylindracea", "undaria pinnatifida",
    "sargassum muticum", "codium fragile", "womersleyella setacea"
  )

  x0 <- tolower(trimws(ifelse(is.na(x), "unknown", x)))
  sp0 <- tolower(trimws(ifelse(is.na(species_name), "", species_name)))

  dplyr::case_when(
    x0 %in% c("invertebrates", "invertebrate") ~ "Invertebrates",
    x0 %in% c("plants", "plant") & sp0 %in% algae_excluded ~ "Other",
    x0 %in% c("plants", "plant") ~ "Plants",
    x0 %in% c("vertebrates", "vertebrate", "fish", "mammals", "mammal", "birds", "bird", "reptiles", "reptile", "amphibians", "amphibian") ~ "Vertebrates",
    TRUE ~ "Other"
  )
}

build_mechanism_dataset <- function(input_path, output_path) {
  df <- read.csv(input_path, stringsAsFactors = FALSE)

  required_cols <- c("invasive_species_name", "invapact_score", "tax_group_plot", "mechanisms")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns in papers_dataset.csv: %s", paste(missing_cols, collapse = ", ")))
  }

  df_mech <- df %>%
    mutate(
      tax_group_mech = normalize_tax_mechanism(tax_group_plot, invasive_species_name)
    ) %>%
    filter(
      !is.na(invapact_score),
      tax_group_mech %in% c("Invertebrates", "Plants", "Vertebrates"),
      !is.na(mechanisms)
    )

  tokens <- lapply(df_mech$mechanisms, split_mechanisms_weighted)
  counts_per_row <- lengths(tokens)
  keep <- counts_per_row > 0

  df_mech <- df_mech[keep, , drop = FALSE]
  tokens <- tokens[keep]
  counts_per_row <- counts_per_row[keep]

  if (nrow(df_mech) == 0) {
    stop("No rows with parseable mechanisms in papers_dataset.csv")
  }

  expanded <- df_mech[rep(seq_len(nrow(df_mech)), counts_per_row),
    c("invasive_species_name", "tax_group_mech", "invapact_score"), drop = FALSE]

  expanded$mechanism_clean <- unlist(tokens, use.names = FALSE)
  expanded$mechanism_weight <- rep(1 / counts_per_row, counts_per_row)

  write.csv(expanded, output_path, row.names = FALSE)
}
