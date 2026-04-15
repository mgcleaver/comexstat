# Build internal data

devtools::load_all()

brazilian_states <- c(
  "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
  "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO",
  "RR", "RS", "SC", "SE", "SP", "TO"
)

# Get Comex Stat's state table
state_table <- process_table("UF") |>
  tibble::as_tibble()

# Get Comex Stat's unit table and build english names
unit_table <- process_table("NCM_UNIDADE") |>
  dplyr::mutate(
    unit_name_pt = stringr::str_to_sentence(no_unid),
    sg_unid = stringr::str_squish(sg_unid),
    unit_name = dplyr::case_when(
      unit_name_pt == "Quilograma liquido" ~ "Net kilogram",
      unit_name_pt == "Numero (unidade)" ~ "Number (unit)",
      unit_name_pt == "Milheiro" ~ "Thousand units",
      unit_name_pt == "Pares" ~ "Pairs",
      unit_name_pt == "Metro" ~ "Meter",
      unit_name_pt == "Metro quadrado" ~ "Square meter",
      unit_name_pt == "Metro cubico" ~ "Cubic meter",
      unit_name_pt == "Litro" ~ "Liter",
      unit_name_pt == "Mil quilowatt hora" ~ "Thousand kilowatt hour",
      unit_name_pt == "Quilate" ~ "Carat",
      unit_name_pt == "Duzia" ~ "Dozen",
      unit_name_pt == "Tonelada metrica liquida" ~ "Net metric ton",
      unit_name_pt == "Grama liquido" ~ "Net gram",
      unit_name_pt == "Bilhoes de unidades internacionais" ~ "Billion international units",
      unit_name_pt == "Quilograma bruto" ~ "Gross kilogram",
      TRUE ~ NA_character_)
  ) |>
  tibble::as_tibble()

# Add to internal data
usethis::use_data(
  unit_table,
  state_table,
  brazilian_states,
  internal = TRUE,
  overwrite = TRUE)
