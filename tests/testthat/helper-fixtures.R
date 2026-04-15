fixture_country_table <- function() {
  tibble::tibble(
    country_code = c(10L, 20L),
    country_name = c("Country A", "Country B"),
    country_name_pt = c("Pais A", "Pais B"),
    country_name_es = c("Pais A ES", "Pais B ES")
  )
}

fixture_ncm_table <- function() {
  tibble::tibble(
    ncm = c("00000001", "00000002"),
    unit_code = c(10L, 11L),
    isic_class_code = c("0111", "0510"),
    bec_n3_code = c("111", "210"),
    cuci_basic_heading_code = c("0011", "3341"),
    ncm_description = c("Live horses", "Coal"),
    ncm_description_pt = c("Cavalos vivos", "Carvao"),
    ncm_description_es = c("Caballos vivos", "Carbon")
  )
}

fixture_isic_table <- function() {
  tibble::tibble(
    isic_class_code = c("0111", "0510"),
    isic_class_desc = c("Growing of cereals", "Mining of coal"),
    isic_class_desc_pt = c("Cultivo de cereais", "Extracao de carvao"),
    isic_class_desc_es = c("Cultivo de cereales", "Extraccion de carbon"),
    isic_group_code = c("011", "051"),
    isic_group_desc = c("Growing of non-perennial crops", "Mining of coal"),
    isic_group_desc_pt = c("Cultivo temporario", "Mineracao de carvao"),
    isic_group_desc_es = c("Cultivo temporal", "Mineria de carbon"),
    isic_division_code = c("01", "05"),
    isic_division_desc = c("Crop production", "Mining of coal and lignite"),
    isic_division_desc_pt = c("Producao vegetal", "Extracao de carvao e linhito"),
    isic_division_desc_es = c("Produccion vegetal", "Extraccion de carbon y lignito"),
    isic_section_code = c("A", "B"),
    isic_section_desc = c("Agriculture", "Mining and quarrying"),
    isic_section_desc_pt = c("Agricultura", "Industrias extrativas"),
    isic_section_desc_es = c("Agricultura", "Minas y canteras")
  )
}

fixture_bec_table <- function() {
  tibble::tibble(
    bec_n3_code = c("111", "210"),
    bec_n3_desc = c("Primary food", "Processed supplies"),
    bec_n3_desc_pt = c("Alimento primario", "Insumo processado"),
    bec_n3_desc_es = c("Alimento primario", "Insumo procesado"),
    bec_n2_code = c("11", "21"),
    bec_n2_desc = c("Food", "Industrial supplies"),
    bec_n2_desc_pt = c("Alimentos", "Insumos industriais"),
    bec_n2_desc_es = c("Alimentos", "Insumos industriales"),
    bec_n1_code = c("1", "2"),
    bec_n1_desc = c("Consumption", "Intermediate"),
    bec_n1_desc_pt = c("Consumo", "Intermediario"),
    bec_n1_desc_es = c("Consumo", "Intermedio")
  )
}

fixture_cuci_table <- function() {
  tibble::tibble(
    cuci_basic_heading_code = c("0011", "3341"),
    cuci_basic_heading_desc_pt = c("Animais vivos", "Combustiveis"),
    cuci_subgroup_code = c("001", "334"),
    cuci_subgroup_desc_pt = c("Animais vivos", "Derivados de petroleo"),
    cuci_group_code = c("00", "33"),
    cuci_group_desc_pt = c("Animais vivos", "Petroleo e derivados"),
    cuci_division_code = c("0", "3"),
    cuci_division_desc_pt = c("Alimentos e animais vivos", "Combustiveis minerais"),
    cuci_section_code = c("0", "3"),
    cuci_section_desc_pt = c("Produtos alimentares", "Combustiveis")
  )
}

fixture_state_table <- function() {
  tibble::tibble(
    state_code = c(35L, 33L),
    state = c("SP", "RJ"),
    state_name = c("Sao Paulo", "Rio de Janeiro"),
    region_name = c("SUDESTE", "SUDESTE")
  )
}

fixture_unit_table <- function() {
  tibble::tibble(
    unit_code = c(10L, 11L),
    unit_description_pt = c("Quilograma liquido", "Numero"),
    unit_description = c("Net kilogram", "Number")
  )
}

write_download_page <- function(links) {
  path <- tempfile(fileext = ".html")
  rows <- paste0(
    '<tr><td><a href="',
    links,
    '">',
    basename(links),
    "</a></td></tr>",
    collapse = ""
  )

  writeLines(
    c("<html><body><table>", rows, "</table></body></html>"),
    path,
    useBytes = TRUE
  )

  path
}

write_trade_csv <- function(path, data) {
  utils::write.table(
    data,
    file = path,
    sep = ";",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE
  )
}

write_latin1_csv <- function(path, lines) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)

  encoded_lines <- iconv(lines, from = "UTF-8", to = "Latin1", toRaw = TRUE)
  for (line in encoded_lines) {
    writeBin(line, con)
    writeBin(as.raw(10), con)
  }
}

write_year_month_dataset <- function(path, data) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  arrow::write_dataset(tibble::as_tibble(data), path, partitioning = "year")
}
