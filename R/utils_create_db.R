#' Create Arrow schemas for Comex Stat datasets
#'
#' Internal helper that returns the Arrow schema used when writing processed
#' import or export datasets.
#'
#' @param category Character string. Either `"export"` or `"import"`.
#'
#' @return An `arrow::schema` object with the fields expected for the selected
#'   dataset category.
#'
#' @keywords internal
#' @noRd
create_schema <- function(category = c("export", "import")) {
  category <- match.arg(category)

  if(category == "export") {
    # Schema for exports
    return(arrow::schema(
      year = arrow::int32(),
      month = arrow::int32(),
      ncm = arrow::utf8(),
      state = arrow::utf8(),
      country_code = arrow::int32(),
      fob_value = arrow::int64(),
      kg = arrow::int64(),
      qty = arrow::int64()
    ))
  }

  if(category == "import") {
    # Schema for imports
    return(arrow::schema(
      year = arrow::int32(),
      month = arrow::int32(),
      ncm = arrow::utf8(),
      state = arrow::utf8(),
      country_code = arrow::int32(),
      fob_value = arrow::int64(),
      cif_value = arrow::int64(), # exportações não tem esse dado
      kg = arrow::int64(),
      qty = arrow::int64()
    ))
  }
}

#' Compare a local dataset with the latest official update
#'
#' Compares the most recent year-month available in a local Arrow dataset with
#' the latest year-month reported by the Comex Stat update API.
#'
#' @param file_dir Character string. Path to the local Arrow dataset directory.
#'
#' @return Logical value. `TRUE` if the local dataset is up to date, `FALSE` otherwise.
#'
#' @keywords internal
#' @noRd
compare_local_db <- function(file_dir) {
  temp_date_api <- get_last_update()
  temp_date_local <- most_recent_date(file_dir = file_dir)

  if (temp_date_api == temp_date_local) {
    return(TRUE)
  }
  return(FALSE)
}

#' Get the latest year-month reported by the Comex Stat API
#'
#' Queries the Comex Stat update endpoint and returns the latest available
#' year-month as a zero-padded string.
#'
#' @return A character string in the format `"yyyy-mm"`.
#'
#' @keywords internal
#' @noRd
get_last_update <- function() {
  url <- "https://api-comexstat.mdic.gov.br/general/dates/updated"

  response <- httr::GET(url)
  json_data <- suppressMessages(httr::content(response, as = "text"))

  last_update <- jsonlite::fromJSON(json_data, flatten = TRUE) |>
    purrr::pluck("data")

  paste0(last_update$year, "-", stringr::str_pad(last_update$monthNumber, 2, "left", "0"))
}

#' Get the most recent year-month from a local dataset
#'
#' Reads a local Arrow dataset and returns the maximum available year-month.
#'
#' @param file_dir Character string. Path to the local Arrow dataset directory.
#'
#' @return A character string in the format `"yyyy-mm"` representing the most recent local date.
#'
#' @keywords internal
#' @noRd
most_recent_date <- function(file_dir) {
  arrow::open_dataset(file_dir) |>
    dplyr::select(year, month) |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::filter(year == max(year)) |>
    dplyr::filter(month == max(month)) |>
    dplyr::mutate(month = stringr::str_pad(month, 2, side = "left", pad = "0")) |>
    dplyr::mutate(result = paste0(year, "-", month)) |>
    dplyr::pull(result)
}

#' Download a Comex Stat file with retry
#'
#' Downloads a raw Comex Stat CSV to disk, retrying up to three times with a
#' short pause between failed attempts.
#'
#' @param link_download Character string. URL for the CSV file.
#' @param dir_file_download Character string. Local path to save the downloaded file.
#'
#' @return No return value. Writes the file to `dir_file_download` or throws an
#'   error after three failed attempts.
#'
#' @keywords internal
#' @noRd
download_cs_file <- function(link_download, dir_file_download) {
  year_from_link <- stringr::str_extract(link_download, "[0-9]{4}")
  sleep <- 5
  download_success <- FALSE

  for (attempt in 1:3) {
    tryCatch({
      httr::GET(
        link_download,
        httr::write_disk(dir_file_download, overwrite = TRUE),
        httr::progress()
      )
      download_success <- TRUE
      break
    },
    error = function(e) {
      message(glue::glue("Attempt {attempt} failed: {e$message}"))
      if (attempt < 3) Sys.sleep(sleep)
    })
  }
  if (!download_success) {
    stop(glue::glue("Failed to download file for year {year_from_link} after 3 attempts.\n"))
  }
}

#' Read and process raw import data
#'
#' Reads a raw Comex Stat import CSV, standardizes column names, aggregates
#' values by year-month-product-state-country, and computes `cif_value`.
#'
#' @param path Character string. File path to the downloaded import csv file.
#'
#' @return A `tibble` with one row per `year`, `month`, `ncm`, `state`, and
#'   `country_code`.
#'
#' @keywords internal
#' @noRd
read_imports <- function(path) {
  data.table::fread(
    path,
    encoding = "Latin-1",
    select = c("CO_ANO", "CO_MES", "CO_NCM", "SG_UF_NCM", "CO_PAIS",
               "KG_LIQUIDO", "QT_ESTAT", "VL_FOB", "VL_FRETE", "VL_SEGURO")
  ) |>
    janitor::clean_names() |>
    dplyr::rename(
      year = co_ano, month = co_mes, ncm = co_ncm, state = sg_uf_ncm,
      country_code = co_pais, kg = kg_liquido, qty = qt_estat, fob_value = vl_fob
    ) |>
    dplyr::mutate(dplyr::across(c(fob_value, kg, qty, vl_frete, vl_seguro), as.numeric)) |>
    dplyr::group_by(year, month, ncm, state, country_code) |>
    dplyr::summarise(dplyr::across(c(fob_value, vl_seguro, vl_frete, kg, qty), sum), .groups = "drop") |>
    dplyr::mutate(
      cif_value = fob_value + vl_seguro + vl_frete,
      ncm = stringr::str_pad(ncm, 8, "left", "0")
    ) |>
    dplyr::select(-vl_seguro, -vl_frete) |>
    dplyr::relocate(cif_value, .after = fob_value) |>
    dplyr::arrange(month)
}

#' Read and process raw export data
#'
#' Reads a raw Comex Stat export CSV, standardizes column names, and aggregates
#' values by year-month-product-state-country.
#'
#' @param path Character string. File path to the export CSV file.
#'
#' @return A `tibble` with one row per `year`, `month`, `ncm`, `state`, and
#'   `country_code`.
#'
#' @keywords internal
#' @noRd
read_exports <- function(path) {
  data.table::fread(
    path,
    encoding = "Latin-1",
    select = c("CO_ANO", "CO_MES", "CO_NCM", "SG_UF_NCM",
               "CO_PAIS", "KG_LIQUIDO", "QT_ESTAT", "VL_FOB")
  ) |>
    janitor::clean_names() |>
    dplyr::rename(
      year = co_ano, month = co_mes, ncm = co_ncm, state = sg_uf_ncm,
      country_code = co_pais, kg = kg_liquido, qty = qt_estat, fob_value = vl_fob
    ) |>
    dplyr::mutate(dplyr::across(c(fob_value, kg, qty), as.numeric)) |>
    dplyr::group_by(year, month, ncm, state, country_code) |>
    dplyr::summarise(dplyr::across(c(fob_value, kg, qty), sum), .groups = "drop") |>
    dplyr::mutate(ncm = stringr::str_pad(ncm, 8, "left", "0")) |>
    dplyr::arrange(month)
}

#' Write processed Comex Stat data to an Arrow dataset
#'
#' Casts a processed data frame to the supplied Arrow schema and writes it to
#' disk partitioned by `year`.
#'
#' @param x tibble. A `tibble` to be written.
#' @param path character string. Output directory for the dataset.
#' @param data_schema An `arrow::schema` to enforce during writing.
#'
#' @return No return value. Writes dataset files to `path`.
#'
#' @keywords internal
#' @noRd
write_cs_db <- function(x, path, data_schema) {
  df <- arrow::arrow_table(x)$cast(data_schema)
  arrow::write_dataset(df, path, partitioning = "year")
}

#' Build one import or export dataset from a Comex Stat download link
#'
#' Downloads a raw Comex Stat CSV, infers whether it is an import or export
#' file from the URL, processes it, and writes the result to the matching Arrow
#' dataset directory.
#'
#' @param link_download Character string. URL to the Comex Stat CSV file.
#' @param db_dirs Character vector containing the local dataset directories.
#' @param schemas Named list of Arrow schemas for the `"imp"` and `"exp"`
#'   datasets.
#'
#' @return No return value. Data is written to disk.
#'
#' @keywords internal
#' @noRd
build_db <- function(link_download, db_dirs, schemas) {
  temp_dir <- file.path(withr::local_tempdir(), "temp.csv")
  year_from_link <- stringr::str_extract(link_download, "[0-9]{4}")
  category <- stringr::str_extract(link_download, "EXP|IMP") |>
    stringr::str_to_lower()

  message(glue::glue("Downloading {category} {year_from_link}\n"))
  download_cs_file(
    link_download = link_download,
    dir_file_download = temp_dir
  )

  if (category == "imp") {
    selected_data <- read_imports(temp_dir)
    write_cs_db(
      x = selected_data,
      path = stringr::str_subset(db_dirs, "imp"),
      data_schema = schemas[["imp"]]
    )
  } else if (category == "exp") {
    selected_data <- read_exports(temp_dir)
    write_cs_db(
      x = selected_data,
      path = stringr::str_subset(db_dirs, "exp"),
      data_schema = schemas[["exp"]]
    )
  }
  message(glue::glue("Download and data write for {category} {year_from_link} complete\n"))
}
