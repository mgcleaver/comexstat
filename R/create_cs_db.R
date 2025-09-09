#' Create Local Comex Stat Database from Official Raw Files
#'
#' Downloads and processes public Comex Stat data from the Brazilian government
#' website, creating a local database organized in Apache Arrow format, partitioned by year.
#'
#' @param dest_dir Character string. Path to the directory where the database should be stored.
#' @param start_year Integer. First year to include in the database (e.g., `2015`).
#' @param timeout Integer. Timeout limit in seconds for HTTP requests. Default is `2000`.
#'
#' @return No return value. Side effect: creates and populates local folders (`export/` and `import/`) with Arrow datasets.
#'
#' @details
#' The function accesses the official Comex Stat page and downloads monthly
#' import and export data (CSV format), processes the files, and writes them as
#' partitioned Arrow datasets. Only data from `start_year` to the current year is included.
#'
#' The structure is ready for efficient analysis using `arrow::open_dataset()`.
#'
#' @seealso [update_cs_db()]
#'
#' @examples
#' \dontrun{
#' create_cs_db(dest_dir = "data/comexstat", start_year = 2015)
#' }
#'
#' @export
create_cs_db <- function(
    dest_dir,
    start_year,
    timeout = 2000
) {

  # Link for raw data
  link_cs <- cs_base_url

  options(timeout = timeout)

  # filter out undesired years for database
  filter_out_interval <- 1997:(start_year - 1)

  # get current date
  current_date <- Sys.Date()

  # get current year
  current_year <- stringr::str_sub(current_date, 1, 4) |>
    as.numeric()

  # get all available years for comex stat. 1997 is the first available year
  cs_all_years_series <- 1997:current_year

  # get years for desired data
  desired_data <- setdiff(cs_all_years_series, filter_out_interval)

  # paths for export and import data
  path_exp <- file.path(dest_dir, "export")
  path_imp <- file.path(dest_dir, "import")

  paths <- c(path_exp, path_imp)

  # create export and import schemas
  schema_comexstat_exp <- create_schema(category = "export")
  schema_comexstat_imp <- create_schema(category = "import")

  df_schemas <- list(exp = schema_comexstat_exp, imp = schema_comexstat_imp)

  message("Creating export and import folders inside desired path\n")
  # creates folder for exports
  dir.create(
    path_exp,
    recursive = TRUE
  )

  # creates folder for imports
  dir.create(
    path_imp,
    recursive = TRUE
  )

  years_to_download <- paste0(desired_data, collapse = "|")

  # get html data
  page <- rvest::read_html(link_cs)

  # links to download database
  links_download <- page |>
    rvest::html_elements("table tr td a") |>
    rvest::html_attr("href") |>
    stringr::str_subset("/ncm/") |>
    stringr::str_subset("COMPLETA|CONFERENCIA", negate = TRUE) |>
    stringr::str_subset(years_to_download)

  # download and build database
  message("Downloading files...\n")
  links_download |>
    purrr::walk(~ build_db(
      .x,
      db_dirs = paths,
      schemas = df_schemas)
    )

  message(glue::glue("Database written to: {dest_dir}\n"))

}

#' Update Local Comex Stat Database
#'
#' Checks if the local database is outdated and updates data if necessary.
#'
#' @param dest_dir Character string. Path to the base directory of the local database (same used in `create_cs_db()`).
#' @param start_year Integer. If supplied, besides updating, the function will check if the desired first year
#'                   is included in the Comex Stat dataset. In case it is not it will download all
#'                   years not included in the dataset starting from start_year. .
#'                   If `NULL`, the function will only will try to update the dataset.
#' @param timeout Integer. Timeout limit in seconds for HTTP requests. Default is `2000`.
#'
#' @return No return value. Side effect: updates existing local Arrow datasets with newer data from the official Comex Stat raw files.
#'
#' @details
#' This function compares the date in the local Arrow datasets with the most recent date in
#' the official database files. In case local dataset is not updated, the function
#' will update local data.
#'
#' If the local database is already up-to-date, the function stops with a message.
#'
#' @seealso [create_cs_db()]
#'
#' @examples
#' \dontrun{
#' update_cs_db(dest_dir = "data/comexstat")
#' }
#'
#' @export
update_cs_db <- function(
    dest_dir,
    start_year = NULL,
    timeout = 2000
) {

  link_cs <- cs_base_url

  options(timeout = timeout)

  # paths for export and import data
  path_exp <- file.path(dest_dir, "export")
  path_imp <- file.path(dest_dir, "import")

  paths <- c(path_exp, path_imp)

  # get available years in export database
  available_exp_years <- try(
    arrow::open_dataset(path_exp) |>
      dplyr::select(year) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(year) |>
      sort(),
    silent = TRUE
  )

  # get available years in import database
  available_imp_years <- try(
    arrow::open_dataset(path_imp) |>
      dplyr::select(year) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(year) |>
      sort(),
    silent = TRUE)

  if (
    inherits(available_exp_years, "try-error") ||
    inherits(available_imp_years, "try-error")
  ) {
    stop("The database was generated with errors and cannot be updated.
         Try running the function create_cs_db\n")
  }

  message("Updating database\n")
  # check start_year to build/update database
  if (is.null(start_year)) {
    exp_min_year <- min(available_exp_years) |>
      unique()
    imp_min_year <- min(available_exp_years) |>
      unique()

    if (exp_min_year == imp_min_year) {
      start_year <- exp_min_year
    }

    if(exp_min_year > imp_min_year) {
      start_year <- imp_min_year
    } else {
      start_year <- exp_min_year
    }
  }

  # filter out years to download
  filter_out_interval <- 1997:(start_year - 1)

  # get current date
  current_date <- Sys.Date()

  # get current year
  current_year <- stringr::str_sub(current_date, 1, 4) |>
    as.numeric()

  # get all available years in comex stat. 1997 is the first year of the database.
  cs_all_years_series <- 1997:current_year

  # get desired years for database
  desired_data <- setdiff(cs_all_years_series, filter_out_interval)

  # create schemas for export and import database
  schema_comexstat_exp <- create_schema(category = "export")
  schema_comexstat_imp <- create_schema(category = "import")

  df_schemas <- list(exp = schema_comexstat_exp, imp = schema_comexstat_imp)

  # ignore years that have already been downloaded correctly
  years_to_exclude_from_download <- intersect(available_exp_years, available_imp_years)

  # get most recent date for import database
  imp_last_update <- arrow::open_dataset(path_imp) |>
    dplyr::select(year, month) |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::filter(max(year) == year) |>
    dplyr::filter(max(month) == month) |>
    dplyr::mutate(month = stringr::str_pad(month, width = 2, side = 'left', pad = '0')) |>
    tidyr::unite(col = "atualizacao", year, month, sep = "-") |>
    dplyr::pull(atualizacao)

  # get most recent date for export database
  exp_last_update <- arrow::open_dataset(path_exp) |>
    dplyr::select(year, month) |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::filter(max(year) == year) |>
    dplyr::filter(max(month) == month) |>
    dplyr::mutate(month = stringr::str_pad(month, width = 2, side = 'left', pad = '0')) |>
    tidyr::unite(col = "atualizacao", year, month, sep = "-") |>
    dplyr::pull(atualizacao)

  # tests if databases are updated
  test_db_export <- compare_local_db(file_dir = path_exp)
  test_db_import <- compare_local_db(file_dir = path_imp)

  # get the last available year in the official comex stat database
  official_last_year_update <- stringr::str_sub(get_last_update(), 1, 4)

  # get the last year on the local database
  local_last_year_update <- stringr::str_sub(imp_last_update, 1, 4)

  # years do remove from download
  years_to_exclude_from_download <-
    years_to_exclude_from_download[!years_to_exclude_from_download %in% local_last_year_update:official_last_year_update]

  # choose year to download
  years_to_download <- setdiff(desired_data, years_to_exclude_from_download)

  # checks if desired year are already in database
  teste_anos_anteriores <- stringr::str_detect(
    years_to_download,
    official_last_year_update,
    negate = TRUE
  ) |>
    any()

  if(test_db_export && test_db_import && !teste_anos_anteriores) {
    stop("The database is already updated")
  }

  message(glue::glue("Local Comex Stat database is outdated. Updating...\n"))

  # years to download
  years_to_download <- paste0(years_to_download, collapse = "|")

  # get html data
  page <- rvest::read_html(link_cs)

  # get relevant links for download
  links_download <- page |>
    rvest::html_elements("table tr td a") |>
    rvest::html_attr("href") |>
    stringr::str_subset("/ncm/") |>
    stringr::str_subset("COMPLETA|CONFERENCIA", negate = TRUE) |>
    stringr::str_subset(years_to_download)

  # download and update database
  message("Downloading files...\n")
  links_download |>
    purrr::walk(~ build_db(
      .x,
      db_dirs = paths,
      schemas = df_schemas)
    )
  message("Update complete\n")
}

