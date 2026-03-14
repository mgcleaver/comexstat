#' Comex Stat base URL
#' @keywords internal
#' @noRd
cs_base_url <- "https://www.gov.br/mdic/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta"

#' Comex Stat correlation tables base URL
#' @keywords internal
#' @noRd
cs_correlation_tables_base_url <- "https://balanca.economia.gov.br/balanca/bd/tabelas"

#' Correlation tables available on Comex Stat
#' @keywords internal
#' @noRd
available_correlation_tables <- c(
  "UF",
  "PAIS",
  "NCM",
  "NCM_ISIC",
  "NCM_CGCE",
  "NCM_CUCI",
  "NCM_UNIDADE"
)

#' Default cache max age for correlation tables (in days)
#' @keywords internal
#' @noRd
default_correlation_cache_max_age_days <- 31L

#' Find correlation-table links on the Comex Stat website
#'
#' Searches the official Comex Stat downloads page and returns the `href` values
#' of CSV links that match a requested correlation-table code.
#'
#' @param name Character. Correlation-table code without the `.csv` extension,
#'   such as `"PAIS"` or `"UF"`.
#'
#' @return A character vector with matching `href` values. Entries can be
#'   relative or absolute URLs depending on the source page.
#'
#' @details The function normalizes `name` with
#'   [normalize_correlation_table_name()] before searching the page.
#'
#' @examples
#' \dontrun{
#'   find_table_link("PAIS")
#' }
#'
#' @keywords internal
#' @noRd
find_table_link <- function(name) {
  table_code <- normalize_correlation_table_name(name)

  page <- rvest::read_html(cs_base_url)

  page |>
    rvest::html_elements("table tr td a") |>
    rvest::html_attr("href") |>
    stringr::str_subset(glue::glue("/{table_code}.csv"))
}

#' Validate and normalize a correlation table code
#'
#' @param name Character. Correlation table code.
#'
#' @return Upper-case table code.
#' @keywords internal
#' @noRd
normalize_correlation_table_name <- function(name) {
  if (length(name) != 1 || !is.character(name) || is.na(name)) {
    stop("`name` must be a single character string (e.g., 'PAIS').")
  }

  table_code <- stringr::str_to_upper(name)
  if (!table_code %in% available_correlation_tables) {
    stop(
      "`name` must be one of: ",
      paste(available_correlation_tables, collapse = ", "),
      "."
    )
  }

  table_code
}

#' Resolve correlation table URL
#'
#' @param href Character. URL (absolute or relative).
#'
#' @return Absolute URL.
#' @keywords internal
#' @noRd
resolve_correlation_table_url <- function(href) {
  if (stringr::str_detect(href, "^https?://")) {
    return(href)
  }

  if (stringr::str_detect(href, "^/")) {
    return(paste0("https://balanca.economia.gov.br", href))
  }

  paste0(cs_correlation_tables_base_url, "/", href)
}

#' Download a Comex Stat correlation table
#'
#' Downloads a correlation table from a URL and saves it to a temporary CSV
#' file.
#'
#' @param url Character. The full URL of the `.csv` file to download.
#'
#' @return A character string with the path to the downloaded file.
#'
#' @details Uses `httr::GET()` with a progress bar and a `tempfile()`
#'   destination.
#'
#' @examples
#' \dontrun{
#'   href <- find_table_link("PAIS")
#'   url <- resolve_correlation_table_url(href[[1]])
#'   path <- download_correlation_table(url)
#' }
#'
#' @keywords internal
#' @noRd
download_correlation_table <- function(url) {
  dest_file <- tempfile(fileext = ".csv")

  resp <- httr::GET(
    url,
    httr::write_disk(dest_file, overwrite = TRUE),
    httr::progress()
  )

  if (httr::http_error(resp)) {
    stop("Download error: ", httr::http_status(resp)$message)
  }

  if (!file.exists(dest_file) || file.info(dest_file)$size == 0) {
    stop("Download failed: file doesn't exist or empty.")
  }
  message("Download complete\n")
  dest_file
}

#' Score likely mojibake in a correlation table
#'
#' Heuristic used to compare candidate encodings when reading a correlation
#' table. Lower scores indicate fewer suspicious character sequences.
#'
#' @param df A data frame containing character columns to inspect.
#'
#' @return A numeric score where lower values are better.
#'
#' @keywords internal
#' @noRd
correlation_table_encoding_score <- function(df) {
  char_cols <- names(df)[vapply(df, is.character, logical(1))]
  if (length(char_cols) == 0) {
    return(0)
  }

  values <- unlist(df[char_cols], use.names = FALSE)
  values <- values[!is.na(values)]
  if (length(values) == 0) {
    return(0)
  }

  max_values <- min(10000L, length(values))
  sample_text <- paste(values[seq_len(max_values)], collapse = " ")
  sample_text <- enc2utf8(sample_text)

  bad_count <- 0L
  bad_count <- bad_count + stringr::str_count(sample_text, stringr::fixed("Ã"))
  bad_count <- bad_count + stringr::str_count(sample_text, stringr::fixed("Â"))
  bad_count <- bad_count + stringr::str_count(sample_text, stringr::fixed("�"))
  bad_count <- bad_count + stringr::str_count(sample_text, stringr::fixed("â€"))

  good_count <- stringr::str_count(
    sample_text,
    "[áéíóúâêôãõçÁÉÍÓÚÂÊÔÃÕÇ]"
  )

  (bad_count * 10L) - good_count
}

#' Convert character columns to UTF-8
#'
#' @param df A data.frame.
#'
#' @return A data.frame with UTF-8 character columns.
#' @keywords internal
#' @noRd
normalize_character_columns_utf8 <- function(df) {
  char_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (col in char_cols) {
    df[[col]] <- enc2utf8(df[[col]])
  }
  df
}

#' Read a correlation table trying different encodings
#'
#' Attempts to read a CSV file with multiple encodings, normalizes character
#' columns to UTF-8, and selects the candidate with the best text-quality score.
#'
#' @param path Character. Full path to the file.
#' @param encodings Character vector with candidate encodings.
#'
#' @return A list with `data`, `encoding`, and the internal quality `score`.
#' @keywords internal
#' @noRd
read_correlation_table_with_best_encoding <- function(
    path,
    encodings = c("UTF-8", "Latin1", "Windows-1252")
) {
  attempts <- list()

  for (enc in encodings) {
    read_attempt <- tryCatch(
      {
        raw_df <- utils::read.csv2(
          path,
          fileEncoding = enc,
          stringsAsFactors = FALSE
        )

        normalized_df <- normalize_character_columns_utf8(raw_df)
        score <- correlation_table_encoding_score(normalized_df)

        list(
          encoding = enc,
          score = score,
          data = normalized_df
        )
      },
      error = function(e) NULL
    )

    if (!is.null(read_attempt)) {
      attempts[[length(attempts) + 1L]] <- read_attempt
    }
  }

  if (length(attempts) == 0) {
    stop(
      "Failed to read correlation table with tried encodings: ",
      paste(encodings, collapse = ", "),
      "."
    )
  }

  scores <- vapply(attempts, function(x) x$score, numeric(1))
  best_idx <- which.min(scores)
  attempts[[best_idx]]
}

#' Read a correlation table from a CSV file
#'
#' Reads a correlation table using the best encoding detected by
#' [read_correlation_table_with_best_encoding()], cleans column names, and
#' returns a tibble.
#'
#' @param path Character. Full path to the `.csv` file to read.
#'
#' @return A `tibble` containing the cleaned correlation table. The selected
#'   encoding is stored in the `source_encoding` attribute.
#' @keywords internal
#' @noRd
read_correlation_table <- function(path) {
  selected <- read_correlation_table_with_best_encoding(path = path)
  selected$data |>
    janitor::clean_names() |>
    tibble::as_tibble() |>
    structure(source_encoding = selected$encoding)
}

#' Rename correlation-table columns when known aliases are present
#'
#' Checks whether source-specific column names are present in `df` and renames
#' them to the package's standardized English names.
#'
#' @param df A `data.frame` or `tibble`.
#'
#' @return A `data.frame` or `tibble` with known columns renamed. Columns not in
#'   the internal renaming map remain unchanged.
#'
#' @keywords internal
#' @noRd
rename_columns_if_present <- function(df) {

  name_map <- c(
    "co_pais" = "country_code",
    "co_pais_ison3" = "country_code_ison3",
    "co_pais_isoa3" = "country_code_isoa3",
    "no_pais" = "country_name_pt",
    "no_pais_ing" = "country_name",
    "no_pais_esp" = "country_name_es",
    "co_uf" = "state_code",
    "sg_uf" = "state",
    "no_uf" = "state_name",
    "no_regiao" = "region_name",
    "co_ncm" = "ncm",
    "co_sh6" = "hs6_code",
    "co_ppe" = "ppe_code",
    "co_ppi" = "ppi_code",
    "co_fat_agreg" = "aggregation_factor_code",
    "co_siit" = "siit_code",
    "co_exp_subset" = "export_subset_code",
    "no_ncm_ing" = "ncm_description",
    "no_ncm_por" = "ncm_description_pt",
    "no_ncm_esp" = "ncm_description_es",
    "co_isic_classe" = "isic_class_code",
    "no_isic_classe" = "isic_class_desc_pt",
    "no_isic_classe_ing" = "isic_class_desc",
    "no_isic_classe_esp" = "isic_class_desc_es",
    "co_isic_grupo" = "isic_group_code",
    "no_isic_grupo" = "isic_group_desc_pt",
    "no_isic_grupo_ing" = "isic_group_desc",
    "no_isic_grupo_esp" = "isic_group_desc_es",
    "co_isic_divisao" = "isic_division_code",
    "no_isic_divisao" = "isic_division_desc_pt",
    "no_isic_divisao_ing" = "isic_division_desc",
    "no_isic_divisao_esp" = "isic_division_desc_es",
    "co_isic_secao" = "isic_section_code",
    "no_isic_secao" = "isic_section_desc_pt",
    "no_isic_secao_ing" = "isic_section_desc",
    "no_isic_secao_esp" = "isic_section_desc_es",
    "co_cgce_n3" = "bec_n3_code",
    "no_cgce_n3" = "bec_n3_desc_pt",
    "no_cgce_n3_ing" = "bec_n3_desc",
    "no_cgce_n3_esp" = "bec_n3_desc_es",
    "co_cgce_n2" = "bec_n2_code",
    "no_cgce_n2" = "bec_n2_desc_pt",
    "no_cgce_n2_ing" = "bec_n2_desc",
    "no_cgce_n2_esp" = "bec_n2_desc_es",
    "co_cgce_n1" = "bec_n1_code",
    "no_cgce_n1" = "bec_n1_desc_pt",
    "no_cgce_n1_ing" = "bec_n1_desc",
    "no_cgce_n1_esp" = "bec_n1_desc_es",
    "co_unid" = "unit_code",
    "co_cuci_item" = "cuci_basic_heading_code",
    "co_cuci_sub" = "cuci_subgroup_code",
    "co_cuci_grupo" = "cuci_group_code",
    "co_cuci_divisao" = "cuci_division_code",
    "co_cuci_sec" = "cuci_section_code",
    "no_cuci_item" = "cuci_basic_heading_desc_pt",
    "no_cuci_sub" = "cuci_subgroup_desc_pt",
    "no_cuci_grupo" = "cuci_group_desc_pt",
    "no_cuci_divisao" = "cuci_division_desc_pt",
    "no_cuci_sec" = "cuci_section_desc_pt"
    )

  rename_map <- name_map[names(name_map) %in% names(df)]
  for (old_name in names(rename_map)) {
    names(df)[names(df) == old_name] <- rename_map[[old_name]]
  }

  return(df)
}

#' Correlation-table cache directory
#'
#' Creates the package cache directory for correlation tables when needed.
#'
#' @return Character string. Path where cached correlation tables are stored.
#' @keywords internal
#' @noRd
correlation_cache_dir <- function() {
  dir <- file.path(
    tools::R_user_dir("comexstat", which = "cache"),
    "correlation_tables"
  )
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

#' Correlation-table cache data path
#'
#' @param table_code Character. Correlation table code.
#'
#' @return Character string. Path to cached table data.
#' @keywords internal
#' @noRd
correlation_cache_data_path <- function(table_code) {
  file.path(correlation_cache_dir(), paste0(table_code, ".rds"))
}

#' Correlation-table cache metadata path
#'
#' @param table_code Character. Correlation table code.
#'
#' @return Character string. Path to cached table metadata.
#' @keywords internal
#' @noRd
correlation_cache_meta_path <- function(table_code) {
  file.path(correlation_cache_dir(), paste0(table_code, "_meta.rds"))
}

#' Read a cached correlation table if available
#'
#' @param table_code Character. Correlation table code.
#'
#' @return `NULL` when cache files are missing, otherwise a list with `data`
#'   and `meta` elements loaded from disk.
#' @keywords internal
#' @noRd
read_correlation_table_cache <- function(table_code) {
  data_path <- correlation_cache_data_path(table_code)
  meta_path <- correlation_cache_meta_path(table_code)

  if (!file.exists(data_path) || !file.exists(meta_path)) {
    return(NULL)
  }

  list(
    data = readRDS(data_path),
    meta = readRDS(meta_path)
  )
}

#' Write a correlation table to the local cache
#'
#' @param table_code Character. Correlation table code.
#' @param table_data Tibble/data.frame with table content.
#' @param source_url Character. Download URL used to fetch the table.
#' @param source_encoding Character. Chosen file encoding used for reading.
#'
#' @return Invisible NULL.
#' @keywords internal
#' @noRd
write_correlation_table_cache <- function(
    table_code,
    table_data,
    source_url,
    source_encoding = NA_character_
) {
  data_path <- correlation_cache_data_path(table_code)
  meta_path <- correlation_cache_meta_path(table_code)
  downloaded_at <- Sys.time()

  meta <- list(
    table_code = table_code,
    source_url = source_url,
    source_encoding = source_encoding,
    downloaded_at = downloaded_at,
    n_rows = nrow(table_data),
    n_cols = ncol(table_data)
  )

  saveRDS(table_data, data_path)
  saveRDS(meta, meta_path)

  invisible(NULL)
}

#' Check whether a cached table is stale
#'
#' @param cache_meta List. Metadata from cache.
#' @param max_age_days Numeric. Maximum cache age in days.
#'
#' @return Logical. `TRUE` when the cache age exceeds `max_age_days`.
#' @keywords internal
#' @noRd
is_correlation_cache_stale <- function(cache_meta, max_age_days) {
  age_days <- as.numeric(
    difftime(
      Sys.time(),
      as.POSIXct(cache_meta$downloaded_at, origin = "1970-01-01"),
      units = "days"
    )
  )

  age_days > max_age_days
}

#' Format cache timestamps for user messages
#'
#' @param datetime POSIXct value or an object coercible to POSIXct.
#'
#' @return Character string formatted in UTC.
#' @keywords internal
#' @noRd
format_cache_datetime <- function(datetime) {
  format(
    as.POSIXct(datetime, origin = "1970-01-01", tz = "UTC"),
    "%Y-%m-%d %H:%M:%S UTC"
  )
}

#' Download and process a correlation table
#'
#' @param table_code Character. Correlation table code.
#'
#' @return Named list with `data`, `source_url`, and `source_encoding`.
#' @keywords internal
#' @noRd
download_correlation_table_data <- function(table_code) {
  links <- unique(find_table_link(table_code))
  links <- stringr::str_subset(links, glue::glue("/{table_code}.csv$"))

  if (length(links) == 0) {
    stop("No link found for correlation table '", table_code, "'.")
  }

  if (length(links) > 1) {
    stop(
      "More than one link found for correlation table '",
      table_code,
      "'."
    )
  }

  source_url <- resolve_correlation_table_url(links[[1]])
  download_path <- download_correlation_table(source_url)

  raw_table <- read_correlation_table(download_path)
  source_encoding <- attr(raw_table, "source_encoding", exact = TRUE)

  table_data <- raw_table |>
    rename_columns_if_present() |>
    post_process_correlation_table(table_code)

  list(
    data = table_data,
    source_url = source_url,
    source_encoding = source_encoding
  )
}

#' Apply table-specific post-processing without dropping columns
#'
#' @param df Tibble/data.frame.
#' @param table_code Character. Correlation table code.
#'
#' @return Tibble with standardized columns and table-specific transforms. For
#'   the `"NCM"` table, `ncm` is left-padded to eight digits.
#' @keywords internal
#' @noRd
post_process_correlation_table <- function(df, table_code) {
  if (table_code == "NCM" && "ncm" %in% names(df)) {
    df <- dplyr::mutate(
      df,
      ncm = stringr::str_pad(as.character(ncm), 8, side = "left", pad = "0")
    )
  }

  tibble::as_tibble(df)
}

#' Get a correlation table with cache, refresh, and fallback behavior
#'
#' @param name Character. Correlation table code.
#' @param refresh Logical. Force refresh from remote source.
#' @param max_age_days Numeric. Max cache age in days before refresh attempt.
#' @param verbose Logical. Inform cache/download decisions in messages.
#'
#' @return Tibble with the requested correlation table.
#'
#' @details
#' If a non-stale cache exists and `refresh = FALSE`, the cached table is
#' returned immediately. Otherwise the function tries to download a fresh copy,
#' updates the cache on success, and falls back to the cached version if the
#' download fails.
#' @keywords internal
#' @noRd
get_correlation_table_cached <- function(
    name,
    refresh = FALSE,
    max_age_days = default_correlation_cache_max_age_days,
    verbose = TRUE
) {
  table_code <- normalize_correlation_table_name(name)

  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("`refresh` must be TRUE or FALSE.")
  }

  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.")
  }

  if (
    !is.numeric(max_age_days) ||
    length(max_age_days) != 1 ||
    is.na(max_age_days) ||
    max_age_days < 0
  ) {
    stop("`max_age_days` must be a single non-negative number.")
  }

  cached <- read_correlation_table_cache(table_code)

  if (!is.null(cached)) {
    is_stale <- is_correlation_cache_stale(cached$meta, max_age_days)
    cache_date <- format_cache_datetime(cached$meta$downloaded_at)
    cache_encoding <- cached$meta$source_encoding
    cache_encoding_msg <- if (!is.null(cache_encoding) && !is.na(cache_encoding)) {
      paste0(" (encoding: ", cache_encoding, ")")
    } else {
      ""
    }

    if (!refresh && !is_stale) {
      if (verbose) {
        message(
          "Using cached '", table_code,
          "' downloaded at ", cache_date, cache_encoding_msg, "."
        )
      }
      return(cached$data)
    }

    if (verbose) {
      message(
        "Cached '", table_code, "' downloaded at ", cache_date,
        cache_encoding_msg,
        " is stale or refresh was requested. Trying to update..."
      )
    }
  } else if (verbose) {
    message(
      "No local cache found for '", table_code, "'. Downloading from source..."
    )
  }

  download_result <- tryCatch(
    download_correlation_table_data(table_code),
    error = function(e) e
  )

  if (!inherits(download_result, "error")) {
    write_correlation_table_cache(
      table_code = table_code,
      table_data = download_result$data,
      source_url = download_result$source_url,
      source_encoding = download_result$source_encoding
    )

    if (verbose) {
      encoding_msg <- if (!is.null(download_result$source_encoding) &&
        !is.na(download_result$source_encoding)) {
        paste0(" (encoding: ", download_result$source_encoding, ")")
      } else {
        ""
      }

      message(
        "Downloaded and cached '", table_code,
        "' at ",
        format_cache_datetime(Sys.time()),
        encoding_msg,
        "."
      )
    }
    return(download_result$data)
  }

  if (!is.null(cached)) {
    if (verbose) {
      message(
        "Update failed for '", table_code,
        "'. Using cached version downloaded at ",
        format_cache_datetime(cached$meta$downloaded_at),
        cache_encoding_msg,
        ". Error: ",
        download_result$message
      )
    }
    return(cached$data)
  }

  stop(
    "Failed to download '", table_code,
    "' and no cache is available. Error: ",
    download_result$message
  )
}

#' Legacy wrapper for correlation-table retrieval
#'
#' @param name Character. Correlation table code.
#'
#' @return Tibble with the requested correlation table.
#' @keywords internal
#' @noRd
process_table <- function(name) {
  get_correlation_table_cached(name = name, refresh = FALSE)
}

#' Get the country correlation table
#'
#' @param refresh Logical. Force refresh from remote source.
#' @param max_age_days Numeric. Max cache age in days before refresh attempt.
#' @param verbose Logical. Inform cache/download decisions in messages.
#'
#' @return Tibble with country codes and names.
#' @keywords internal
#' @noRd
get_country_table <- function(
    refresh = FALSE,
    max_age_days = default_correlation_cache_max_age_days,
    verbose = TRUE
) {
  get_correlation_table_cached(
    name = "PAIS",
    refresh = refresh,
    max_age_days = max_age_days,
    verbose = verbose
  )
}

#' Get the NCM correlation table
#'
#' @param refresh Logical. Force refresh from remote source.
#' @param max_age_days Numeric. Max cache age in days before refresh attempt.
#' @param verbose Logical. Inform cache/download decisions in messages.
#'
#' @return Tibble with NCM codes and related metadata.
#' @keywords internal
#' @noRd
get_ncm_table <- function(
    refresh = FALSE,
    max_age_days = default_correlation_cache_max_age_days,
    verbose = TRUE
) {
  get_correlation_table_cached(
    name = "NCM",
    refresh = refresh,
    max_age_days = max_age_days,
    verbose = verbose
  )
}

#' Get the ISIC correlation table
#'
#' @param refresh Logical. Force refresh from remote source.
#' @param max_age_days Numeric. Max cache age in days before refresh attempt.
#' @param verbose Logical. Inform cache/download decisions in messages.
#'
#' @return Tibble with ISIC codes and descriptions.
#' @keywords internal
#' @noRd
get_isic_table <- function(
    refresh = FALSE,
    max_age_days = default_correlation_cache_max_age_days,
    verbose = TRUE
) {
  get_correlation_table_cached(
    name = "NCM_ISIC",
    refresh = refresh,
    max_age_days = max_age_days,
    verbose = verbose
  )
}

#' Get the BEC correlation table
#'
#' @param refresh Logical. Force refresh from remote source.
#' @param max_age_days Numeric. Max cache age in days before refresh attempt.
#' @param verbose Logical. Inform cache/download decisions in messages.
#'
#' @return Tibble with BEC codes and descriptions.
#' @keywords internal
#' @noRd
get_bec_table <- function(
    refresh = FALSE,
    max_age_days = default_correlation_cache_max_age_days,
    verbose = TRUE
) {
  get_correlation_table_cached(
    name = "NCM_CGCE",
    refresh = refresh,
    max_age_days = max_age_days,
    verbose = verbose
  )
}

#' Get the CUCI correlation table
#'
#' @param refresh Logical. Force refresh from remote source.
#' @param max_age_days Numeric. Max cache age in days before refresh attempt.
#' @param verbose Logical. Inform cache/download decisions in messages.
#'
#' @return Tibble with CUCI codes and descriptions.
#' @keywords internal
#' @noRd
get_cuci_table <- function(
    refresh = FALSE,
    max_age_days = default_correlation_cache_max_age_days,
    verbose = TRUE
) {
  get_correlation_table_cached(
    name = "NCM_CUCI",
    refresh = refresh,
    max_age_days = max_age_days,
    verbose = verbose
  )
}

#' Get the state reference table
#'
#' Returns the bundled `state_table` when it is available in the package
#' namespace; otherwise falls back to the cached/downloaded `"UF"` correlation
#' table.
#'
#' @param refresh Logical. Force refresh from remote source when the fallback
#'   path is used.
#' @param max_age_days Numeric. Max cache age in days before refresh attempt.
#' @param verbose Logical. Inform cache/download decisions in messages.
#'
#' @return Tibble with state codes, abbreviations, names, and regions.
#' @keywords internal
#' @noRd
get_state_table <- function(
    refresh = FALSE,
    max_age_days = default_correlation_cache_max_age_days,
    verbose = TRUE
) {
  if (exists("state_table", envir = asNamespace("comexstat"), inherits = FALSE)) {
    return(get("state_table", envir = asNamespace("comexstat"), inherits = FALSE))
  }

  get_correlation_table_cached(
    name = "UF",
    refresh = refresh,
    max_age_days = max_age_days,
    verbose = verbose
  )
}

#' Get the bundled unit reference table
#'
#' @return Tibble with unit codes and unit descriptions.
#' @keywords internal
#' @noRd
get_unit_table <- function() {
  unit_table
}
