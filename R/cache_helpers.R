#' Dynamic correlation tables handled by cache helpers
#' @keywords internal
#' @noRd
dynamic_correlation_tables <- c(
  "PAIS",
  "NCM",
  "NCM_ISIC",
  "NCM_CGCE",
  "NCM_CUCI"
)

#' Clear cached dynamic correlation tables
#'
#' Removes cached files for the dynamic correlation tables refreshed from the
#' Comex Stat source. This clears cache entries only for `"PAIS"`, `"NCM"`,
#' `"NCM_ISIC"`, `"NCM_CGCE"`, and `"NCM_CUCI"`.
#'
#' The bundled internal tables built in `build_internal_data.R`, including
#' `"UF"` and `"NCM_UNIDADE"`, are not removed by this function.
#'
#' @return Invisible character vector with the cache file paths that were
#'   removed.
#'
#' @examples
#' \dontrun{
#' clear_cache()
#' }
#'
#' @export
clear_cache <- function() {
  cache_files <- tibble::tibble(
    table_code = dynamic_correlation_tables,
    data_path = purrr::map_chr(
      dynamic_correlation_tables,
      correlation_cache_data_path
    ),
    meta_path = purrr::map_chr(
      dynamic_correlation_tables,
      correlation_cache_meta_path
    )
  ) |>
    tidyr::pivot_longer(
      cols = c("data_path", "meta_path"),
      names_to = "cache_type",
      values_to = "path"
    )

  cache_files <- cache_files[file.exists(cache_files$path), , drop = FALSE]

  if (nrow(cache_files) > 0) {
    file.remove(cache_files$path)
  }

  invisible(cache_files$path)
}

#' Refresh cached dynamic correlation tables
#'
#' Downloads fresh versions of the dynamic correlation tables from Comex Stat
#' and updates their local cache. This refreshes only `"PAIS"`, `"NCM"`,
#' `"NCM_ISIC"`, `"NCM_CGCE"`, and `"NCM_CUCI"`.
#'
#' The bundled internal tables built in `build_internal_data.R`, including
#' `"UF"` and `"NCM_UNIDADE"`, are not refreshed by this function.
#'
#' @return Invisible named list with the refreshed tables.
#'
#' @examples
#' \dontrun{
#' refresh_cache()
#' }
#'
#' @export
refresh_cache <- function() {
  refreshed_tables <- purrr::map(
    dynamic_correlation_tables,
    get_correlation_table_cached,
    refresh = TRUE
  )
  names(refreshed_tables) <- dynamic_correlation_tables

  invisible(refreshed_tables)
}
