#' Add statiscal unit descriptions to Comex Stat data
#'
#' This helper function matches NCM codes with their corresponding  statistical
#' unit codes and then appends the unit description in the specified language.
#' The function complements the understanding of the qty (quantity) variable
#' of the database created with `create_cs_db`.
#'
#' The function uses external `ncm_table` and internal `unit_table`
#' data stored in the package.
#'
#' @param x A tibble containing a column named `ncm`.
#' @param lang A string indicating the desired language for the unit description.
#'   Must be `"en"` (English) or `"pt"` (Portuguese).
#'   Defaults to `"en"`.
#' @param drop_key Logical. If `TRUE` (default), the `unit_code` column is
#' not included in the final table.
#'
#' @return A tibble or data frame with the unit_description. In case drop_key =
#' `FALSE`, then the unit_code will also be present.
#'
#' @export
add_units <- function(
    x,
    lang = c("en","pt"),
    drop_key = TRUE
) {
  lang <- match.arg(lang)

  name_col <- switch(
    lang,
    en = "unit_description",
    pt = "unit_description_pt"
  )

  ncm_table <- get_ncm_table(verbose = FALSE)

  temp <- dplyr::left_join(
    x,
    dplyr::select(ncm_table, ncm, unit_code),
    by = "ncm"
  )

  temp <- dplyr::left_join(
    temp,
    dplyr::select(unit_table, unit_code, dplyr::all_of(name_col)),
    by = "unit_code"
  )

  if (drop_key) {
    temp <- dplyr::select(temp, -unit_code)
  }

  return(temp)
}
