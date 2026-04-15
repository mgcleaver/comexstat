#' Add statistical unit names to Comex Stat data
#'
#' Uses the NCM correlation table to recover the `unit_code` associated with
#' each `ncm` in `x`, then joins the bundled `unit_table` to append the
#' corresponding unit name.
#'
#' @param x A data frame containing an `ncm` column.
#' @param lang Language of the appended unit name. Must be `"en"` or
#'   `"pt"`.
#' @param drop_code Logical. If `TRUE` (default), removes `unit_code` from the
#'   result after the join.
#'
#' @return A data frame with the same rows as `x`, plus `unit_name` or
#'   `unit_name_pt`. When `drop_code = FALSE`, `unit_code` is also kept.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(ncm = c("01012100", "02011000"))
#' add_units(df, lang = "pt")
#' }
#'
#' @export
add_units <- function(
    x,
    lang = c("en","pt"),
    drop_code = TRUE
) {
  lang <- match.arg(lang)

  name_col <- switch(
    lang,
    en = "unit_name",
    pt = "unit_name_pt"
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

  temp <- dplyr::relocate(
    temp,
    unit_code,
    .after = ncm
  )

  temp <- dplyr::relocate(
    temp,
    dplyr::all_of(name_col),
    .after = unit_code
  )

  if (drop_code) {
    temp <- dplyr::select(temp, -unit_code)
  }

  return(temp)
}
