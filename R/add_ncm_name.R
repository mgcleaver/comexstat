#' Add NCM names from Comex Stat
#'
#' Joins the Comex Stat NCM correlation table to `x` using the `ncm` column and
#' appends a name column in the requested language.
#'
#' @param x A data frame containing an `ncm` column.
#' @param lang Language of the appended name. Must be one of `"en"`, `"pt"`,
#'   or `"es"`.
#' @param drop_code Logical. If `TRUE`, removes `ncm` from the result after the
#'   join. Defaults to `FALSE`.
#'
#' @return A data frame with the same rows as `x`, plus one of `ncm_name`,
#'   `ncm_name_pt`, or `ncm_name_es`.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(ncm = c("01012100", "02011000"))
#' add_ncm_name(df, lang = "es")
#' }
#'
#' @export
add_ncm_name <- function(
    x,
    lang = c("en", "pt", "es"),
    drop_code = FALSE
) {
  lang <- match.arg(lang)

  name_col <- switch(
    lang,
    en = "ncm_name",
    pt = "ncm_name_pt",
    es = "ncm_name_es"
  )

  ncm_table <- get_ncm_table(verbose = FALSE)

  temp <- dplyr::left_join(
    x,
    dplyr::select(ncm_table, ncm, dplyr::all_of(name_col)),
    by = "ncm"
  )

  temp <- dplyr::relocate(
    temp,
    dplyr::all_of(name_col),
    .after = ncm
  )

  if (drop_code) {
    temp <- dplyr::select(temp, -ncm)
  }

  return(temp)
}
