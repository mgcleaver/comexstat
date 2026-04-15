#' Add country names from Comex Stat country codes
#'
#' Joins the Comex Stat country correlation table to `x` using `country_code`
#' and appends a single country-name column in the requested language.
#'
#' @param x A data frame containing a `country_code` column.
#' @param lang Language of the appended country name. Must be one of `"en"`,
#'   `"pt"`, or `"es"`.
#' @param drop_code Logical. If `TRUE` (default), removes `country_code` from
#'   the result after the join.
#'
#' @return A data frame with the same rows as `x`, plus one of
#'   `country_name`, `country_name_pt`, or `country_name_es`. When
#'   `drop_code = TRUE`, the `country_code` column is removed from the output.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(country_code = c(20, 23, 40))
#' add_country_name(df, lang = "pt")
#' }
#'
#' @export
add_country_name <- function(
    x,
    lang = c("en","pt", "es"),
    drop_code = TRUE
) {
  lang <- match.arg(lang)

  name_col <- switch(
    lang,
    en = "country_name",
    pt = "country_name_pt",
    es = "country_name_es"
  )

  country_table <- get_country_table(verbose = FALSE)

  temp <- dplyr::left_join(
    x,
    dplyr::select(country_table, country_code, dplyr::all_of(name_col)),
    by = "country_code"
  )

  temp <- dplyr::relocate(
    temp,
    dplyr::all_of(name_col),
    .after = country_code
  )

  if (drop_code) {
    temp <- dplyr::select(temp, -country_code)
  }

  return(temp)

}
