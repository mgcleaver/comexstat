#' Add BEC descriptions from Comex Stat
#'
#' Uses the NCM and BEC correlation tables to map each `ncm` in `x` to its
#' Broad Economic Category (BEC) classification and append description columns
#' at the requested aggregation level.
#'
#' @param x A data frame containing an `ncm` column.
#' @param lang Language of the appended BEC descriptions. Must be one of
#'   `"en"`, `"pt"`, or `"es"`.
#' @param level BEC aggregation level to return. Must be one of `"n3"`, `"n2"`,
#'   `"n1"`, or `"all"`.
#' @param drop_code Logical. If `TRUE` (default), removes joined BEC code
#'   columns from the result.
#'
#' @return A data frame with the same rows as `x`, plus the selected BEC
#'   description columns for the requested language. When `drop_code = FALSE`,
#'   the corresponding BEC code columns are also retained.
#'
#' @details
#' The function first joins `x` to the NCM table to recover `bec_n3_code` and
#' then joins the BEC table to retrieve the requested descriptions. When
#' `level = "all"`, descriptions for all available BEC levels are returned.
#' When `level` is `"n2"` or `"n1"`, intermediate `n3` columns are removed from
#' the final output.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' df <- tibble(ncm = c("01012100", "02011000"))
#'
#' # Add BEC descriptions at level n3 in Portuguese
#' add_bec_description(df, lang = "pt", level = "n3")
#'
#' # Add all BEC levels in English
#' add_bec_description(df, lang = "en", level = "all")
#' }
#'
#' @export
add_bec_description <- function(
    x,
    lang = c("en", "pt", "es"),
    level = c("n3", "n2", "n1", "all"),
    drop_code = TRUE
) {
  lang <- match.arg(lang)
  level <- match.arg(level)

  lang_col <- switch(
    lang,
    en = "desc$",
    pt = "desc_pt$",
    es = "desc_es$"
  )

  if (level == "all") {
    level_col_desc <- "(n3|n2|n1)"
    regex_col_code <- paste0(
      c("n3", "n2", "n1"),
      "_code",
      collapse = "|"
      )
  } else {
    level_col_desc <- level
    regex_col_code <- paste0(level, "_code")
  }

  regex_col_desc <- paste0(
    "(?=.*",
    level_col_desc,
    ")(?=.*",
    lang_col,
    ")"
  )

  ncm_table <- get_ncm_table(verbose = FALSE)
  bec_table <- get_bec_table(verbose = FALSE)

  temp <- dplyr::left_join(
    x,
    dplyr::select(ncm_table, ncm, bec_n3_code),
    by = "ncm"
  ) |>
    dplyr::left_join(
      dplyr::select(
        bec_table,
        bec_n3_code,
        dplyr::matches(regex_col_code, perl = TRUE),
        dplyr::matches(regex_col_desc, perl = TRUE)
      ),
      by = "bec_n3_code"
    )

  if (level != "n3" & level != "all") {
    temp <- temp |>
      dplyr::select(-dplyr::matches("n3"))
  }

  if (drop_code) {
    remove_codes <- temp |>
      dplyr::select(dplyr::matches("code")) |>
      names() |>
      stringr::str_subset("country", negate = TRUE) |>
      paste0(collapse = "|")
    temp <- dplyr::select(temp, -dplyr::matches(remove_codes))
  }

  temp
}
