#' Add BEC level descriptions based on Comex Stat's BEC table
#'
#' This function adds textual descriptions from the Broad Economic Category
#' (BEC) classification to a dataset containing NCM codes.
#'
#' @param x A data frame containing a column `ncm` with NCM codes.
#' @param lang A string indicating the desired language for BEC descriptions.
#'   Must be one of `"en"` (English), `"pt"` (Portuguese), or `"es"` (Spanish).
#' @param level A string specifying the BEC aggregation level to use.
#'   Must be one of:
#'   - `"n3"`
#'   - `"n2"`
#'   - `"n1"`
#'   - `"all"` (returns all available BEC levels)
#' @param drop_code Logical. If `TRUE` (the default), all column codes are removed
#' from output.
#'
#' @return
#' A data frame with the same rows as `x`, extended with one or more BEC
#' description columns corresponding to the specified level(s) and language.
#'
#' @details
#' When `level = "all"`, all BEC description columns for the selected language
#' are returned. Otherwise, only the column(s) corresponding to the chosen level
#' are included.
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
