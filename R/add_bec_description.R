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
#' @param drop_key Logical. If `TRUE` (the default), the intermediate join key
#'   `bec_n3_code` is removed from the output.
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
    drop_key = TRUE
) {
  lang <- match.arg(lang)
  level <- match.arg(level)

  lang_col <- switch(
    lang,
    en = "desc$",
    pt = "desc_pt$",
    es = "desc_es$"
  )

  level_col <- dplyr::if_else(
    level == "all",
    "n3|n2|n1",
    level
  )

  regex_col_select <- paste0(
    "(?=.*",
    level_col,
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
        dplyr::matches(regex_col_select, perl = TRUE)
      ),
      by = "bec_n3_code"
    )

  if (drop_key) {
    temp <- dplyr::select(temp, -bec_n3_code)
  }

  temp
}
