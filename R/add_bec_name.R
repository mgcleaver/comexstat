#' Add BEC names from Comex Stat
#'
#' Uses the NCM and BEC correlation tables to map each `ncm` in `x` to its
#' Broad Economic Category (BEC) classification and append name columns at the
#' requested aggregation level.
#'
#' @param x A data frame containing an `ncm` column.
#' @param lang Language of the appended BEC names. Must be one of `"en"`,
#'   `"pt"`, or `"es"`.
#' @param level BEC aggregation level to return. Must be one of `"n3"`, `"n2"`,
#'   `"n1"`, or `"all"`.
#' @param drop_code Logical. If `TRUE` (default), removes joined BEC code
#'   columns from the result.
#'
#' @return A data frame with the same rows as `x`, plus the selected BEC name
#'   columns for the requested language relocated immediately after `ncm`. When
#'   `drop_code = FALSE`, the corresponding BEC code columns are also retained
#'   in the same block.
#'
#' @details
#' The function first joins `x` to the NCM table to recover `bec_n3_code` and
#' then joins the BEC table to retrieve the requested names. When
#' `level = "all"`, names for all available BEC levels are returned. When
#' `level` is `"n2"` or `"n1"`, intermediate `n3` columns are removed from the
#' final output. The joined BEC columns are always placed immediately after
#' `ncm`.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' df <- tibble(ncm = c("01012100", "02011000"))
#'
#' # Add BEC names at level n3 in Portuguese
#' add_bec_name(df, lang = "pt", level = "n3")
#'
#' # Add all BEC levels in English
#' add_bec_name(df, lang = "en", level = "all")
#' }
#'
#' @export
add_bec_name <- function(
    x,
    lang = c("en", "pt", "es"),
    level = c("n3", "n2", "n1", "all"),
    drop_code = TRUE
) {
  lang <- match.arg(lang)
  level <- match.arg(level)

  lang_col <- switch(
    lang,
    en = "name$",
    pt = "name_pt$",
    es = "name_es$"
  )
  name_cols <- switch(
    lang,
    en = c("bec_n3_name", "bec_n2_name", "bec_n1_name"),
    pt = c("bec_n3_name_pt", "bec_n2_name_pt", "bec_n1_name_pt"),
    es = c("bec_n3_name_es", "bec_n2_name_es", "bec_n1_name_es")
  )
  code_cols <- c("bec_n3_code", "bec_n2_code", "bec_n1_code")

  if (level == "all") {
    level_col_name <- "(n3|n2|n1)"
    regex_col_code <- paste0(
      c("n3", "n2", "n1"),
      "_code",
      collapse = "|"
    )
  } else {
    level_col_name <- level
    regex_col_code <- paste0(level, "_code")
  }

  regex_col_name <- paste0(
    "(?=.*",
    level_col_name,
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
        dplyr::matches(regex_col_name, perl = TRUE)
      ),
      by = "bec_n3_code"
    )

  if (level != "n3" & level != "all") {
    temp <- temp |>
      dplyr::select(-dplyr::matches("n3"))
  }

  for (i in seq_along(code_cols)) {
    if (all(c(code_cols[[i]], name_cols[[i]]) %in% names(temp))) {
      temp <- dplyr::relocate(
        temp,
        dplyr::all_of(name_cols[[i]]),
        .after = dplyr::all_of(code_cols[[i]])
      )
    }
  }

  if (drop_code) {
    remove_codes <- temp |>
      dplyr::select(dplyr::matches("code")) |>
      names() |>
      stringr::str_subset("country", negate = TRUE) |>
      paste0(collapse = "|")
    temp <- dplyr::select(temp, -dplyr::matches(remove_codes))
  }

  added_cols <- names(temp)
  added_cols <- added_cols[!added_cols %in% names(x)]

  temp <- dplyr::relocate(
    temp,
    dplyr::all_of(added_cols),
    .after = ncm
  )

  temp
}
