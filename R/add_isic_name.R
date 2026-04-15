#' Add ISIC names from Comex Stat
#'
#' Uses the NCM and ISIC correlation tables to map each `ncm` in `x` to its
#' ISIC classification and append name columns for the requested aggregation
#' level.
#'
#' @param x A data frame containing an `ncm` column.
#' @param lang Language of the appended ISIC names. Must be one of `"en"`,
#'   `"pt"`, or `"es"`.
#' @param level ISIC aggregation level to return. Must be one of `"class"`,
#'   `"group"`, `"division"`, `"section"`, or `"all"`.
#' @param drop_code Logical. If `TRUE` (default), removes joined ISIC code
#'   columns from the result.
#'
#' @return A data frame with the same rows as `x`, plus the selected ISIC name
#'   columns for the requested language relocated immediately after `ncm`. When
#'   `drop_code = FALSE`, the corresponding ISIC code columns are also retained
#'   in the same block.
#'
#' @details
#' The function first joins `x` to the NCM table to recover `isic_class_code`
#' and then joins the ISIC table to retrieve the requested names. When
#' `level = "all"`, names for all available ISIC levels are returned. When
#' `level` is not `"class"` or `"all"`, intermediate `class` columns are
#' removed from the final output. The joined ISIC columns are always placed
#' immediately after `ncm`.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(ncm = c("01012100", "02011000"))
#' add_isic_name(df, lang = "en", level = "division")
#' }
#'
#' @export
add_isic_name <- function(
    x,
    lang = c("en","pt", "es"),
    level = c("class", "group", "division", "section", "all"),
    drop_code = TRUE
){
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
    en = c(
      "isic_class_name",
      "isic_group_name",
      "isic_division_name",
      "isic_section_name"
    ),
    pt = c(
      "isic_class_name_pt",
      "isic_group_name_pt",
      "isic_division_name_pt",
      "isic_section_name_pt"
    ),
    es = c(
      "isic_class_name_es",
      "isic_group_name_es",
      "isic_division_name_es",
      "isic_section_name_es"
    )
  )
  code_cols <- c(
    "isic_class_code",
    "isic_group_code",
    "isic_division_code",
    "isic_section_code"
  )

  if (level == "all") {
    level_col_name <- "(class|group|division|section)"
    regex_col_code <- paste0(
      c("class", "group", "division", "section"),
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
  isic_table <- get_isic_table(verbose = FALSE)

  temp <- dplyr::left_join(
    x,
    dplyr::select(ncm_table, ncm, isic_class_code),
    by = "ncm"
  ) |>
    dplyr::left_join(
      dplyr::select(
        isic_table,
        isic_class_code,
        dplyr::matches(regex_col_code, perl = TRUE),
        dplyr::matches(regex_col_name, perl = TRUE)
      ),
      by = "isic_class_code"
    )

  if (level != "class" & level != "all") {
    temp <- temp |>
      dplyr::select(-dplyr::matches("class"))
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

  return(temp)
}
