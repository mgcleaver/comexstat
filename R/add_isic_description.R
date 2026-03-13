#' Add ISIC level descriptions based on Comex Stat's ISIC table
#'
#' @export
add_isic_description <- function(
    x,
    lang = c("en","pt", "es"),
    level = c("class", "group", "division", "section", "all"),
    drop_code = TRUE
){
  lang <- match.arg(lang)
  level <- match.arg(level)

  lang_col <- switch(
    lang,
    en = "desc$",
    pt = "desc_pt$",
    es = "desc_es$"
  )

  if (level == "all") {
    level_col_desc <- "(class|group|division|section)"
    regex_col_code <- paste0(
      c("class", "group", "division", "section"),
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
        dplyr::matches(regex_col_desc, perl = TRUE)
      ),
      by = "isic_class_code"
    )

  if (level != "class" & level != "all") {
    temp <- temp |>
      dplyr::select(-dplyr::matches("class"))
  }

  if (drop_code) {
    remove_codes <- temp |>
      dplyr::select(dplyr::matches("code")) |>
      names() |>
      stringr::str_subset("country", negate = TRUE) |>
      paste0(collapse = "|")
    temp <- dplyr::select(temp, -dplyr::matches(remove_codes))
  }

  return(temp)
}
