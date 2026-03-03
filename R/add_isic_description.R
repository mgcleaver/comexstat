#' Add ISIC level descriptions based on Comex Stat's ISIC table
#'
#' @export
add_isic_description <- function(
    x,
    lang = c("en","pt", "es"),
    level = c("class", "group", "division", "section", "all"),
    drop_key = TRUE
){
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
    "class|group|division|section",
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
        dplyr::matches(regex_col_select, perl = TRUE)),
      by = "isic_class_code"
    )

  if (drop_key) {
    temp <- dplyr::select(temp, -isic_class_code)
  }

  return(temp)
}
