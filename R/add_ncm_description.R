#' Add Comex Stat's NCM descriptions
#'
#' @export
add_ncm_description <- function(
    x,
    lang = c("en","pt", "es"),
    drop_key = FALSE
) {
  lang <- match.arg(lang)

  name_col <- switch(
    lang,
    en = "ncm_description",
    pt = "ncm_description_pt",
    es = "ncm_description_es"
  )

  ncm_table <- get_ncm_table(verbose = FALSE)

  temp <- dplyr::left_join(
    x,
    dplyr::select(ncm_table, ncm, dplyr::all_of(name_col)),
    by = "ncm"
  )

  if (drop_key) {
    temp <- dplyr::select(temp, -ncm)
  }

  return(temp)

}
