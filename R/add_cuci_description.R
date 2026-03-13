#' Add CUCI (SITC) level descriptions based on Comex Stat's CUCI table (STIC table)
#'
#' This function adds textual descriptions from the CUCI (SITC) classification
#' to a dataset containing NCM codes. Only portuguese descriptions are available.
#'
#' @param x A data frame containing a column `ncm` with NCM codes.
#' @param level A string specifying the CUCI aggregation level to use.
#'   Must be one of:
#'   - `"basic_heading"`
#'   - `"subgroup"`
#'   - `"group"`
#'   - `"division"`
#'   - `"section"`
#'   - `"all"` (returns all available CUCI levels)
#'
#' @param drop_code Logical. If `TRUE` (the default), all column codes are removed
#' from output.
#'
#' @return
#' A data frame with the same rows as `x`, extended with one or more CUCI
#' description columns corresponding to the specified level(s).
#'
#' @details
#' When `level = "all"`, all CUCI description columns are returned.
#' Otherwise, only the column corresponding to the chosen level is included.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Example input tibble with NCM codes
#' df <- tibble(ncm = c("01012100", "02011000"))
#'
#' # Add CUCI descriptions at the group level
#' df_with_cuci <- add_cuci_description(df, level = "group")
#'
#' # Add CUCI descriptions at all levels
#' df_all <- add_cuci_description(df, level = "all")
#' }
#'
#' @export
add_cuci_description <- function(
    x,
    level = c("basic_heading", "subgroup", "group", "division", "section", "all"),
    drop_code = TRUE
      ) {
  level <- match.arg(level)

  level_col <- dplyr::if_else(
    level == "all",
    "basic_heading|subgroup|group|division|section",
    level
  )

  regex_col_select <- paste0(
    "(?=.*_",
    level_col,
    ")"
  )

  ncm_table <- get_ncm_table(verbose = FALSE)
  cuci_table <- get_cuci_table(verbose = FALSE)

  temp <- dplyr::left_join(
    x,
    dplyr::select(ncm_table, ncm, cuci_basic_heading_code),
    by = "ncm"
  ) |>
    dplyr::left_join(
      dplyr::select(
        cuci_table,
        cuci_basic_heading_code,
        cuci_basic_heading_desc_pt,
        dplyr::matches(regex_col_select, perl = TRUE)),
      by = "cuci_basic_heading_code"
    )

  if (level != "basic_heading" & level != "all") {
    temp <- temp |>
      select(
        -dplyr::matches("basic_heading")
      )
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
