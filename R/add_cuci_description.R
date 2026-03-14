#' Add CUCI descriptions from Comex Stat
#'
#' Uses the NCM and CUCI correlation tables to map each `ncm` in `x` to its
#' CUCI classification and append Portuguese description columns for the
#' requested aggregation level.
#'
#' @param x A data frame containing an `ncm` column.
#' @param level CUCI aggregation level to return. Must be one of
#'   `"basic_heading"`, `"subgroup"`, `"group"`, `"division"`, `"section"`,
#'   or `"all"`.
#' @param drop_code Logical. If `TRUE` (default), removes joined CUCI code
#'   columns from the result.
#'
#' @return A data frame with the same rows as `x`, plus the selected CUCI
#'   description columns. When `drop_code = FALSE`, the corresponding CUCI code
#'   columns are also retained.
#'
#' @details
#' Only Portuguese CUCI descriptions are available in the source table. The
#' function first joins `x` to the NCM table to recover
#' `cuci_basic_heading_code`, then joins the CUCI table. When `level = "all"`,
#' all CUCI description levels are returned. When `level` is not
#' `"basic_heading"` or `"all"`, intermediate `basic_heading` columns are
#' removed from the final output.
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
      dplyr::select(
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
