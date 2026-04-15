#' Add CUCI names from Comex Stat
#'
#' Uses the NCM and CUCI correlation tables to map each `ncm` in `x` to its
#' CUCI classification and append Portuguese name columns for the requested
#' aggregation level.
#'
#' @param x A data frame containing an `ncm` column.
#' @param level CUCI aggregation level to return. Must be one of
#'   `"basic_heading"`, `"subgroup"`, `"group"`, `"division"`, `"section"`,
#'   or `"all"`.
#' @param drop_code Logical. If `TRUE` (default), removes joined CUCI code
#'   columns from the result.
#'
#' @return A data frame with the same rows as `x`, plus the selected CUCI name
#'   columns relocated immediately after `ncm`. When `drop_code = FALSE`, the
#'   corresponding CUCI code columns are also retained in the same block.
#'
#' @details
#' Only Portuguese CUCI names are available in the source table. The function
#' first joins `x` to the NCM table to recover `cuci_basic_heading_code`, then
#' joins the CUCI table. When `level = "all"`, all CUCI name levels are
#' returned. When `level` is not `"basic_heading"` or `"all"`, intermediate
#' `basic_heading` columns are removed from the final output. The joined CUCI
#' columns are always placed immediately after `ncm`.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Example input tibble with NCM codes
#' df <- tibble(ncm = c("01012100", "02011000"))
#'
#' # Add CUCI names at the group level
#' df_with_cuci <- add_cuci_name(df, level = "group")
#'
#' # Add CUCI names at all levels
#' df_all <- add_cuci_name(df, level = "all")
#' }
#'
#' @export
add_cuci_name <- function(
    x,
    level = c("basic_heading", "subgroup", "group", "division", "section", "all"),
    drop_code = TRUE
      ) {
  level <- match.arg(level)
  code_cols <- c(
    "cuci_basic_heading_code",
    "cuci_subgroup_code",
    "cuci_group_code",
    "cuci_division_code",
    "cuci_section_code"
  )
  name_cols <- c(
    "cuci_basic_heading_name_pt",
    "cuci_subgroup_name_pt",
    "cuci_group_name_pt",
    "cuci_division_name_pt",
    "cuci_section_name_pt"
  )

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
        cuci_basic_heading_name_pt,
        dplyr::matches(regex_col_select, perl = TRUE)),
      by = "cuci_basic_heading_code"
    )

  if (level != "basic_heading" & level != "all") {
    temp <- temp |>
      dplyr::select(
        -dplyr::matches("basic_heading")
      )
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
