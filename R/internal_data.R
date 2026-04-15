#' Comex Stat's unit code and name table
#'
#' A table containing unit codes and names
#'
#' @format A tibble with 15 rows and 4 columns:
#' \describe{
#'   \item{unit_code}{NCM unit code.}
#'   \item{unit_name_pt}{Unit name in Portuguese.}
#'   \item{sg_unid}{Unit abbreviation in Portuguese.}
#'   \item{unit_name}{Unit name in English.}
#' }
#'
#' @source \url{https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_UNIDADE.csv}
#' @keywords internal
#' @name unit_table
#' @docType data
NULL

#' Comex Stat's state table
#'
#' A table containing state codes, abbreviations, names and regions
#'
#' @format A tibble with 27 rows and 4 columns:
#' \describe{
#'   \item{state_code}{State numeric code.}
#'   \item{state}{State abbreviation.}
#'   \item{state_name}{State full name in Portuguese.}
#'   \item{region_name}{Brazilian macro region name.}
#' }
#'
#' @source \url{https://balanca.economia.gov.br/balanca/bd/tabelas/UF.csv}
#' @keywords internal
#' @name state_table
#' @docType data
NULL
