#' babynamer
#'
#' Tools to access baby name data
#'
#' @author Alex Bresler (abresler@@asbcllc.com)
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
#' @import dplyr
#' @importFrom curl curl_download
#' @importFrom httr2 request req_headers req_perform resp_status resp_body_raw
#' @importFrom lubridate ymd
#' @importFrom glue glue
#' @importFrom memoise memoise
#' @importFrom purrr possibly map_dfr
#' @importFrom readr read_csv col_character col_double
#' @importFrom xml2 read_html
#' @importFrom tidyr nest
#' @importFrom rvest html_nodes html_attr
#' @importFrom stringr str_to_upper str_to_lower str_c str_detect str_remove_all
#' @importFrom tibble tibble

NULL
