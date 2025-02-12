
#' Extracts end year
#'
#' Filters out the end year value based on the stock synthesis model
#'
#' @param ss_objectlist Stock Synthesis list object data, primary from
#' [r4ss::SS_output()]
#'
extract_end_year <- function(ss_objectlist) {

  ## TODO Basic Validation on ss_objectlist
  checkmate::assert_list(ss_objectlist)

  drvquants <- ss_objectlist$derived_quants |>
    dplyr::filter(str_detect(.data$Label,"F_")) |>
    dplyr::select("Label")

  endyr <- max( as.numeric(
    stringr::str_sub(drvquants$Label,
                     stringr::str_length(drvquants$Label)-3,
                     stringr::str_length(drvquants$Label)) ),
    na.rm = TRUE )
}
