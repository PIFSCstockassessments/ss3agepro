
#' Extracts end year
#'
#' Filters out the end year value based on the stock synthesis model
#'
#' @template ss_objectlist
#'
extract_end_year <- function(ss_objectlist) {

  ## TODO Basic Validation on ss_objectlist
  checkmate::assert_list(ss_objectlist)

  drvquants <- ss_objectlist$derived_quants |>
    dplyr::filter(str_detect(.data$Label,"F_")) |>
    dplyr::select("Label")

  endyr <- suppressWarnings(max( as.numeric(
    stringr::str_sub(drvquants$Label,
                     stringr::str_length(drvquants$Label)-3,
                     stringr::str_length(drvquants$Label)) ),
    na.rm = TRUE ))

  return(endyr)
}


#' Fleets with a unique length based selectivity
#'
#' (Review) For building AGEPRO input files, from Stock Synthesis length based
#' selectivity data.
#'
#' @template ss_objectlist
#'
#' @examples
#' \dontrun{
#'
#' basemodel_dir <- file.path(find.package("sso.agepro"),"01_base")
#' base_model <- r4ss::SS_output(basemodel_dir)
#'
#' # Returns Catch Fleet Numbers matching unique selectvity criteria.
#' unique_selectivity_fleets(base_model)
#'
#' # Number of Fleets
#' length(unique_selectivity_fleets(base_model))
#'
#' }
#'
#'
unique_selectivity_fleets <- function(ss_objectlist){

  #Check ss_objectlist has


  num_fleets <- ss_objectlist$Fishery_SelAtAge |>
    dplyr::filter(.data$Yr == max(ss_objectlist$FbyFleet$Yr) ) |>
    dplyr::distinct(dplyr::across(-c("Yr", "Fleet")), .keep_all = TRUE) |>
    dplyr::select("Fleet") |>
    dplyr::pull()

  return(num_fleets)
}

#' Check Stock Synthesis Numeric Version Number
#'
#' Custom checkmate function to compare Stock Synthesis Numeric Version Number.
#' Includes minimum version check.
#'
#' @param x Numeric integer representing Stock Synthesis Version, found in
#' ss_versionNumeric
#' @param min_version Minimum SS version to compare Stock Synthesis object
#' files. By default, it is 3.3.
#'
#' @export
#'
check_ss_versionNumeric <- function (x, min_version = 3.3) {

  res <- checkmate::check_numeric(x)
  if(!isTRUE(res)) {
    return(res)
  }

  if(x < min_version){
    return("SS_versionNumeric is lower than minimun version")
  }


  return(TRUE)
}


