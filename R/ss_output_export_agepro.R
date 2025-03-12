
#' @title
#' Export Stock Synthesis Model data for AGEPRO Input Files
#'
#' @description
#' Function to extract input data needed from the SS model to use in an agepro projection model
#'
#' @details
#' Units: SSB and catch are in metric tons of biomass, recruitment is in 1000s of fish,
#' catch/weight at ages are all in kilograms, alpha and beta parameters are converted from metric tons/1000s
#' of fish to kilograms and numbers of fish by multiplying by 1000
#'
#' @param ss_objectlist Stock Synthesis list object data, primary from
#' [r4ss::SS_output()]
#' @param timestep "Year" or "Quarter": Indicates is you are running AGEPRO
#' with a yearly time step or as quarters as years. "Year" as Default.
#'
#' @template return_list_ss_agepro
#'
#' @author Michelle Sculley
#' @author Eric Fletcher
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  # Setup For Bootstrap File and Directory
#'  basemodel_dir <- file.path(find.package("sso.agepro"),"01_base")
#'  bootstrap_dir <- file.path(tempdir())
#'  setup_ss_bootstrap(basemodel_dir, bootstrap_dir, n_boot = 10)
#'
#'  # Call r4ss::SS_output to extract from Report.sso
#'  ss_objectlist <- r4ss::SS_output(dir = bootstrap_dir, verbose = FALSE, printstats = FALSE)
#'
#'  ss_agepro <- ss_output_export_agepro(ss_objectlist)
#'
#'  #Filter For unique selectivity
#'
#' }
#'
ss_output_export_agepro <- function(ss_objectlist, timestep = c("Year","Quarter")){

  #Validate timestep parameter
  timestep <- match.arg(timestep)

  ## Validation: Check for ss_objectlist, and version (SS_versionNumeric)
  checkmate::assert(checkmate::check_list(ss_objectlist),
                    check_ss_versionNumeric(ss_objectlist$SSversionNumeric))

  ss_agepro <- list()

  # Number of Fleets
  ss_agepro[["Nfleets"]] <- length(which(ss_objectlist[["fleet_type"]] == 1)) #length(unique(unique_selectivity_fleets(ss_objectlist)))

  ## TODO: Make this flexable enough for AGEPRO Recruitment
  # Set RECRUIT values
  set_parametric_recruit(ss_objectlist, ss_agepro)

  #indicate if you are doing yearly or years as quarters
  switch(timestep,
         "Year" = export_ss_objectlist_year(ss_objectlist,
                                            ss_agepro),
         "Quarter" = export_ss_objectlist_quarter(ss_objectlist,
                                                  ss_agepro))

  return(ss_agepro)
}

#' Set Parametric Recruitment values with Stock Synthesis Data
#'
#' Exports Stock Synthesis Output data for parametric recruitment (For Example,
#' Beverton-Holt or Ricker curve) values
#'
#' @details
#' If using a Beverton-Holt recruitment (options 5 or 10), you need alpha,
#' beta, and variance note that $B_0$ is kilograms of biomass and $R_0$ is
#' numbers of fish
#'
#' This specification accounts for AGEPRO's Single Sex projections. Beta is
#' multiplied by 2, because Stock Synthesis Spawning Stock Biomass (SSB)
#' accounts Female-only biomass, but AGEPRO stock SSB is in total biomass.
#'
#' @template ss_objectlist
#' @template ss_agepro
#'
#' @author Marc Nadon
#'
#'
set_parametric_recruit <- function(ss_objectlist, ss_agepro) {

  # Ensure ss_objectlist has the right parameters to select
  objectlist_params <- c("recruit","parameters","derived_quants")
  # Validate ss_objectlist
  checkmate::assert(
    checkmate::check_list(ss_objectlist),
    checkmate::check_names(names(ss_objectlist), must.include = objectlist_params),
    checkmate::check_list(ss_agepro)
  )


  ss_agepro[["RecruitmentObs"]] <- ss_objectlist$recruit[,c("Yr","SpawnBio","pred_recr","dev")]

  Steepness <- ss_objectlist$parameters[which(ss_objectlist$parameters$Label=="SR_BH_steep"),"Value"]

  # alpha
  R0        <- ss_objectlist$derived_quants[which(ss_objectlist$derived_quants$Label=="Recr_Virgin"),"Value"]
  ss_agepro[["alpha"]] <- 4*Steepness*R0/(5*Steepness-1)
  # aeta
  # IMPORTANT: the reason for beta x 2 is that Agepro SSB is in TOTAL biomass, not just FEMALE biomass, like SSB.
  SSB0      <- ss_objectlist$derived_quants[which(ss_objectlist$derived_quants$Label=="SSB_Virgin"),"Value"]
  ss_agepro[["beta"]]  <- SSB0*(1-Steepness)/(5*Steepness-1) * 2

  #variance is sigmaR
  # IMPORTANT: need to square since Agepro takes Variance as a parameter
  ss_agepro[["BH_var"]] <- ss_objectlist$parameters[which(ss_objectlist$parameters$Label=="SR_sigmaR"),"Value"]^2

}



#' Get a parameter and its values from the Stock Synthesis Object List
#'
#' Convenience function to get parameter values from the Stock Synthesis Object
#' List.
#'
#' @template ss_objectlist
#' @param ss_label Name of the ss_objectlist parameter to extract values from.
#'
#' @export
#'
get_ss_objectlist_parameter <- function(ss_objectlist, ss_label){
  return(ss_objectlist$parameters[which(ss_objectlist$parameters$Label == ss_label), "Value"])
}


#' Gets growth information for weight of Age parameter values.
#'
#' Convenience function to get Growth Information  from
#' the Stock Synthesis Object List to calculate Weight of Age. Method will
#' vary depending on timestep used.
#'
#'
#' @template ss_objectlist
#' @param timestep "Year" or "Quarter": Indicates is you are running AGEPRO
#' with a yearly time step or as quarters as years. "Year" as Default.
#'
#'
get_WAA_growth <- function(ss_objectlist,
                              timestep = c("Year","Quarter")){

  # Validate timestep
  timestep <- match.arg(timestep)

  # Validate ss_objectlist

  # Extract end year
  yr_end <- extract_end_year(ss_objectlist)

  if(timestep == "Year") {
    return(
      ss_objectlist$growthseries |>
        dplyr::filter(
          .data$Yr == yr_end,
          .data$Seas == 1,
          .data$SubSeas == 1
          ) |>
        dplyr::select(6:ncol(ss_objectlist$growthseries)) |>
        unlist()
      )
  }else if(timestep == "Quarter"){
    return(
      ss_objectlist$growthseries |>
        dplyr::filter(.data$Yr == yr_end, .data$SubSeas == 1) |>
        dplyr::select(6:ncol(ss_objectlist$growthseries)) |>
        data.table::melt() |>
        dplyr::select(2) |>
        as.vector()
    )
  }else{
    stop("Invalid Operation")
  }

}


#' Get Timeseries Parameter Value
#'
#' Returns a matrix showing values for Stock Synthesis timeseries parameter.
#' The function will return values that matches the column name, or column name
#' prefixes for fleet-related columns, and return the target parameter
#' included in the timeseries of the input stock synthesis object list.
#'
#' AGEPRO models timesteps are run by year. To facilitate models that run per
#' quarterly time step, setting `timestep` parameter to `Quarter` will
#' set quarters as years.
#'
#'
#' @template ss_objectlist
#' @param colname_param Character string to select to target parameter with for each fleet
#' @param timestep "Year" or "Quarter": Indicates is you are running AGEPRO
#' with a yearly time step or as quarters as years. "Year" as Default.
#'
#' @keywords Internal
#'
get_timeseries_param <- function(ss_objectlist,
                               colname_param,
                               timestep = c("Year","Quarter")) {

  # Validate ss_objectlist
  checkmate::assert_list(ss_objectlist)
  checkmate::assert_names(names(ss_objectlist), must.include = "timeseries")
  checkmate::assert_names(names(ss_objectlist$timeseries), type = "unique")

  timestep <- match.arg(timestep)

  # TODO: colname_param validation
  # TODO: Determine to check target Fleet-Specfic Columns or not
  checkmate::assert_character(colname_param)

  # Extract end year
  yr_end <- extract_end_year(ss_objectlist)

  if(timestep == "Year") {
    return(ss_objectlist$timeseries |>
             dplyr::filter(.data$Yr <= yr_end) |>
             dplyr::select("Yr",dplyr::starts_with(colname_param)) |>
             dplyr::group_by(.data$Yr) |>
             dplyr::summarize_all(sum))
  }else if (timestep == "Quarter") {
    return(ss_objectlist$timeseries |>
        dplyr::filter(.data$Yr <= yr_end) |>
        dplyr::select(dplyr::starts_with(colname_param)) |>
        data.table::melt(id.vars = c("Yr","Seas")) |>
        data.table::dcast(.data$Yr ~ .data$variable + .data$Seas))
  }else{
    stop("Invalid Operation")
  }

}

#' Process Error parameter's default Coefficient of Variation
#'
#' Returns the vector of Coefficient of Variation (CV) values. The length of
#' the vector is determined by max_age.
#'
#' @param max_age Max Age. Determines the length of vector.
#' @param value Default value for CV.
#'
#' @examples
#' \dontrun{
#'   get_cv_process_error(10)
#' }
#'
default_cv_process_error <- function(max_age, value = 0.1) {
  return(rep(value,max_age))
}


#' Default Coefficient of Variation for process error parameters with fleets
#'
#' Returns a matrix containing a default Coefficient of Variation (CV) value.
#' The dimensions of the matrix is determined by max_age over the number of
#' fleets.
#'
#' @inheritParams default_cv_process_error
#' @param num_fleets Number of Fleets. For
#'
#' @examples
#' \dontrun{
#'   get_cv_fleets_process_error(10, 5, 0.1)
#' }
#'
#'
default_cv_fleets_process_error <- function(max_age,
                                            num_fleets,
                                            value = 0.1) {

  return(matrix(value,
                nrow = num_fleets,
                ncol = max_age))


}

#' Export Stock Synthesis Object List for AGEPRO
#'
#' `export_ss_objectlist_year` exports the Stock Synthesis object
#' list parameters that AGEPRO uses, using the Year time series.
#' `export_ss_objectlist_quarter` will do the same, but in quarterly
#' time steps.
#'
#' The function will gather the parameter and CV table for Maturity, Fishery
#' Selectivity Of Age, Weights of Age (Jan-1, Spawning Stock
#' Biomass, Mid-Year), and Catch at Age. Each years `CatchByFleet` is used
#' to calculate the proportion of total catch.
#'
#' @details
#' For Maturity, maturity if age, starting at age 1. The length at age then
#' use maturity to give to calculate maturity at age
#' \deqn{P_{mature}(L) = \frac{1}{(1 + exp(beta*(L-L_{50})))}}
#'
#' @template return_list_ss_agepro
#'
#' @template ss_objectlist
#' @param ss_agepro Stock Synthesis Object List for AGEPRO parameters
#'
export_ss_objectlist_year <- function (ss_objectlist, ss_agepro){

  # Extract end year
  yr_end <- extract_end_year(ss_objectlist)

  # MaxAge
  ss_agepro[["MaxAge"]] <- ss_objectlist$accuage

  # Maturity

  Mat_Slope <- get_ss_objectlist_parameter(ss_objectlist, "Mat_slope_Fem_GP_1")
  Mat_50 <- get_ss_objectlist_parameter(ss_objectlist, "Mat50%_Fem_GP_1")

  which_Latage <-
    which(
      ss_objectlist$growthseries$Yr == yr_end &
        ss_objectlist$growthseries$Seas == 1 &
        ss_objectlist$growthseries$SubSeas == 1
    )

  LatAge <- ss_objectlist$growthseries[which_Latage, 6:ncol(ss_objectlist$growthseries)]

  ss_agepro[["MatAtAge"]] <- 1 / (1 + exp(Mat_Slope*(LatAge-Mat_50)))
  ss_agepro[["MatAtAgeCV"]] <- rep(0.01, ss_agepro$MaxAge)


  ## Fishery Selectivity at age

  ss_agepro[["Fishery_SelAtAge"]] <- ss_objectlist$ageselex |>
    dplyr::filter(.data$Factor == "Asel2",
                  .data$Yr <= yr_end,
                  .data$Seas == 1,
                  .data$Fleet <= ss_agepro$Nfleets) |>
    dplyr::select("Yr","Fleet",9:ncol(ss_objectlist$ageselex))
                #select("Yr","Fleet",9:ncol(.))


  ##Fishery_seleatage coefficient of variation, set to a standard 0.1
  ss_agepro[["Fishery_SelAtAgeCV"]] <- matrix(0.1,
                                              nrow=ss_agepro$Nfleets,
                                              ncol=ss_agepro$MaxAge)

  # Natural Mortality

  ss_agepro[["NatMort_atAge"]] <- ss_objectlist$Natural_Mortality |>
    dplyr::slice(1) |>
    dplyr::select(6:ncol(ss_objectlist$Natural_Mortality))

  ss_agepro[["NatMort_atAgeCV"]] <- rep(0.01,ss_agepro$MaxAge)


  # JAN-1

  ss_agepro[["Jan_WAA"]] <- get_WAA_growth(ss_objectlist, "Year")
  ss_agepro[["Jan_WAA"]] <-
    get_ss_objectlist_parameter(ss_objectlist, "Wtlen_1_Fem_GP_1") *
    (ss_agepro[["Jan_WAA"]] ^
       get_ss_objectlist_parameter(ss_objectlist, "Wtlen_2_Fem_GP_1"))
  ss_agepro[["Jan_WAACV"]] <- default_cv_process_error(ss_agepro$MaxAge,
                                                       value = 0.1)

  # Mid-Year

  ss_agepro[["MidYr_WAA"]] <- get_WAA_growth(ss_objectlist, "Year")
  ss_agepro[["MidYr_WAA"]] <-
    get_ss_objectlist_parameter(ss_objectlist, "wtlen_1_Fem_GP_1") *
    (ss_agepro[["Midyr_WAA"]] ^
       get_ss_objectlist_parameter(ss_objectlist, "Wtlen_2_Fem_GP_1"))
  ss_agepro[["SSB_WAACV"]] <- default_cv_process_error(ss_agepro$MaxAge,
                                                       value = 0.1)

  #SSB

  ss_agepro[["SSB_WAA"]] <- get_WAA_growth(ss_objectlist, "Year")
  ss_agepro[["SSB_WAA"]] <-
    get_ss_objectlist_parameter(ss_objectlist, "Wtlen_1_Fem_GP_1") *
    (ss_agepro[["SSB_WAA"]] ^
       get_ss_objectlist_parameter(ss_objectlist,"Wtlen_2_Fem_GP_1"))
  ss_agepro[["SSB_WAACV"]] <- default_cv_process_error(ss_agepro$MaxAge,
                                                       value = 0.1)

  ss_agepro[["CatchbyFleet"]] <-
    get_timeseries_param(ss_objectlist, "sel(B):_")

  ss_agepro[["FbyFleet"]] <- get_timeseries_param(ss_objectlist, "F:_")


  return(ss_agepro)

}

#' @rdname export_ss_objectlist_year
#'
export_ss_objectlist_quarter <- function(ss_objectlist, ss_agepro) {

  # Extract end year
  yr_end <- extract_end_year(ss_objectlist)

  # Maturity
  ss_agepro[["MaxAge"]] <- ss_objectlist$accuage*4

  # Maturity at age, from age 1
  # Length at age then use maturity to give to calculate maturity at age
  ## Pmature(L) = 1 / (1 + exp(beta*(L-L50)))

  mat_beta <- get_ss_objectlist_parameter(ss_objectlist, "Mat_slope_Fem_GP_1")
  mat_L <- seq(1,ss_agepro[["MaxAge"]]/4,0.25)
  mat_L50 <- get_ss_objectlist_parameter(ss_objectlist, "Mat50%_Fem_GP_1")

  ss_agepro[["MatAtAge"]] <- 1 / ( 1 + exp( mat_beta * (mat_L - mat_L50)))
  ss_agepro[["MatAtAgeCV"]] <- rep(0.01, ss_agepro[["MaxAge"]])


  ## Fishery Selectivity at age

  ss_agepro[["Fishery_SelAtAge"]] <- ss_objectlist$ageselex |>
    dplyr::filter(.data$Factor == "Asel2",
                  .data$Yr <= yr_end,
                  .data$Fleet <= ss_agepro[["Nfleets"]]) |>
    dplyr::select("Yr","Fleet","Seas",9:ncol(ss_objectlist$ageselex))

  ss_agepro[["Fishery_SelAtAgeCV"]] <- matrix(0.1,
                                              nrow = ss_agepro[["Nfleets"]],
                                              ncol = ss_agepro[["MaxAge"]])


  ss_agepro[["NatMort_atAge"]] <-
    ss_objectlist$Natural_Mortality |>
    dplyr::select(6:ncol(ss_objectlist$Natural_Mortality)) |>
    data.table::melt() |>
    dplyr::select(.data$value)

  ss_agepro[["NatMort_atAgeCV"]] <- rep(0.1, ss_agepro[["MaxAge"]])


  ## Weights of Age

  # JAN-1

  ss_agepro[["Jan_WAA"]] <- get_WAA_growth(ss_objectlist,
                                           timestep = "Quarter")

  ss_agepro[["Jan_WAACV"]] <- rep(0.1, ss_agepro[["MaxAge"]])




  return(ss_agepro)
}




