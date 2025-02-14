
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
#' @return
#' This fuction Returns a list containing the the Stock Synthesis following values:
#'  * `Nfleets` is the total number of catch fleets in the SS3 Report file
#'  * `alpha` is the alpha parameter of a Beverton Holt or Richards stock assessment function
#'  * `beta` is the beta parameter of a beverton holt or richards stock assessment function
#'  * `BH_Var` the sigmaR from the SS3 model, or the variance of the recruitment deviations
#'  * `RecruitmentObs` is a datatable containing the predicted recruitment, SSB, and recruitment deviation for each year or quarter
#'  * `MaxAge` is the Amax parameter from the SS3 model or maximum age, note for the Quarter time step this is maximum age in quarters (or `MaxAge\*4`)
#'  * `MatAtAge` is a vector containing the probability of maturity at age by year or quarter
#'  * `MatAtAgeCV` is a vector of the CV for the probability of maturity at age, note this (and all CVs) are set to 0.1 as a default but can be adjusted after running the script
#'  * `Fishery_SelAtAge` datatables of the selectivity by age and CV for each of the Nfleets
#'  * `Fishery_SelAtAgeCV` datatables of the selectivity by age and CV for each of the Nfleets
#'  * `NatMort_atAge` vectors of the natural mortality by age in years or quarters and their CV
#'  * `NatMort_atAgeCV` vectors of the natural mortality by age in years or quarters and their CV
#'  * `Jan_WAA` is the weight-at-age of the stock on Jan-1
#'  * `Jan_WAACV` CV of Weight-Of-Age of stock on Jan-1
#'  * `MidYr_WAA` is the weight-at-age of the stock on July-1
#'  * `MidYr_WAACV` CV of weight-at-age of the stock on July-1
#'  * `SSB_WAA` SSB Weight of Age
#'  * `SSB_WAACV` SSB Weight-of-Age
#'  * `SSB_WAACV` CV of SSB Weight-Of-Age
#'  * `Catage` Catch Weight-of-age by Nfleet
#'  * `CatageCV` Catch Weight-of-Age CV by Nfleet
#'  * `CatchByFleet` is the total catch by fleet in the last year/quarter of the model
#'  * `FByFleet` Fishing Mortality by Fleet
#'
#'  @author Michelle Sculley
#'  @author Eric Fletcher
#'
ss_output_export_agepro <- function(ss_objectlist, timestep = c("Year","Quarter")){


  ## TODO: Validate ss_objectlist

  #Validate timestep parameter
  timestep <- match.arg(timestep)

  ss_agepro <- list()

  # Number of Fleets
  ss_agepro[["Nfleets"]] <- length(unique(unique_selectivity_fleets(ss_objectlist)))

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
#' @param ss_agepro Stock Synthesis Object List for AGEPRO parameters
#'
#' @author Marc Nadon
#'
#'
set_parametric_recruit <- function(ss_objectlist, ss_agepro) {

  ## TODO Refactor to validation function
  # Validate ss_objectlist
  checkmate::assert_list(ss_objectlist)
  recruit_params <- c("RecruitmentObs","alpha","beta","BH_Var")
  checkmate::assert_names(names(ss_objectlist), permutation.of = recruit_params)
  checkmate::assert_list(ss_agepro)


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

#' Export Stock Synthesis Object List for AGEPRO by Year
#'
#' Exports the Stock Synthesis Object List parameters that AGEPRO uses, using
#' the Year time series.
#'
#' The function will gather the parameter and CV table for Maturity, Fishery
#' Selectivity Of Age, Weights of Age ( Jan-1, Spawning Stock
#' Biomass, Mid-Year), and Catch at Age. Each years `CatchByFleet` is used
#' to calculate the proportion of total catch.
#'
#' @details
#' For Maturity, maturity if age, starting at age 1. The length at age then
#' use maturity to give to calculate maturity at age
#' \deqn{P_{mature}(L) = \frac{1}{(1 + exp(beta*(L-L_{50})))}}
#'
#'
#'
#' @template ss_objectlist
#' @param ss_agepro Stock Synthesis Object List for AGEPRO parameters
#'
export_ss_objectlist_year <- function (ss_objectlist, ss_agepro){

  # Extract end year
  yr_end <- extract_end_year(ss_objectlist)

  #MaxAge
  ss_agepro[["MaxAge"]] <- ss_objectlist$accuage


}

export_ss_objectlist_quarter <- function(ss_objectlist, ss_agepro) {

  # Extract end year
  yr_end <- extract_end_year(ss_objectlist)

}




