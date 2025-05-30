



#' Sets up AGEPRO Recruitment Probability values
#'
#' Convenience function to set the recruitment probability of a agepro model.
#'
#' @param inp_model AGEPRO model R6class from agerpoR
#' @param recruit_model_prob The Recruitment Probabilities for each recruitment
#' model.
#'
#'
set_inp_model_recruit_prob <- function(inp_model,
                                       recruit_model_prob) {

  checkmate::check_r6(inp_model, public = "recruit")

  # TODO: Check that length recruit_model_prob matches number of recruitment
  # models

  # ageproR TODO: Implement set_recruitment_probability
  new_recruit_prob_value <-
    lapply(recruit_model_prob, rep, each=inp_model$general$num_years)

  # agepro TODO: Implement for set_recruitment_probablity
  for(recruit in 1:inp_model$recruit$number_recruit_models) {
    names(new_recruit_prob_value[[recruit]]) <- inp_model$general$seq_years
  }
  inp_model$recruit$recruit_probability <- new_recruit_prob_value

  rm(new_recruit_prob_value)

}


#' Use Stock Synthesis data to Set Empirical Recruitment Distribution.
#'
#' Helper function to set Empirical Recruitment Distribution data (Model #3)
#' from stock synthesis report objectlist data.
#'
#' @param inp_model AGEPRO model
#' @param recobs Recruitment Observations data frame
#' @param irec Index of (Multi) recruitment Model
#'
set_empirical_distribution_data <- function (inp_model, recobs, irec = 1) {

  # TODO: Validate for observed_points, observations
  checkmate::assert(
    checkmate::check_data_frame(recobs, ncols = 1),
    checkmate::check_names(names(recobs), permutation.of = "recruit"))

  # ageproR: Recruitment observations is stored as a matrix
  recobs_matrix <- as.matrix(recobs)
  # AGEPRO: Number of recruitment data points: T
  inp_model$recruit$recruit_data[[irec]][["observed_points"]] <-
    length(recobs_matrix)
  # AGEPRO: Recruitment: R_1 ... R_T
  inp_model$recruit$recruit_data[[irec]][["observations"]] <-
    recobs_matrix

}


#' Subset empirical recruitment observation table
#'
#' Helper method to extract the empirical recruitment observation table from
#' the Stock synthesis report objectlist.
#'
#' @template ss_agepro
#' @param start_yr First year of time period to filter "Yr" field. If NULL,
#' value will default to input stock synthesis objectlist `StartYr`
#' @param end_yr Last Year of time period to filter "Yr" field.  If NULL,
#' value will default to input stock synthesis objectlist `EndYr`
#'
#'
subset_empirical_recobs <- function(ss_agepro, start_yr = NULL, end_yr = NULL) {

  #If start_yr and end_yr are not
  if(missing(start_yr)){
    checkmate::assert_number(ss_agepro[["StartYr"]],lower = 0)
    start_yr <- ss_agepro[["StartYr"]]
  }

  if(missing(end_yr)){
    checkmate::assert_number(ss_agepro[["EndYr"]], lower = 0)
    end_yr <- ss_agepro[["EndYr"]]
  }

  recobs <- ss_agepro[["RecruitmentObs"]] |>
    dplyr::filter(.data$Yr >= start_yr & .data$Yr <= end_yr) |>
    subset(select = "pred_recr")

  colnames(recobs) <- "recruit"

  return(recobs)
}

#' Use Stock Synthesis Object data to create AGEPRO model recruit data.
#'
#' Helper function to set AGEPRO model recruitment model data.
#'
#' @param inp_model AGEPRO model
#' @template ss_agepro
#'
set_inp_model_recruit_data <- function(inp_model, ss_agepro) { # NOTE FROM Marc N.: The general multi-models structure is there, but have only fixed model 3 and 5.

  # TODO: Validate Recruitment Model Number list
  checkmate::assert(checkmate::check_numeric(inp_model$recruit$number_recruit_models, len = 1, lower = 1))

  recruit_models <- inp_model$recruit$recruit_model_num_list


  # Recruitment model(s) parameters
  for(irec in 1:inp_model$recruit$number_recruit_models){

    #aRecMod  <- Recruitment$rec_models[[rm]]
    #aRecPars <- Recruitment$rec_pars[[rm]] # Load the parameters for the model in the loop

    rec_model <- inp_model$recruit$recruit_model_num_list[[irec]]

    if (rec_model %in% c(1, 4, 9, 15)) {
      unsupported_model()
    }

    if (rec_model %in% c(2)) {
      # ageproR TODO: Implement model #2
      #   lines[[paste0("Rmod",rm,"a")]]<-paste(aRecPars$Nobs)
      #   lines[[paste0("Rmod",rm,"b")]]<-paste(aRecPars$Recruits$pred_recr, collapse = "  ")
      #   lines[[paste0("Rmod",rm,"c")]]<-paste(aRecPars$Recruits$SpawnBio, collapse = "  ")
      # a <- ss_agepro[["Nobs"]]
      # b <- subset_empirical_recobs(ss_agepro)
      # c <- ss_agepro[["SpawnBio"]]
      not_implmented_ageproR()
    }

    if (rec_model %in% c(3)) {

      recobs <- subset_empirical_recobs(ss_agepro)
      set_empirical_distribution_data(inp_model, recobs, irec)
    }

    if(rec_model %in% c(5, 6, 7, 10, 11)) {

      inp_model$recruit$recruit_data[[irec]][["alpha"]] <- ss_agepro[["alpha"]]
      inp_model$recruit$recruit_data[[irec]][["beta"]] <- ss_agepro[["beta"]]
      inp_model$recruit$recruit_data[[irec]][["variance"]] <- ss_agepro[["BH_var"]]

    }
    if(rec_model %in% c(10,11)) {
      #lines[[paste0("Rmod",rm,"b")]]<-paste(aRecPars$Phi, aRecPars$LastResid, collapse = "  ")
      not_implmented_ageproR()
    }


    if (rec_model %in% c(7, 12)) {
      # TODO: implement Kparm on ss_agepro
      inp_model$recruit$recruit_data[[irec]][["kpar"]] <- ss_agepro[["Kparm"]]
    }
    if (rec_model == 12) {
      # lines[[paste0("Rmod",rm,"b")]]<-paste(aRecPars$Phi, aRecPars$LastResid, collapse = "  ")
      not_implmented_ageproR()
    }



    #
    # if (aRecMod %in% c(8, 13)) {
    #   lines[[paste0("Rmod",rm,"a")]]<-paste(aRecPars$mean, aRecPars$stdev, collapse = "  ")
    #   if (aRecMod == 13) {
    #     lines[[paste0("Rmod",rm,"b")]]<-paste(aRecPars$Phi, aRecPars$LastResid, collapse = "  ")
    #   }
    # }
    #
    # if (aRecMod == 14) {
    #   lines[[paste0("Rmod",rm,"a")]]<-paste(aRecPars$Nobs)
    #   lines$Rmod2<-paste(aRecPars$Recruits$pred_recr, collapse = "  ")
    # }
    #
    # if (aRecMod %in% c(16, 17, 18, 19)) {
    #   lines[[paste0("Rmod",rm,"a")]]<-paste(aRecPars$Ncoeff, aRecPars$var, aRecPars$Intercept, collapse = "  ")
    #   lines[[paste0("Rmod",rm,"b")]]<-paste(aRecPars$Coeff, collapse = "  ")
    #   lines[[paste0("Rmod",rm,"c")]]<-paste(aRecPars$Observations, collapse = "  ")
    # }
    #
    # if (aRecMod == 20) {
    #   lines[[paste0("Rmod",rm,"a")]]<-paste(aRecPars$Data, collapse = "  ")
    # }
    #
    # if (aRecMod == 21) {
    #   lines[[paste0("Rmod",rm,"a")]]<-paste(aRecPars$Nobs)
    #   lines[[paste0("Rmod",rm,"b")]]<-paste(aRecPars$Obs, collapse = "  ")
    #   lines[[paste0("Rmod",rm,"c")]]<-paste(aRecPars$SSBHingeValue, collapse = "  ")
    # }

  } # End of Recruitment models for loop
}


validate_model_num_list <- function(x) {

  # Catch "Empty" argument
  if(isTRUE(all.equal(length(x), 0))){
    return(paste0("No recruitment model numbers passed"))
  }

  # Catch Multiple parameters and return validation message
  if(!isTRUE(all.equal(length(x),1))){
    return(paste0("Multiple parameters detected, ",
                  "please pass multiple recruitment models as a single vector"))
  }

  return(TRUE)

}
