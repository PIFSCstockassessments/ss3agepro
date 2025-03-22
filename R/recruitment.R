






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
