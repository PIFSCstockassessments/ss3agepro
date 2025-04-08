

#' AGEPRO Model class creations from ss_agepro objectlist
#'
#' Conveince function to setup AGEPRO model class with AGEPRO input model
#' file functions.
#'
#' @template ss_agepro
#' @param num_years Number of Years
#' @param num_pop_sims Number of population simulations
#' @param recruit_models Vector of recruitment model numbers
#' @param recruit_model_prob Recruitment proabbility
#' @param bsn_file Bootstrap file
#' @param num_boot Number of bootstraps
#' @param model_name Model name
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' #Continuing from ss_output_export_agepro example
#' bsn_file <- file.path(bootstrap_dir,"bootstrap.bsn")
#'
#' inp_file <- setup_agepro_inpfile(ss_agepro, num_years = 8, num_pop_sims = 1000, recruit_models = c(5,3), recruit_model_prob = c(0.6, 0.4), bsn_file)
#'
#' }
#'
#'
setup_agepro_inpfile <- function(ss_agepro,
                                 num_years,
                                 num_pop_sims,
                                 recruit_models,
                                 recruit_model_prob,
                                 bsn_file,
                                 num_boot = 100,
                                 model_name = "untitled"){

  # TODO: Check if recruit_models is a single int, multile int, and valid recruit_model
  # TODO: Check bsn file path


  inp_model <- suppressWarnings(ageproR::agepro_inp_model$new(yr_start = 1,
                                             yr_end = num_years,
                                             age_begin = 1,
                                             age_end = ss_agepro[["MaxAge"]],
                                             num_pop_sims = num_pop_sims,
                                             num_fleets = ss_agepro[["Nfleets"]],
                                             num_rec_models = length(recruit_models),
                                             enable_cat_print = FALSE))

  # Case ID (Model Name)
  inp_model$case_id$model_name <- model_name

  # BOOTSTRAP
  set_inp_model_bootstrap(inp_model, bsn_file, num_boot, 1000)

  #RECRUIT
  suppressMessages(inp_model$set_recruit_model(recruit_models))
  set_inp_model_recruit_prob(inp_model, recruit_model_prob)

  set_inp_model_recruit_data(inp_model, ss_agepro)

  # AGEPRO TODO: recruit_scaling_factor, ssb_scaling_factor, max_recruit_obs is read only
  # General recruitment parameters used for all models
  # inp_model$recruit$recruit_scaling_factor <- 1000
  # inp_model$recruit$ssb_scaling_factor <- 1
  # inp_model$recruit$max_recruit_obs <- 100


  return(invisible(inp_model))
}



set_inp_model_bootstrap <- function(inp_model,
                                bsn_file,
                                num_boot = 0,
                                boot_factor = 1000) {

  inp_model$set_bootstrap_filename(bsn_file)
  inp_model$bootstrap$num_bootstraps <- num_boot
  inp_model$bootstrap$pop_scale_factor <- boot_factor
}



