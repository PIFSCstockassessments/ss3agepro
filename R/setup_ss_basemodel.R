

#' Creates a Stock Synthesis base model and bootstrap runs
#'
#' Sets up a Stock Synthesis Base model including individual bootstrap runs.
#' Function includes helper functions to to help with running SS in parallel
#'
#' @param n_boot Number of Bootstraps
#' @param seed Pusedorandom Number Seed
#' @param basemodel_dir where the base model SS files are located
#' @param bootstrap_outdir Path where the bootstrap file and bootstrap runs will be saved
#' @param ss3_path File path of the Stock Synthesis Binary
#' @param ss3_exe Executable name. Can be just the name of the executable file if it is in the specified directory or in the user's PATH. Can also include the absolute path or a path relative to the specified directory. Needs to be a single character string, not a vector. On Windows, exe can optionally have the .exe extension appended; on Unix-based systems (i.e., Mac and Linux), no extension should be included.
#'
#' @export
#' @import r4ss
#' @import parallel
#' @import tidyverse
#' @import stringr
#' @importFrom utils write.table
#' @importFrom this.path this.proj
#' @importFrom this.path here
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @importFrom rlang eval_tidy
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
#'
setup_ss_basemodel <- function (basemodel_dir,
                                    ss3_path,
                                    n_boot = 100,
                                    seed = NULL,
                                    ss3_exe = "ss3",
                                    bootstrap_outdir = file.path(this.path::this.proj())) {

  #By Default, create a pseudo random number seed if seed is NULL
  if(!is.null(seed)){
    set.seed(seed)
  }


  ## TODO: Option to clean up Previous Bootstrap files.

  ## TODO: REFACTOR Setup as helper function
  # Key directory: boot_dir
  boot_dir <- file.path(basemodel_dir,"Bootstraps")

  dir.create(boot_dir,showWarnings = F) # Directory where bootstrap will be run.

  message(paste0("Creating bootstrap data files in :\n\t", boot_dir))

  #Run Model to SS Once to generate data bootstrap files
  setup_bootstrap_dir(basemodel_dir, boot_dir, ss3_path)

  # Extract end year of model using single run
  endyr <- endyr_model(boot_dir)

  # Set up each bootstrap run in its own folder, to help with running SS in parallel
  Lt <- setup_n_boot_runs(n_boot, basemodel_dir, boot_dir)

  run_parallel(Lt)

  message("\nOUTPUT BOOTSTRAP FILES\n")

  # Copy n_boot sso files back to bootstrap directory
  copy_n_boot_sso(boot_dir, n_boot)


  write_bsn_file(endyr, output_dir = bootstrap_outdir, n_boot)
  #output_bootstrap_runs(endyr, boot_dir, output_dir = bootstrap_outdir )

}

#' Create SS data for bootstrap.
#'
#' Embedded Helper function to copy base model stock synthesis data for each
#' bootstrap and runs.
#'
#' @param basemodel_dir Target base model directory path
#' @param boot_dir Target path for bootstrap output
#' @param n_boot number of bootstraps
#' @param ss3_exe ss3_exe
#'
#' @keywords internal
#'
setup_bootstrap_dir <- function (basemodel_dir,
                                 boot_dir,
                                 n_boot = 1,
                                 ss3_exe = "ss3.exe") {

  checkmate::assert_directory_exists(basemodel_dir)

  # Key directory: boot_dir
  #boot_dir <- file.path(basemodel_dir,"Bootstraps")
  #dir.create(boot_dir,showWarnings = F) # Directory where bootstrap will be run.
  #message(paste0("Creating bootstrap data files in :\n\t", boot_dir))


  #TODO: Check of In Case ss3 binary exists in basemodel_dir, if not
  #ss3 binary saved to basebmodel_dir
  if(!checkmate::test_file_exists(ss3_exe)){
    #stop("missing ss3 path")
    get_ss3_exe(dir = basemodel_dir)
  }

  file.copy(
    list.files(
      basemodel_dir,
      pattern = paste0(
        "ss.par|data.ss|control.ss|starter.ss|forecast.ss", ss3_exe,
        sep="|"),
      full.names = TRUE),
    to = boot_dir)

  start <- r4ss::SS_readstarter(file = file.path(boot_dir, "starter.ss"))
  start$N_bootstraps <- n_boot + 2
  r4ss::SS_writestarter(start, dir = boot_dir, overwrite = T)

  r4ss::run(dir = boot_dir, exe = ss3_exe, extras = "-nohess",
            skipfinished = FALSE, show_in_console = F)
}



#' Create Bootstrap runs
#'
#' Set up each bootstrap run in its own folder, to help with running SS in
#' parallel. The Base Model Stock Synthesis Files are copied to each
#' Bootstrap run.
#'
#' Each Bootstrap run starter file will be saved as Data Boot Run File.
#'
#' @param n_boot number of bootstraps
#' @param basemodel_dir basemodel directory
#' @param boot_dir boostrap directory
#' @param ss3_exe ss3_exe
#'
#' @keywords Internal
#'
setup_n_boot_runs <- function(basemodel_dir,
                              boot_dir,
                              n_boot,
                              ss3_exe = "ss3.exe") {

  #validate Base Model Report
  checkmate::assert_file_exists(file.path(basemodel_dir,"Report.sso"))

  #Validate Base Model CompReport
  checkmate::assert_file_exists(file.path(basemodel_dir,"CoReport.sso"))

  #Validate Base Model covar
  checkmate::assert_file_exists(file.path(basemodel_dir,"Report.sso"))

  #Validate Base Model warning
  checkmate::assert_file_exists(file.path(basemodel_dir,"Report.sso"))


  #create the bootstrap data file numbers (pad with leading 0s)
  boot_num <- stringr::str_pad(seq(1, n_boot, by = 1), 3, pad = "0")



  # Set up each bootstrap run in its own folder, to help with running SS in parallel
  Lt <- vector("list",n_boot)
  for(i in 1:n_boot){

    aBootDir <- file.path(boot_dir, paste0("Boot",i))
    dir.create(aBootDir, showWarnings = FALSE)


    # Copy original SS files
    file.copy(
      list.files(
        basemodel_dir,
        pattern = paste0("ss.par|control.ss|starter.ss|forecast.ss", ss3_exe,
                         sep="|"),
        full.names = TRUE),
      to = aBootDir)

    # Copy the bootstrapped data files
    file.copy(file.path(boot_dir,paste0("data_boot_", boot_num[i], ".ss")),to=aBootDir)
    file.remove(file.path(boot_dir,paste0("data_boot_", boot_num[i], ".ss")))

    # Change Starter file to point to Bootstrap data file
    starter <- r4ss::SS_readstarter(file = file.path(basemodel_dir, "starter.ss")) # read starter file
    starter[["datfile"]] <- paste0("data_boot_", boot_num[i], ".ss")
    r4ss::SS_writestarter(starter, dir = aBootDir, overwrite = TRUE)

    Lt[[i]] <- append(Lt[[i]], aBootDir)

  }

  return(Lt)

}


#' Copy Bootstrap Run Output Files to Bootstrap Directory
#'
#' Copies the Stock Synthesis Report File, Composition Data, Cumulative
#' Summaries, and Log of Warnings from each Bootstrap run to the main
#' bootstrap directory. Each copied bootstrap run file will be renamed to
#' have a bootstrap run identifier.
#'
#'
#' @param boot_dir Bootstrap Directory
#' @param n_boot Number of Bootstraps
#' @param copy_compReport Copy compReport Bootstrap Run to Bootstrap
#' Directory? Default is TRUE
#' @param copy_covar Copy covar Bootstrap Run to Bootstrap Directory? Default
#' is TRUE
#' @param copy_warning Copy warning Bootstrap Run to Bootstrap Directory?
#' Default is TRUE.
#'
#' @keywords internal
#'
copy_n_boot_sso <- function(boot_dir,
                            n_boot,
                            copy_compReport = TRUE,
                            copy_covar = TRUE,
                            copy_warning = TRUE) {

  #validate target boot_dir path
  checkmate::assert_directory_exists(boot_dir)

  for(i in 1:n_boot){

    n_boot_dir <- file.path(boot_dir, paste0("Boot",i))

    #Validate Bootstrap Run directory exists
    checkmate::assert_directory_exists(n_boot_dir)

    file.copy(file.path(n_boot_dir, "Report.sso"),
              paste(boot_dir, "/Report_", i, ".sso", sep = ""),
              overwrite = TRUE)

    if(copy_compReport){
      file.copy(file.path(n_boot_dir, "CompReport.sso"),
                paste(boot_dir, "/CompReport_", i, ".sso", sep = ""),
                overwrite = TRUE)
    }

    if(copy_covar){
      file.copy(file.path(n_boot_dir, "covar.sso"),
                paste(boot_dir, "/covar_", i, ".sso", sep = ""),
                overwrite = TRUE)
    }

    if(copy_warning){
      file.copy(file.path(n_boot_dir, "warning.sso"),
                paste(boot_dir, "/warning_",    i, ".sso", sep = ""),
                overwrite = TRUE)
    }

  }


}

#' Write an Age Based Bootstrap File
#'
#' Writes an Age Based Bootstrap File
#'
#' @param endyr End of Year Model
#' @param output_dir Target path to write AGEPRO bootstrap file
#' @param n_boot number of Bootstraps
#'
#' @keywords internal
#'
write_bsn_file <- function(endyr, output_dir, n_boot = 1){

  AgeStr.List <- list()
  for(i in 1:n_boot){

    aBootDir    <- file.path(output_dir,paste0("Boot",i))
    anOutput    <- r4ss::SS_output(dir=aBootDir)
    anAgeStr    <- data.table::data.table(anOutput$natage)
    FinalAgeStr <- anAgeStr["Yr"==endyr&anAgeStr$'Beg/Mid'=="B"] |> select(-("Area":"Era"))

    AgeStr.List <- append(AgeStr.List,list(FinalAgeStr))
  }

  BootAgeStr <- data.table::rbindlist(AgeStr.List)

  write.table(BootAgeStr,
              file = output_dir,
              row.names=F, col.names=F)

}

#' Extracts end year of model
#'
#' Embedded Helper function to return end year of model (endyr) and derived
#' quants
#'
#' @param boot_dir Bootstrap Directory
#'
#' @import stringr
#' @importFrom r4ss SS_output
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @keywords internal
#'
endyr_model <- function (boot_dir) {

  base.model <- r4ss::SS_output(boot_dir)

  drvquants <-
    base.model$derived_quants %>%
    dplyr::filter(stringr::str_detect(.data$Label,"F_")) %>%
    dplyr::select("Label")

  endyr <-
    max( as.numeric(
      stringr::str_sub(drvquants$Label, stringr::str_length(drvquants$Label)-3,
                       stringr::str_length(drvquants$Label)) ),
      na.rm=TRUE)

  return(endyr)
}

