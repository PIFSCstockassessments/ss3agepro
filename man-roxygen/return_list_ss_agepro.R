#' @return
#' This fuction Returns a list containing the the Stock Synthesis following values:
#'  * `Nfleets` is the total number of catch fleets in the SS3 Report file
#'  * `alpha` is the alpha parameter of a Beverton Holt or Richards stock assessment function
#'  * `beta` is the beta parameter of a beverton holt or richards stock assessment function
#'  * `BH_Var` the sigmaR from the SS3 model, or the variance of the recruitment deviations
#'  * `RecruitmentObs` is a datatable containing the predicted recruitment, SSB, and recruitment deviation for each year or quarter
#'  * `MaxAge` is the Amax parameter from the SS3 model or maximum age, note for the Quarter time step this is maximum age in quarters (or `MaxAge\*4`)
#'  * `MaturityAtAge` is a vector containing the probability of maturity at age by year or quarter
#'  * `MaturityAtAgeCV` is a vector of the CV for the probability of maturity at age, note this (and all CVs) are set to 0.1 as a default but can be adjusted after running the script
#'  * `Fishery_SelAtAge` datatables of the selectivity by age and CV for each of the Nfleets
#'  * `Fishery_SelAtAgeCV` datatables of the selectivity by age and CV for each of the Nfleets
#'  * `NatMort_atAge` vectors of the natural mortality by age in years or quarters and their CV
#'  * `NatMort_atAgeCV` vectors of the natural mortality by age in years or quarters and their CV
#'  * `Jan_WAA` is the weight-at-age of the stock on Jan-1
#'  * `Jan_WAACV` CV of Weight-Of-Age of stock on Jan-1
#'  * `MidYear_WAA` is the weight-at-age of the stock on July-1
#'  * `MidYear_WAACV` CV of weight-at-age of the stock on July-1
#'  * `SSB_WAA` SSB Weight of Age
#'  * `SSB_WAACV` SSB Weight-of-Age
#'  * `SSB_WAACV` CV of SSB Weight-Of-Age
#'  * `CatchAtAge` Catch Weight-of-age by Nfleet
#'  * `CatchAtAgeCV` Catch Weight-of-Age CV by Nfleet
#'  * `CatchByFleet` is the total catch by fleet in the last year/quarter of the model
#'  * `FByFleet` Fishing Mortality by Fleet
#'  * `StartYr` First year of the SS3 Report time period
#'  * `EndYr` Final year of the SS3 Report time period
