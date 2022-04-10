#' Almond Yield Anomaly Model 
#'
#' @param mon_temp_min 
#' @param mon_precip 
#' @param temp_coeff1 
#' @param temp_coeff2 
#' @param precip_coeff1 
#' @param precip_coeff2 
#' @param constant 
#'
#' @return
#' @export
#'
#' @examples

almond_model <- function(mon_temp_min, mon_precip, temp_coeff1, temp_coeff2,
                         precip_coeff1, precip_coeff2, constant){

  yield_anomaly <- (mon_temp_min * temp_coeff1) +
    (mon_temp_min^2 * temp_coeff2) +
    (mon_precip * precip_coeff1) +
    (mon_precip^2 * precip_coeff2) +
    constant
  
  return(yield_anomaly)
}

