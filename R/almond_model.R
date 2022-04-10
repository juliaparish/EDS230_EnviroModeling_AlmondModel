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

almond_model <- function(Tn2, Tn2_coeff1 = -0.015, Tn2_coeff2 = -0.0046,
                         P1, P1_coeff1 = -0.07, P1_coeff2 = 0.0043, 
                         constant = 0.28)
  {
  yield_anomaly <- (Tn2_coeff1 * Tn2) +
    (Tn2_coeff2 * Tn2**2) +
    (P1_coeff1 * P1) +
    (P1_coeff2 * P1**2) +
    constant
  
  return(yield_anomaly)
}

