#' Title Almond Yield Anomaly Model 
#' 
#' This function calculates the yearly almond yield anomaly in tons per acre based on the model in the Lobell 2006 paper studied in class.
#' 
#' @authors Paloma Cartwright, Julia Parish, Quin Smith 
#'
#' @param Tn2 minimum temperature (degrees Celsius) for February 
#' @param Tn2_coeff1 coefficient 1 to be used with Tn2, default set at -0.015
#' @param Tn2_coeff2 coefficient 2 to be used with Tn2^2, default is set at -0.0046
#' @param P1 precipitation (mm) for January 
#' @param P1_coeff1 coefficient 1 to be used with P1, default set at -0.07
#' @param P1_coeff2 coefficient 2 to be used with P1^2, default set at 0.0043
#' @param constant default set at 0.28
#'
#' @return almond yield anomaly (ton/acre)
#'
#' @examples almond_model(Tn2 = 20, P1 = 8)
#' @examples almond_model(Tn2 = 20, Tn2_coeff1 = -0.015, Tn2_coeff2 = -0.0046, P1 = 8, P1_coeff1 = -0.07, P1_coeff2 = 0.0043, constant = 0.28)
#' 

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

