#' Title Almond Yield Anomaly Profit Model
#' 
#' This function calculates the profit of anomalous almond yield per year. 
#' 
#' @authors Paloma Cartwright, Julia Parish, Quin Smith
#' 
#' @param anmly almond yield anomaly ~ avg monthly min temp and tot precipitation (tons/acre)
#' @param price price per almond ($/ton)
#' @param year when was anmly obtained
#' @param discount rate (default 0.12)
#' @return data frame with estimate of profit ($/tons/acre)

compute_profit_fromanmly <- function(anmly, year, price, discount = 0.12) {
  
  if (length(anmly) < 1)
    return(NA)
  
  scen <- seq(from = 1, to = length(anmly))
  
  yearprofit <- data.frame(scen = scen, anmly = anmly, year = year)
  
  yearprofit$net <- yearprofit$anmly * price


  yearprofit <- yearprofit %>% 
    mutate(price = price, netpre = compute_NPV(value = net, time = year - year[1], discount = discount))
  
  return(yearprofit)
}