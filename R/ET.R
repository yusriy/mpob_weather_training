#### Calculate evapotranspiration using the Penman-Monteith equation
ET <- function(Tmin, # Min temperature in C 
               Tmax,  # Max temperature in C
               solar_rad, # Solar radiation in W m-2 day-1
               ) {
  meanT <- (Tmin + Tmax) / 2
  
}