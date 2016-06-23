#### Calculate evapotranspiration using the Penman-Monteith equation
ET <- function(Tmin, # Min temperature in C 
               Tmax,  # Max temperature in C
               solar_rad, # Solar radiation in W m-2 day-1
               wind_speed, # Wind speed in m s-1, must be measured > 2 m
               z, # Elevation above ground level for atm pressure calc
               RHmax, # Maximum relative humidity %
               RHmin, # Minimum relative humidity %
               ) {
  # Calculate average day temperature
  meanT <- (Tmin + Tmax) / 2
  # Calculate solar radiation to MJ m-2 day-1
  solar_rad <- solar_rad * 0.0864
  # Slope of the saturation vapor pressure curve
  delta <- 4098 * 0.6108 * 
    exp((17.27 * meanT)/(meanT + 237.3))/(meanT + 237.3)^2
  # Atmospheric pressure (kPa)
  P <- 101.3 * ((293 - (0.0065 * z))/293)^5.26
  # Pscyhrometric constant
  lambda <- 0.000665 * P
  # Delta term
  dt <- delta/(delta + (lambda * (1 + 0.34 * wind_speed)))
  # Temperature term
  tt <- (900 / (meanT + 273)) * wind_speed
  # Mean saturation vapor pressure derived from air temperature (es)
  eTmax <- 0.6108 * exp((17.27 * Tmax)/(Tmax + 237.3))
  eTmin <- 0.6108 * exp((17.27 * Tmin)/(Tmin + 237.3))
  es <- (eTmax + eTmin) / 2
  # Actual vapor pressure (ea) derived from relative humidity
  ea <- (eTmin * (RHmax/100) + eTmax(RHmin/100)) / 2
  
  
}