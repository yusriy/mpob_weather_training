#### Calculate evapotranspiration using the Penman-Monteith model
ET <- function(j,           # Julian day
               Tmin,        # Min temperature [C] 
               Tmax,        # Max temperature [C]
               solar_rad,   # Avg solar radiation [W m-2 day-1]
               wind_speed,  # Avg wind speed [m s-1], must be measured > 2 m height
               z,           # Elevation above ground level for atm pressure calc.
                            # [m]
               RHmax,       # Maximum relative humidity [%]
               RHmin,       # Minimum relative humidity [%]
               lat_deg,     # Latitude [degrees]; northern hemisphere (+), etc.
               a = 0.23     # Grass crop reference default = 0.23 [dimensionless]
               ) {
  
  ## Calculate average day temperature
  meanT <- (Tmin + Tmax) / 2
  
  ## Calculate solar radiation to [MJ m-2 day-1]
  solar_rad <- solar_rad * 0.0864
  
  ## Slope of the saturation vapor pressure curve
  delta <- 4098 * 0.6108 * 
    exp((17.27 * meanT)/(meanT + 237.3))/(meanT + 237.3)^2
  
  ## Atmospheric pressure (kPa)
  P <- 101.3 * ((293 - (0.0065 * z))/293)^5.26
  
  ## Pscyhrometric constant
  lambda <- 0.000665 * P
  
  ## Delta term
  dt <- delta/(delta + (lambda * (1 + 0.34 * wind_speed)))
  
  ## Temperature term
  tt <- (900 / (meanT + 273)) * wind_speed
  
  ## Psi term
  pt <- lambda/(delta + (lambda * (1 + (0.34 * wind_speed))))
  
  ## Mean saturation vapor pressure derived from air temperature (es)
  # Max vapor pressure at max temperature
  eTmax <- 0.6108 * exp((17.27 * Tmax)/(Tmax + 237.3))
  # Minimum vapor pressure at min temperature
  eTmin <- 0.6108 * exp((17.27 * Tmin)/(Tmin + 237.3))
  # Saturated vapor pressure
  es <- (eTmax + eTmin) / 2
  # Actual vapor pressure (ea) derived from relative humidity
  ea <- (eTmin * (RHmax/100) + eTmax * (RHmin/100)) / 2
  
  ## Calculate the inverse relative distance Earth-Sun (dr) and 
  ## solar declination (del)
  # Inverse relative distance (dr)
  dr <- 1 + 0.033 * cos((2 * j * pi)/365)
  # Solar declination
  del <- 0.409 * sin((((2 * pi) / 365) * j) - 1.39)
  # Convert latitude in decimal degrees to radians
  lat_rad <- (pi/180) * lat_deg
  # Sunset hour angle
  sunset <- acos(-1 * tan(lat_rad) * tan(del))
  
  ## Extraterrestial radiation
  ## solar constant, Gsc
  Gsc <- 0.0820 # [MJ m-2 min-1]
  Ra <- ((24 * 60) / pi) * Gsc * dr * ((sunset * sin(lat_rad) * sin(del)) + 
                                         (cos(lat_rad) * cos(del) * sin(sunset)))
  
  # Clear sky solar radiation
  Rso <- (0.75 + (2E-5 * z)) * Ra
  
  # Nett solar or shortwave radiation
  albedo <- a # 0.23 for grass crop reference
  Rns <- (1 - a) * solar_rad
  
  # Nett outgoing longwave radiation
  sigma <- 4.903E-9 # Stefan-Boltzmann constant [MJ K-4 m-2 day-1]
  Rnl <- sigma * ((((Tmax + 273.16)^4 + (Tmin + 273.16)^4))/2) * 
    (0.34 - (0.14 * ea^0.5)) * ((1.35 * (solar_rad/Rso)) - 0.35)
  
  # Nett radiation
  Rn <- Rns - Rnl
  
  # To express net radiation as evaporation
  Rng <- 0.408 * Rn # [mm]
  
  ## Final steps (finally!)
  
  # Radiation term
  ETrad <- dt * Rng # [mm day-1]
  
  # Wind term
  ETwind <- pt * tt * (es - ea) # [mm day-1]
  
  # Final reference evapotranspiration value (ET0)
  ET0 <- ETrad + ETwind # [mm day-1]
  
  return(ET0)
  
}