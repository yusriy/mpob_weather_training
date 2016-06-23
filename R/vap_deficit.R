#### Calculate vapor pressure deficit 
vap_deficit <- function(Temp, # in K 
                        RH # in %
                        ) { 
  
  # Saturate vapor pressure, units of kPa
  # From the ASCE Standardized Reference Evapotranspiration Equation
  vpsat <- 0.6108 * exp(17.27 * Temp / (Temp + 237.3))
  # Actual partial pressure, units of kPa
  vpair <- vpsat * RH/100
  # Vapor pressure deficit, units of kPa
  vpd <- vpsat - vpair
}
