julian_conv <- function(x) {
  if (is.na(x)) { # Because julian() cannot accept NA values
    return(NA)
  }
  else {
    j <-julian(x, 
               origin = as.POSIXlt(paste0(format(x, "%Y"),'-01-01')))
    temp <- unclass(j) # To unclass the object julian day to extract julian day
    return(temp[1])
  }
  
}
