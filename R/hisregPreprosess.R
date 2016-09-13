#' Preprosesser hisregdata
#'
#' Denne funksjonen gjør en del nøvendig preprosessering for at Hisreg sitt datasett
#' kan brukes i rapporter
#'
#'
hisregPreprosess <- function(RegData)
{
  RegData <- RegData[RegData$BasisRegStatus==1, ]

  # datoVar <- 'HovedDato'
  RegData$HovedDato <- as.POSIXlt(RegData$HovedDato, format="%Y-%m-%d") # Ordne datoformat
  RegData$ErMann[RegData$ErMann==2] <- 0

  return(invisible(RegData))
}
