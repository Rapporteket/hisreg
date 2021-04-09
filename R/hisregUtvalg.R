#' Gjør utvalg av dataene
#'
#' Denne funksjonen gjør utvalg av dataene og returnerer det filtrerte datasettet, utvalgsteksten
#' og fargepaletten for bruk i figuren
#'
#' @inheritParams hisregFigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Default: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

hisregUtvalg <- function(RegData, datoFra, datoTil, minald, maxald, erMann, forlop1, forlop2, fargepalett='BlaaRapp')
{
  # Definerer intersect-operator
  "%i%" <- intersect

  Ninn <- dim(RegData)[1]
  indVarMed <- 1:Ninn
  indAld <- which(RegData$PasientAlder >= minald & RegData$PasientAlder <= maxald)
  indDato <- which(RegData$HovedDato >= datoFra & RegData$HovedDato <= datoTil)
  indKj <- if (erMann %in% 0:1) {which(RegData$ErMann == erMann)} else {indKj <- 1:Ninn}
  indForlop1 <- if (forlop1 %in% c(1:4)) {which(RegData$ForlopsType1Num == forlop1)} else {indForlop1 <- 1:Ninn}
  indForlop2 <- if (forlop2 %in% c(1:5)) {which(RegData$ForlopsType2Num == forlop2)} else {indForlop2 <- 1:Ninn}

  indMed <- indVarMed %i% indAld %i% indDato %i% indKj  %i% indForlop1 %i% indForlop2
  RegData <- RegData[indMed,]

  if (dim(RegData)[1] > 0){
    utvalgTxt <- c(paste('Registrert: ',
                         min(RegData$HovedDato, na.rm=T), ' til ', max(RegData$HovedDato, na.rm=T), sep='' ),
                   if ((minald>0) | (maxald<120)) {
                     paste('Pasienter fra ', min(RegData$PasientAlder, na.rm=T), ' til ', max(RegData$PasientAlder, na.rm=T), ' år', sep='')},
                   if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
                   if (forlop1 %in% c(1:4)) {paste0('Intervensjon: ', RegData$ForlopsType1[match(forlop1, RegData$ForlopsType1Num)])},
                   if (forlop2 %in% c(1:5)) {paste0('Forl\370pstype 2: ', RegData$ForlopsType2[match(forlop2, RegData$ForlopsType2Num)])}
    )
  } else {
    utvalgTxt <- paste0('Dato: ', datoFra, ' til ', datoTil)
  }


  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
  return(invisible(UtData))
}
