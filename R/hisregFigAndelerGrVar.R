#' Lag søylediagram eller som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et søylediagram som viser andeler av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' @inheritParams hisregFigAndeler
#'
#' @return En figur med andel av ønsket variabel pr. grupperingsvariabel
#'
#' @export

hisregFigAndelerGrVar <- function(RegData, valgtVar, datoFra='2000-01-01', datoTil='2050-01-01', reshID,
                             minald=0, maxald=120, erMann=99, outfile='', forlop1 = 99, forlop2 = 99,
                             enhetsUtvalg=1, preprosess=F, hentData=F)

{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- hisregHentRegData()
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- hisregPreprosess(RegData=RegData)
  }

  # Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$AvdRESH == reshID), ]}

  # Sykehustekst avhengig av bruker og brukervalg
  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'
  } else {
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)])
  }

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  hisregUtvalg <- hisregUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, forlop1 = forlop1, forlop2 = forlop2)
  RegData <- hisregUtvalg$RegData
  utvalgTxt <- hisregUtvalg$utvalgTxt



  ###################### UNDER UTVIKLING #########################################
  ###################### UNDER UTVIKLING #########################################
  ###################### UNDER UTVIKLING #########################################
  ###################### UNDER UTVIKLING #########################################


}






