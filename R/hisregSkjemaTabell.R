#'Skjematabeller for hisreg
#'
#' Denne funksjonen tar inn skjemaoversikt og lager tabeller
#' av antall skjematyper per sykehus
#'
#' @param df en dataramme med alle variabler
#' @param tidfra start dato. format: "YYYY-mm-dd"
#' @param tidtil slutt dato.  format: "YYYY-mm-dd"
#' @param status skjema status
#'                  1: Ferdigstilt
#'                  2: Ikke ferdig
#' @param typeDato Dato for filtrering. "HovedDato"
#'                                      "OpprettetDato"
#'                                      "SistLagtretDato"
#'
#' @return tabelldata med antall ferdigstilte/ikke ferdige
#' skjema per sykehus
#'
#' @export
#'

hisregSkjemaTabell <- function(df,
                              tidFra = "2008-01-01",
                              tidTil = Sys.Date(),
                              status= 1,
                              typeDato = "HovedDato") {
  iKladd <-  c(0, -1)
  if (status == 99) {
    status <-  iKladd
  } else if ( status == 1) {
    status <- setdiff(unique(df$SkjemaStatus), iKladd)
  }

  skjemaData <- df %>% dplyr::filter(SkjemaStatus %in% status,
                .data[[typeDato]] %>% as.Date() %>%
                  dplyr::between(as.Date(tidFra), as.Date(tidTil)))

  if (dim(skjemaData)[1] != 0) {

  lev <- c("Preintervensjon",
           "Intervensjon",
           "Kontroll 3 mnd",
           "Kontroll 6 mnd",
           "Kontroll 9 mnd",
           "Kontroll 12 mnd",
           "Kontroll 15 mnd",
           "Kontroll 18 mnd",
           "Kontroll 21 mnd",
           "Kontroll 24 mnd" )

  levPresent <- lev[lev %in% unique(skjemaData$Skjemanavn)]
  levels(skjemaData$Skjemanavn) <- levPresent
  skjemaData$Skjemanavn <- ordered(skjemaData$Skjemanavn, levels = levPresent)




  utData <- stats::addmargins(table(skjemaData[["Sykehusnavn"]],
                        skjemaData[["Skjemanavn"]] ))
  utData <- as.data.frame.matrix(utData)
  } else {
    utData <- NULL
  }
  return( utData)
}



