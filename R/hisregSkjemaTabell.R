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
  if (status == 99) {
    status <-  setdiff(unique(df$SkjemaStatus) ,1)
  }

  skjemaData <- df %>% dplyr::filter(SkjemaStatus %in% status,
                .data[[typeDato]] %>% as.Date() %>%
                  dplyr::between(as.Date(tidFra), as.Date(tidTil)))

  levels(skjemaData$Skjemanavn) <- c("Preintervensjon",
                                   "Intervensjon",
                                   "Kontroll 3 mnd",
                                   "Kontroll 6 mnd",
                                   "Kontroll 9 mnd",
                                   "Kontroll 12 mnd",
                                   "Kontroll 15 mnd",
                                   "Kontroll 18 mnd",
                                   "Kontroll 21 mnd",
                                   "Kontroll 24 mnd" )

  stats::addmargins(table(skjemaData[["SykehusNavn"]],
                        skjemaData[["Skjemanavn"]] ))

}



