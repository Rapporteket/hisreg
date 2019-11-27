#' Forløps of pasient tabeller for hisreg
#'
#' Denne funksjonen tar inn registerdata og lager tabeller
#' av antall pasienter/forløp per sykehus per tidsenhet
#'
#' @param RegDt en dataramme med alle variabler
#' @param tidfra start dato. format: "YYYY-mm-dd"
#' @param tidtil slutt dato.  format: "YYYY-mm-dd"
#' @param aldmin alder fra.
#' @param aldmax  alder til
#' @param kjoen kjønn der Kvinne = 0
#'                        Mann = 1
#'                        begge = 99
#' @param tidenh tidsenhet. kan være "aar" eller "maaned"
#' @param frlType forløpstype
#'                  1: Kirurgisk intervensjon
#'                  2: Medisinsk intervensjon
#'                  3: Kirurgisk og medisinsk intervensjon
#'                  4: Ingen intervensjon
#' @param IDtype ID for filtrering. "m_mceid" eller "PasientID"
#'
#' @return tabelldata med antall pasienter/forløp per sykhus
#' per tidsenhet
#'
#' @export
#'

hisregForlPasTabell <- function(RegDt ,
                                tidFra = "2008-01-01",
                                tidTil = Sys.Date(),
                                aldmin = 0,
                                aldmax = 120,
                                kjoen = 99,
                                tidenh = "aar",
                                frlType = 1,
                                IDType = "PasientID") {

  if (kjoen == 99) {
    kjoen <- c(0, 1)
  }
  if (99 %in% frlType ) {
    ingen_intervensjon = unique(RegDt$ForlopsType1Num) %>%
      setdiff(c(1,2,3))
    frlType <- frlType %>%
      setdiff(99) %>%
      c(ingen_intervensjon)

  }
  if(tidenh == "maaned" & length(seq(as.Date(tidFra), as.Date(tidTil), by = "month")) - 1 < 14) {
    tidenh <- "underEtAar"
  }


  tabData <- RegDt %>%
    dplyr::select(PasientID,
                  m_mceid,
                  SykehusNavn,
                  HovedDato,
                  ErMann,
                  PasientAlder,
                  ForlopsType1Num,
                  ForlopsType1) %>%
      dplyr::mutate(maaned = factor(lubridate::month(HovedDato),
                                    labels = c("Jan", "Feb", "Mar",
                                               "Apr", "Mai", "Jun",
                                               "Jul", "Aug", "Sep",
                                               "Okt", "Nov", "Des")),
                  aar = lubridate::year(HovedDato),
                  underEtAar = paste(maaned, "-", aar)) %>%
        dplyr::filter(as.Date(HovedDato) %>% dplyr::between(as.Date(tidFra),
                                                             as.Date(tidTil)),
                       PasientAlder %>% dplyr::between(aldmin, aldmax),
                       ErMann %in% kjoen, ForlopsType1Num %in% frlType) %>%
          dplyr::select(PasientID, m_mceid, SykehusNavn, maaned, aar, underEtAar) %>%
            dplyr::arrange(aar, maaned)

  tabData <- tabData %>% dplyr::filter(!duplicated(tabData[[IDType]]))
  tabData$underEtAar <- ordered(
    tabData$underEtAar,
    levels = unique(tabData$underEtAar)
  )
  utData <-  addmargins(table(tabData[["SykehusNavn"]],
                                      tabData[[tidenh]]))
  return( as.data.frame.matrix(utData))
}
