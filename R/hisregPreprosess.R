#' Preprosesser hisregdata
#'
#' Denne funksjonen gjør en del nøvendig preprosessering for at Hisreg sitt datasett
#' kan brukes i rapporter
#'
#' @inheritParams hisregFigAndeler
#'
#' @export
#'
hisregPreprosess <- function(RegData, versjon=1)
{
  RegData <- RegData[RegData$BasisRegStatus==1, ]
  if (versjon == 1) {
    RegData$HovedDato <- as.Date(RegData$HovedDato, format="%Y-%m-%d") # Ordne datoformat
    RegData$Aar <- as.numeric(format(RegData$HovedDato, "%Y"))
    RegData$ErMann[RegData$ErMann==2] <- 0
    RegData$SykehusNavnLang <- RegData$SykehusNavn
    RegData$SykehusNavn <- as.character(RegData$SykehusNavn)
    RegData$SykehusNavn[RegData$SykehusNavn == 'Haugesund Sanitetsforenings revmatismesykehus'] <- 'Haugesund rev.'
    RegData$SykehusNavn[RegData$SykehusNavn == 'Helse Bergen HF'] <- 'Haukeland Universitetssykehus'
    RegData$SykehusNavn[RegData$SykehusNavn == 'Helse Stavanger HF'] <- 'Stavanger Universitetssykehus'
    RegData$SykehusNavn[RegData$SykehusNavn == 'Oslo universitetssykehus HF'] <- 'Rikshospitalet'
    RegData$SykehusNavn[RegData$SykehusNavn == 'St. Olavs Hospital HF'] <- 'St. Olavs Hospital'
    RegData$SykehusNavn[RegData$SykehusNavn == 'Universitetssykehuset Nord-Norge HF'] <- 'UNN'
    RegData$SykehusNavn <- as.factor(RegData$SykehusNavn)

    RegData$Intervensjon <- NA
    RegData$Intervensjon[RegData$ForlopsType1=="Kirurgisk intervensjon"] <- 1
    RegData$Intervensjon[RegData$ForlopsType1=="Medisinsk intervensjon"] <- 2
    RegData$Intervensjon[RegData$ForlopsType1=="Kirurgisk og medisinsk intervensjon"] <- 3
    RegData$Intervensjon <- factor(RegData$Intervensjon, levels = 1:3, labels = c('Kirugisk', 'Medisinsk', 'Kir. og. med.'))
  }
  if (versjon == 1) {
    RegData <- tidyr::as_tibble(RegData)
    RegData$HovedDato <- as.Date(RegData$HovedDato, format="%Y-%m-%d") # Ordne datoformat
    RegData$Aar <- as.numeric(format(RegData$HovedDato, "%Y"))


    # boolsk <- hisreg::klokebok_hisreg$fysisk_feltnavn[which(hisreg::klokebok_hisreg$type == 'Avkrysningsboks')]
    # boolsk <- intersect(boolsk, names(RegData))
    # RegData <- RegData %>% mutate_at(boolsk, function(x){as.logical(x)})

    RegData$Intervensjon <- NA
    RegData$Intervensjon[RegData$ForlopsType1=="Kirurgisk intervensjon"] <- 1
    RegData$Intervensjon[RegData$ForlopsType1=="Medisinsk intervensjon"] <- 2
    RegData$Intervensjon[RegData$ForlopsType1=="Kirurgisk og medisinsk intervensjon"] <- 3
    RegData$Intervensjon <- factor(RegData$Intervensjon, levels = 1:3, labels = c('Kirugisk', 'Medisinsk', 'Kir. og. med.'))
  }

  return(invisible(RegData))
}
