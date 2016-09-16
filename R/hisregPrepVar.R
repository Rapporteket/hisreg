#' Preparer variabler for plotting
#'
#' Denne funksjonen grupperer og klargjør variabler for andelsplot
#'
#' Her kan detaljer skrives
#'
#' @inheritParams hisregFigAndeler
#'
#' @return Plotparams En liste med plotrelevante størrelser
#'
#' @export
#'
hisregPrepVar <- function(RegData, valgtVar)

{
  retn= 'V'; tittel <- ''; AntVar <- NA; NVar <- NA;
  cexgr <- 1.0; grtxt <- ''; grtxt2 <- ''; subtxt <- '';


  if (valgtVar=='PasientAlder') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    #     RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    #     RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- 'Aldersfordeling i registeret'
    gr <- c(0, seq(10, 80, 10), 120)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '80+')
    subtxt <- 'Aldersgrupper'
  }

  if (valgtVar=='p_age_abscess') {
    RegData$Variabel <- RegData[, valgtVar]
    #     RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    #     RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- 'Alder ved første byll'
    gr <- c(0, seq(5, 50, 5), 120)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '50+')
    subtxt <- 'Aldersgrupper'
    retn <- 'H'
  }

  if (valgtVar=='SykehusNavn') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData$VariabelGr <- RegData$Variabel
    grtxt <- levels(RegData$VariabelGr)
    #     RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    #     RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- 'Andel registrert av deltagende avdelinger'
    # gr <- c(1:5, 9)
    #     grtxt <- c('Grunnskole 7-10 år', 'Yrkesfaglig videregående, yrkesskoler, \neller realskole', 'Allmennfaglig videregående skole \neller gymnas',
    #                'Høgskole eller universitetet \n(mindre enn 4 år)', 'Høgskole eller universitet \n(mer enn, eller 4 år)', 'Ukjent')
    #     RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    # subtxt <- 'Utdanningsnivå'
    retn <- 'H'
  }


  if (valgtVar=='p_education') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- 'Pasientgruppens utdanningsnivå'
    gr <- c(1:5, 9)
    grtxt <- c('Grunnskole 7-10 år', 'Yrkesfaglig videregående, yrkesskoler, \neller realskole', 'Allmennfaglig videregående skole \neller gymnas',
               'Høgskole eller universitetet \n(mindre enn 4 år)', 'Høgskole eller universitet \n(mer enn, eller 4 år)', 'Ukjent')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    subtxt <- 'Utdanningsnivå'
    retn <- 'H'
  }

  if (valgtVar=='pre_smoking') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- 'Pasientgruppens røykevaner'
    gr <- c(1:3, 9)
    grtxt <- c('Røyker nå', 'Røykte tidligere, \nmen sluttet', 'Aldri røykt', 'Ukjent')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    subtxt <- 'Røykevaner'
    retn <- 'H'
  }

  if (valgtVar=='pre_work') {  # Per pasient, nyeste registrering
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- 'Arbeidsstatus preintervensjon'
    gr <- c(1:6)
    grtxt <- c('I full jobb/student', 'Sykmeldt (fullt eller delt)', 'Uføretrygdet (fullt eller delt)',
               'Attføring/rehabilitering', 'Alderspensjon', 'Arbeidsledig')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    subtxt <- 'Yrkesstatus'
    retn <- 'H'
  }

  if (valgtVar=='i_type') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData$Variabel[RegData$Variabel %in% 5:6] <- 4  # Intill videre skal 'Ingen intervention bestemt av lege',
    #'Ingen intervention bestemt av pasient' og 'Ikke møtt' slås sammen
    tittel <- 'Type intervensjon'
    gr <- c(1:4)
    grtxt <- c('Kirurgisk intervensjon', 'Medisinsk intervensjon', 'Kirurgisk og medisinsk \nintervensjon',
               'Ingen intervensjon')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    retn <- 'H'
  }

  if (valgtVar=='i_surgery_type') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$i_type %in% c(1,3), ]
    tittel <- 'Type kirurgi'
    gr <- c(1:5)
    grtxt <- c('Eksisjon lukket \nmed sutur', 'Eksisjon med \nåpen granulering og \nsekundær tilheling',
               'Eksisjon med påfølgende \nhudplantasjon', 'CO2-laser', 'Deroofing')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    # grtxt <- parse(text = grtxt)
    retn <- 'H'
  }

  if (valgtVar=='MedisinskBeh') {
    RegData <- RegData[RegData$i_type %in% c(2,3), ]
    N <- dim(RegData)[1]
    RegData$i_biological_treatment[RegData$i_biological_treatment == 1] <- 0
    RegData$i_biological_treatment[RegData$i_biological_treatment != 0] <- 1
    RegData$i_antibiotic_therapy[RegData$i_antibiotic_therapy == 2] <- 0
    RegData$i_antibiotic_therapy[RegData$i_antibiotic_therapy != 0] <- 1
    RegData$i_antiinflammatory_treatment[RegData$i_antiinflammatory_treatment == 1] <- 0
    RegData$i_antiinflammatory_treatment[RegData$i_antiinflammatory_treatment != 0] <- 1
    RegData$i_analgesics[RegData$i_analgesics == 1] <- 0
    RegData$i_analgesics[RegData$i_analgesics != 0] <- 1
    RegData$i_localized_med_treatment[RegData$i_localized_med_treatment == 2] <- 0
    RegData$i_localized_med_treatment[RegData$i_localized_med_treatment != 0] <- 1
    tittel <- 'Type medisinsk behandling'
    AntVar <- colSums(RegData[, c("i_biological_treatment", "i_antibiotic_therapy", "i_antiinflammatory_treatment",
                                  "i_analgesics", "i_localized_med_treatment")])
    NVar<-rep(N, length(AntVar))
    grtxt <- c('Biologisk \nbehandling', 'Antibiotisk \nbehandling',
               'Antiinflammatorisk \nbehandling', 'Analgetika', 'Lokalisert medisinsk \nbehandling')
    retn <- 'H'
  }



  if (valgtVar=='TidlBeh') {
    SamletPrPID <- aggregate(RegData[, c("p_surgery", "p_antibiotics")], by=list(RegData$PasientID),
                             function(x){if (1 %in% x) {y<-1} else {if (2 %in% x) {y<-0} else {y<-NA}}})
    SamletPrPID <- SamletPrPID[,-1]
    SamletPrPID$begge <- rowSums(SamletPrPID[, c("p_surgery", "p_antibiotics")])
    SamletPrPID$begge[SamletPrPID$begge==1] <- 0
    SamletPrPID$begge[SamletPrPID$begge==2] <- 1
    SamletPrPID$ingen <- NA
    SamletPrPID$ingen[SamletPrPID$p_surgery == 0 & SamletPrPID$p_antibiotics == 0] <- 1
    SamletPrPID$ingen[SamletPrPID$p_surgery == 1 | SamletPrPID$p_antibiotics == 1] <- 0
    AntVar <- colSums(SamletPrPID, na.rm = TRUE)
    NVar<-apply(SamletPrPID, 2, function(x) {sum(!is.na(x))})
    grtxt <- c("Kirurgisk", "Medisinsk", "Begge", "Ingen")
    tittel <- 'Tidligere behandling'
  }

  if (valgtVar=='DLQI_PrePost') {
    RegData$VarPre <- cut(RegData$VarPre, breaks=c(0,10,20,30), include.lowest=TRUE, right=FALSE, labels = F)
    RegData$VarPost <- cut(RegData$VarPost, breaks=c(0,10,20,30), include.lowest=TRUE, right=FALSE, labels = F)
    tittel <- c('DLQI alvorlighetsgrad før og etter behandling')
    grtxt <- c('Mild', 'Moderat', 'Alvorlig')
    grtxt2 <- c('DLQI<10', '10<DLQI<20', 'DLQI>20')
    subtxt <- 'Alvorlighetsgrad'
    cexgr <- 0.8
  }

  if (valgtVar=='Hurley_PrePost') {
    #     RegData$VarPre <- cut(RegData$VarPre, breaks=c(0,10,20,30), include.lowest=TRUE, right=FALSE, labels = F)
    #     RegData$VarPost <- cut(RegData$VarPost, breaks=c(0,10,20,30), include.lowest=TRUE, right=FALSE, labels = F)
    tittel <- c('Hurley-score før og etter behandling')
    grtxt <- c('Stadium I', 'Stadium II', 'Stadium III')
    # grtxt2 <- c('DLQI<10', '10<DLQI<20', 'DLQI>20')
    subtxt <- 'Hurley-score'
    cexgr <- 0.8
  }


  PlotParams <- list(RegData=RegData, tittel=tittel, grtxt=grtxt, grtxt2=grtxt2, subtxt=subtxt,
                     retn=retn, cexgr=cexgr, AntVar=AntVar, NVar=NVar)

  return(invisible(PlotParams))
}
