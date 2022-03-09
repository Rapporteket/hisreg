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


  if (valgtVar=='PasientAlder') { # per pasient
    # RegData$Variabel <- RegData[, valgtVar]
    RegData$Variabel <- RegData[[valgtVar]]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- 'Aldersfordeling i registeret'
    gr <- c(0, seq(10, 80, 10), 120)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    # grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '80+')
    grtxt <- c('<10', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '>=80')
    subtxt <- 'Aldersgrupper'
    retn <- 'H'
  }

  if (valgtVar=='p_age_abscess') {# per pasient
    RegData$Variabel <- RegData[[valgtVar]]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- 'Alder ved første byll'
    gr <- c(0, seq(10, 80, 10), 120)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c('<10', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '>=80')
    # gr <- c(0, seq(5, 50, 5), 120)
    # RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    # # grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '50+')
    # grtxt <- c('<5', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-59', '>=50')
    subtxt <- 'Aldersgrupper'
    retn <- 'H'
  }

  if (valgtVar=='AGE_ABSCESS') {# per pasient
    RegData$Variabel <- RegData[[valgtVar]]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- 'Alder ved første byll'
    gr <- c(0, seq(10, 80, 10), 120)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c('<10', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '>=80')
    subtxt <- 'Aldersgrupper'
    retn <- 'H'
  }

  if (valgtVar=='ErMann') {# per pasient
    gr <- c(0,1)
    grtxt <- c('Kvinne', 'Mann')
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$Variabel <- as.character(RegData[[valgtVar]])
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Kjønnsfordeling HISREG'
    retn <- 'H'
  }

  if (valgtVar=='SykehusNavn') {
    RegData$Variabel <- as.character(RegData[[valgtVar]])
    RegData$VariabelGr <- factor(RegData$Variabel, levels = names(table(RegData$Variabel)[order(table(RegData$Variabel), decreasing = T)]))
    grtxt <- levels(RegData$VariabelGr)
    tittel <- 'Andel registrert av deltagende avdelinger'
    retn <- 'H'
  }

  if (valgtVar=='UnikePasienter') {
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$Variabel <- as.character(RegData$SykehusNavn)
    RegData$VariabelGr <- factor(RegData$Variabel, levels = names(table(RegData$Variabel)[order(table(RegData$Variabel), decreasing = T)]))
    grtxt <- levels(RegData$VariabelGr)
    tittel <- c('Andel unike pasienter registrert', 'av deltagende avdelinger')
    retn <- 'H'
  }

  if (valgtVar=='p_education') {
    RegData$Variabel <- RegData[[valgtVar]]
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

  if (valgtVar=='UtdanningSSB') {
    RegData$Variabel <- RegData[[valgtVar]]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- 'Pasientgruppens utdanningsnivå'
    gr <- c("01 Grunnskolenivå", "02a Videregående skolenivå",
               "03a Universitets- og høgskolenivå kort",
               "04a Universitets- og høgskolenivå lang",
               "09a Uoppgitt eller ingen fullført utdanning")
    grtxt <- c("01 Grunnskole", "02a Videregående skole",
               "03a Universitets- og \n høgskolenivå kort",
               "04a Universitets- og \n høgskolenivå lang",
               "09a Uoppgitt eller ingen \n fullført utdanning")

    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    subtxt <- 'Utdanningsnivå'
    retn <- 'H'
  }

  if (valgtVar=='pre_smoking') {
    RegData$Variabel <- RegData[[valgtVar]]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- 'Pasientgruppens røykevaner'
    gr <- c(1:3, 9)
    grtxt <- c('Røyker nå', 'Røykte tidligere, \nmen sluttet', 'Aldri røykt', 'Ukjent')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    subtxt <- 'Røykevaner'
    retn <- 'H'
  }

  if (valgtVar=='SMOKING') {
    RegData$Variabel <- RegData[[valgtVar]]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- 'Pasientgruppens røykevaner'
    # gr <- klokebok_hisreg$listeverdier[which(klokebok_hisreg$fysisk_feltnavn %in% valgtVar)]
    # grtxt <- klokebok_hisreg$listetekst[which(klokebok_hisreg$fysisk_feltnavn %in% valgtVar)]
    gr <- c(1:3, 9)
    grtxt <- c('Røyker nå', 'Røykte tidligere, \nmen sluttet', 'Aldri røykt', 'Ukjent')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    subtxt <- 'Røykevaner'
    retn <- 'H'
  }

  if (valgtVar=='pre_work') {  # Per pasient, nyeste registrering
    RegData$Variabel <- RegData[[valgtVar]]
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

  if (valgtVar=='WORK') {  # Per pasient, nyeste registrering
    RegData$Variabel <- RegData[[valgtVar]]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- 'Arbeidsstatus preintervensjon'
    # gr <- klokebok_hisreg$listeverdier[which(klokebok_hisreg$fysisk_feltnavn %in% valgtVar)]
    # grtxt <- klokebok_hisreg$listetekst[which(klokebok_hisreg$fysisk_feltnavn %in% valgtVar)]
    gr <- c(1:6, 9)
    grtxt <- c("Fullt arbeid/student", "Sykemeldt (fullt eller gradert)",
            "Uførepensjon (fullt eller gradert)", "Arbeidsavklaring",
            "Alderspensjon", "Arbeidsledig", "Vet ikke")
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    subtxt <- 'Yrkesstatus'
    retn <- 'H'
  }


  if (valgtVar=='i_type') {
    RegData$Variabel <- RegData[[valgtVar]]
    RegData$Variabel[RegData$Variabel %in% 5:6] <- 4  # Inntil videre skal 'Ingen intervention bestemt av lege',
    # "Ingen intervention bestemt av pasient" og 'Ikke møtt' slås sammen
    tittel <- 'Type intervensjon'
    gr <- c(1:4)
    grtxt <- c('Kirurgisk intervensjon', 'Medisinsk intervensjon', 'Kirurgisk og medisinsk \nintervensjon',
               'Ingen intervensjon')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    RegData$Gr <- RegData$SykehusNavn
    retn <- 'H'
  }

  if (valgtVar=='ForlopsType1Num') {
    RegData$Variabel <- RegData[[valgtVar]]
    RegData$Variabel[RegData$Variabel %in% 5:6] <- 4  # Intill videre skal 'Ingen intervention bestemt av lege',
    # "Ingen intervention bestemt av pasient" og 'Ikke møtt' slås sammen
    tittel <- 'Type intervensjon'
    gr <- c(1:4)
    grtxt <- c('Kirurgisk intervensjon', 'Medisinsk intervensjon', 'Kirurgisk og medisinsk \nintervensjon',
               'Ingen intervensjon')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    RegData$Gr <- RegData$SykehusNavn
    retn <- 'H'
  }


  if (valgtVar=='TYPE_INTERVENTION') {
    RegData$Variabel <- RegData[[valgtVar]]
    RegData$Variabel[RegData$Variabel %in% 5:6] <- 4  # Intill videre skal 'Ingen intervention bestemt av lege',
    # "Ingen intervention bestemt av pasient" og 'Ikke møtt' slås sammen
    tittel <- 'Type intervensjon'
    gr <- c(1:4)
    grtxt <- c('Kirurgisk intervensjon', 'Medisinsk intervensjon', 'Kirurgisk og medisinsk \nintervensjon',
               'Ingen intervensjon')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    RegData$Gr <- RegData$SykehusNavn
    retn <- 'H'
  }

  if (valgtVar=='i_surgery_type') {
    RegData$Variabel <- RegData[[valgtVar]]
    RegData <- RegData[RegData$i_type %in% c(1,3), ]
    tittel <- 'Type kirurgi'
    gr <- c(1:5)
    grtxt <- c('Eksisjon lukket \nmed sutur', 'Eksisjon med \nåpen granulering og \nsekundær tilheling',
               'Eksisjon med påfølgende \nhudplantasjon', 'CO2-laser', 'Deroofing')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    RegData$Gr <- RegData$SykehusNavn
    retn <- 'H'
  }

  if (valgtVar=='type_kirurgi') { # NY
    RegData <- RegData[RegData$TYPE_INTERVENTION %in% c(1,3), ]
    N <- dim(RegData)[1]
    tittel <- 'Type kirurgi'
    AntVar <- colSums(RegData[, c("SURGERY_TYPE_EXCISION_SUTUR", "SURGERY_TYPE_EXCISION_GRANUL",
                                  "SURGERY_TYPE_EXCISION_TRANSPLANT", "SURGERY_TYPE_LASER",
                                  "SURGERY_TYPE_DEROOFING")], na.rm = TRUE)
    NVar<-rep(N, length(AntVar))
    grtxt <- c('Eksisjon lukket \nmed sutur', 'Eksisjon med \nåpen granulering og \nsekundær tilheling',
               'Eksisjon med påfølgende \nhudplantasjon', 'CO2-laser', 'Deroofing')
    retn <- 'H'
  }

  if (valgtVar=='type_kirurgi_gr') { # NY
    RegData <- RegData[RegData$TYPE_INTERVENTION %in% c(1,3), ]
    N <- dim(RegData)[1]
    tittel <- 'Type kirurgi'
    AntVar <- aggregate(RegData[, c("SURGERY_TYPE_EXCISION_SUTUR", "SURGERY_TYPE_EXCISION_GRANUL",
                                    "SURGERY_TYPE_EXCISION_TRANSPLANT", "SURGERY_TYPE_LASER",
                                    "SURGERY_TYPE_DEROOFING")], by = list(RegData$SykehusNavn), sum, na.rm=T)
    NVar <- aggregate(RegData[, c("SURGERY_TYPE_EXCISION_SUTUR", "SURGERY_TYPE_EXCISION_GRANUL",
                                  "SURGERY_TYPE_EXCISION_TRANSPLANT", "SURGERY_TYPE_LASER",
                                  "SURGERY_TYPE_DEROOFING")], by = list(RegData$SykehusNavn), length)

    grtxt <- c('Eksisjon lukket \nmed sutur', 'Eksisjon med \nåpen granulering og \nsekundær tilheling',
               'Eksisjon med påfølgende \nhudplantasjon', 'CO2-laser', 'Deroofing')
    retn <- 'H'
    cexgr <- 1
  }



  if (valgtVar=='i_biological_treatment') {
    RegData <- RegData[which(RegData$i_type %in% c(2,3)), ]
    RegData$Variabel <- RegData[[valgtVar]]
    RegData <- RegData[which(RegData$Variabel != 1), ]
    # RegData$Variabel[is.na(RegData$Variabel)] <- 99
    tittel <- 'Biologiske legemidler'
    gr <- c(4,5,6,3)
    grtxt <- c('Infliximab', 'Adalimumab', 'Biosimilars', 'Andre')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    retn <- 'H'
  }

  if (valgtVar=='BiologiskBeh') { # NY
    RegData <- RegData[which(RegData$TYPE_INTERVENTION %in% c(2,3)), ]
    N <- dim(RegData)[1]
    tittel <- 'Type biologisk behandling'
    AntVar <- colSums(RegData[, c("ADALIMUMAB_BIOSIMILAR", "INFLIXIMAB_BIOSIMILAR",
                                  "BIOLOGICAL_TREATMENT_OTHER")], na.rm = TRUE)
    NVar<-rep(N, length(AntVar))
    grtxt <- c('Adalimumab', 'Infliximab', 'Andre')
    retn <- 'H'
  }

  if (valgtVar=='Antibiotisk') {
    RegData <- RegData[which(RegData$i_antibiotic_therapy == 1), ]
    N <- dim(RegData)[1]
    tittel <- 'Type antibiotisk behandling'
    AntVar <- colSums(RegData[, c("i_rifampicin_clindamycin", "i_tetracyclin_lymecyclin", "i_amoxicilin",
                                  "i_sys_antibiotic_other")], na.rm = TRUE)
    NVar<-rep(N, length(AntVar))
    grtxt <- c("Rifampicin og Clindamycin", "Tetracyclin eller Lymecyclin", "Amoxicilin med clavulansyre",
               "Andre")
    retn <- 'H'
  }

  if (valgtVar=='Antibiotisk_v2') { # NY
    RegData <- RegData[which(RegData$SYSTEMIC_ANTIBIOTIC_THERAPY == 1), ]
    N <- dim(RegData)[1]
    tittel <- 'Type antibiotisk behandling'
    AntVar <- colSums(RegData[, c("RIFAMPICIN_CLINDAMYCIN", "TETRACYCLIN_LYMECYCLIN", "AMOXICILIN",
                                  "SYSTEMIC_ANTIBIOTIC_THERAPY_OTHER")], na.rm = TRUE)
    NVar<-rep(N, length(AntVar))
    grtxt <- c("Rifampicin og Clindamycin", "Tetracyclin eller Lymecyclin", "Amoxicilin med clavulansyre",
               "Andre")
    retn <- 'H'
  }

  if (valgtVar=='LokalisertMedisinsk') {
    RegData <- RegData[which(RegData$i_localized_med_treatment == 1), ]
    N <- dim(RegData)[1]
    tittel <- 'Type lokalisert medisinsk behandling'
    AntVar <- colSums(RegData[, c("i_corticosteroid_injection", "i_acelacic_acid", "i_clindamycin", "i_resorcinol")], na.rm = TRUE)
    NVar<-rep(N, length(AntVar))
    grtxt <- c("Intralesjonell kortikosteroidinjeksjon", "Acelacic acid", "Clindamycin", "Resorcinol 15%")
    retn <- 'H'
  }

  if (valgtVar=='LokalisertMedisinsk_v2') {
    RegData <- RegData[which(RegData$BASED_MEDICAL_TREATMENT == 1), ]
    N <- dim(RegData)[1]
    tittel <- 'Type lokalisert medisinsk behandling'
    AntVar <- colSums(RegData[, c("INTRALESIONAL_CORTICOSTEROID_INJECTION", "ACELACIC_ACID", "CLINDAMYCIN", "RESORCINOL")], na.rm = TRUE)
    NVar<-rep(N, length(AntVar))
    grtxt <- c("Triamcinolon", "Acelacic acid", "Clindamycin", "Resorcinol 15%")
    retn <- 'H'
  }

  if (valgtVar=='i_antiinflammatory_treatment') {
    RegData <- RegData[which(RegData$i_type %in% c(2,3)), ]
    RegData$Variabel <- RegData[[valgtVar]]
    RegData <- RegData[which(RegData$Variabel != 1), ]
    tittel <- 'Antiinflammatorisk behandling'
    gr <- c(4,5,6,8,3)
    grtxt <- c('Dapson', 'Prednisolon', 'Ciclosporin', 'Acitretin', 'Andre')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    retn <- 'H'
  }

  if (valgtVar=='AntiinflammatoriskBeh') {
    RegData <- RegData[which(RegData$ANTIINFLAMMATORY_TREATMENT == 1), ]
    tittel <- 'Antiinflammatorisk behandling'
    N <- dim(RegData)[1]
    AntVar <- colSums(RegData[, c("DAPSON", "PREDNISOLON", "CICLOSPORIN", "ACITRETIN", "ISOTRETINOIN",
                                  "METFORMIN", "ANTIINFLAMMATORY_TREATMENT_OTHER")], na.rm = TRUE)
    NVar<-rep(N, length(AntVar))
    grtxt <- c('Dapson', 'Prednisolon', 'Ciclosporin', 'Acitretin', 'Isotretinoin', 'Metformin','Andre')
    retn <- 'H'
  }


  if (valgtVar=='MedisinskBeh') {
    RegData <- RegData[which(RegData$i_type %in% c(2,3)), ]
    N <- dim(RegData)[1]
    RegData$i_biological_treatment[RegData$i_biological_treatment == 1] <- 0
    RegData$i_biological_treatment[which(RegData$i_biological_treatment != 0)] <- 1
    RegData$i_antibiotic_therapy[RegData$i_antibiotic_therapy == 2] <- 0
    RegData$i_antibiotic_therapy[RegData$i_antibiotic_therapy == 9] <- NA
    # RegData$i_antibiotic_therapy[RegData$i_antibiotic_therapy != 0] <- 1
    RegData$i_antiinflammatory_treatment[RegData$i_antiinflammatory_treatment == 1] <- 0
    RegData$i_antiinflammatory_treatment[which(RegData$i_antiinflammatory_treatment != 0)] <- 1
    RegData$i_analgesics[RegData$i_analgesics == 1] <- 0
    RegData$i_analgesics[which(RegData$i_analgesics != 0)] <- 1
    RegData$i_localized_med_treatment[RegData$i_localized_med_treatment == 2] <- 0
    RegData$i_localized_med_treatment[which(RegData$i_localized_med_treatment != 0)] <- 1
    tittel <- 'Type medisinsk behandling'
    AntVar <- colSums(RegData[, c("i_biological_treatment", "i_antibiotic_therapy", "i_antiinflammatory_treatment",
                                  "i_analgesics", "i_localized_med_treatment")], na.rm = TRUE)
    # NVar<-rep(N, length(AntVar))
    NVar<-apply(RegData[, c("i_biological_treatment", "i_antibiotic_therapy", "i_antiinflammatory_treatment",
                            "i_analgesics", "i_localized_med_treatment")], 2, function(x){length(which(!is.na(x)))})
    grtxt <- c('Biologisk \nbehandling', 'Antibiotisk \nbehandling',
               'Antiinflammatorisk \nbehandling', 'Analgetika', 'Lokalisert medisinsk \nbehandling')
    retn <- 'H'
  }

  if (valgtVar=='MedisinskBeh_v2') { # NY
    RegData <- RegData[which(RegData$TYPE_INTERVENTION %in% c(2,3)), ]
    N <- dim(RegData)[1]
    tittel <- 'Type medisinsk behandling'
    AntVar <- colSums(RegData[, c("BIOLOGICAL_TREATMENT", "SYSTEMIC_ANTIBIOTIC_THERAPY", "ANTIINFLAMMATORY_TREATMENT",
                                  "ANALGESICS", "BASED_MEDICAL_TREATMENT")], na.rm = TRUE)
    NVar<-rep(N, length(AntVar))
    grtxt <- c('Biologisk \nbehandling', 'Antibiotisk \nbehandling',
               'Antiinflammatorisk \nbehandling', 'Analgetika', 'Lokalisert medisinsk \nbehandling')
    retn <- 'H'
  }

  if (valgtVar=='MedisinskBeh_gr') { # NY
    RegData <- RegData[RegData$TYPE_INTERVENTION %in% c(2,3), ]
    N <- dim(RegData)[1]
    tittel <- 'Type medisinsk behandling'
    AntVar <- aggregate(RegData[, c("BIOLOGICAL_TREATMENT", "SYSTEMIC_ANTIBIOTIC_THERAPY", "ANTIINFLAMMATORY_TREATMENT",
                                    "ANALGESICS", "BASED_MEDICAL_TREATMENT")], by = list(RegData$SykehusNavn), sum, na.rm=T)
    NVar <- aggregate(RegData[, c("BIOLOGICAL_TREATMENT", "SYSTEMIC_ANTIBIOTIC_THERAPY", "ANTIINFLAMMATORY_TREATMENT",
                                  "ANALGESICS", "BASED_MEDICAL_TREATMENT")], by = list(RegData$SykehusNavn), length)

    grtxt <- c('Biologisk \nbehandling', 'Antibiotisk \nbehandling',
               'Antiinflammatorisk \nbehandling', 'Analgetika', 'Lokalisert medisinsk \nbehandling')
    retn <- 'H'
    cexgr <- 1
  }


  if (valgtVar=='KirurgiLokalisering') {
    RegData <- RegData[which(RegData$i_type %in% c(1,3)), ]
    N <- dim(RegData)[1]
    RegData$i_aksille[which(RegData$i_aksille == 2)] <- 0
    RegData$i_aksille[which(RegData$i_aksille == 3)] <- NA
    RegData$i_lyske[which(RegData$i_lyske == 2)] <- 0
    RegData$i_lyske[which(RegData$i_lyske == 3)] <- NA
    RegData$i_pubis[which(RegData$i_pubis == 2)] <- 0
    RegData$i_pubis[which(RegData$i_pubis == 3)] <- NA
    RegData$i_genitalt[which(RegData$i_genitalt == 2)] <- 0
    RegData$i_genitalt[which(RegData$i_genitalt == 3)] <- NA
    RegData$i_peritalt[which(RegData$i_peritalt == 2)] <- 0
    RegData$i_peritalt[which(RegData$i_peritalt == 3)] <- NA
    RegData$i_glutealt[which(RegData$i_glutealt == 2)] <- 0
    RegData$i_glutealt[which(RegData$i_glutealt == 3)] <- NA
    RegData$i_mammae[which(RegData$i_mammae == 2)] <- 0
    RegData$i_mammae[which(RegData$i_mammae == 3)] <- NA
    RegData$i_other_location[which(RegData$i_other_location == 2)] <- 0
    RegData$i_other_location[which(RegData$i_other_location == 3)] <- NA
    tittel <- 'Lokalisasjon av kirugisk inngrep'
    AntVar <- colSums(RegData[, c("i_aksille", "i_lyske", "i_pubis", "i_genitalt",
                                  "i_peritalt", 'i_glutealt', 'i_mammae', 'i_other_location')], na.rm = TRUE)
    NVar<-apply(RegData[, c("i_aksille", "i_lyske", "i_pubis", "i_genitalt",
                            "i_peritalt", 'i_glutealt', 'i_mammae', 'i_other_location')], 2, function(x){length(which(!is.na(x)))})
    grtxt <- c('Aksille', 'Lyske', 'Pubis', 'Genitalt', 'Perianalt', 'Glutealt', 'Mammae', 'Annen \nlokalisasjon')
    retn <- 'H'
  }

  if (valgtVar== 'KomplKir') {
    RegData <- RegData[which(RegData$i_type %in% c(1,3)), ]
    RegData <- RegData[which(RegData$c_do_month == 6 & RegData$OppflgRegStatus >= 1), ]
    RegData <- RegData[which(RegData$c_infection %in% c(1,2) | RegData$c_delayed_wound_healing %in% c(1,2) |
                          RegData$c_stricturer %in% c(1,2) | RegData$c_nervedamage %in% c(1,2) |
                          RegData$c_bloodpoisoning %in% c(1,2) | RegData$c_bleeding %in% c(1,2) |
                          RegData$c_other_complications %in% c(1,2)), ]
    N <- dim(RegData)[1]
    RegData$c_infection[RegData$c_infection == 2] <- 0
    RegData$c_infection[RegData$c_infection == 3] <- NA
    RegData$c_delayed_wound_healing[RegData$c_delayed_wound_healing == 2] <- 0
    RegData$c_delayed_wound_healing[RegData$c_delayed_wound_healing == 3] <- NA
    RegData$c_stricturer[RegData$c_stricturer == 2] <- 0
    RegData$c_stricturer[RegData$c_stricturer == 3] <- NA
    RegData$c_nervedamage[RegData$c_nervedamage == 2] <- 0
    RegData$c_nervedamage[RegData$c_nervedamage == 3] <- NA
    RegData$c_bloodpoisoning[RegData$c_bloodpoisoning == 2] <- 0
    RegData$c_bloodpoisoning[RegData$c_bloodpoisoning == 3] <- NA
    RegData$c_bleeding[RegData$c_bleeding == 2] <- 0
    RegData$c_bleeding[RegData$c_bleeding == 3] <- NA
    RegData$c_other_complications[RegData$c_other_complications == 2] <- 0
    RegData$c_other_complications[RegData$c_other_complications == 3] <- NA
    RegData$c_ingen_kompl <- NA
    RegData$c_ingen_kompl[which(rowSums(RegData[, c("c_infection", "c_delayed_wound_healing", "c_stricturer",
                                                    "c_nervedamage", 'c_bloodpoisoning', 'c_bleeding',
                                                    'c_other_complications')], na.rm = TRUE)==0)] <- 1
    RegData$c_ingen_kompl[which(rowSums(RegData[, c("c_infection", "c_delayed_wound_healing", "c_stricturer",
                                                    "c_nervedamage", 'c_bloodpoisoning', 'c_bleeding',
                                                    'c_other_complications')], na.rm = TRUE)>0)] <- 0
    tittel <- 'Komplikasjoner ved kirugisk behandling'
    AntVar <- colSums(RegData[, c("c_infection", "c_delayed_wound_healing",
                                  "c_stricturer", "c_nervedamage", 'c_bloodpoisoning',
                                  'c_bleeding', 'c_other_complications', 'c_ingen_kompl')], na.rm = TRUE)
    NVar<-apply(RegData[, c("c_infection", "c_delayed_wound_healing",
                            "c_stricturer", "c_nervedamage", 'c_bloodpoisoning',
                            'c_bleeding', 'c_other_complications', 'c_ingen_kompl')], 2, function(x){length(which(!is.na(x)))})
    grtxt <- c('Infeksjon i\n operasjonssår', 'Forsinket \nsårtilheling', 'Strikturer', 'Nerveskader',
               'Blodforgiftning\n (Sepsis)', 'Blødning', 'Andre \nkomplikasjoner', 'Ingen komplikasjoner')
    retn <- 'H'
  }

  if (valgtVar== 'KomplKir_v2') { # NY
    RegData <- RegData[which(RegData$ForlopsType1Num %in% c(1,3)), ]
    RegData <- RegData[which(RegData$OppflgRegStatus >= 1), ]
    # RegData <- RegData[, ]
    RegData <- RegData[which(RegData$INFECTION_kir %in% c(0,1) | RegData$DELAYED_WOUND_HEALING_kir %in% c(0,1) |
                               RegData$STRIKTURER_kir %in% c(0,1) | RegData$NERVEDAMAGE_kir %in% c(0,1) |
                               RegData$BLOODPOISEN_kir %in% c(0,1) | RegData$BLEEDING_kir %in% c(0,1) |
                               RegData$OTHER_COMPLICATIONS_ID_kir %in% c(0,1)), ]
    N <- dim(RegData)[1]
    RegData$INFECTION_kir[RegData$INFECTION_kir == 9] <- NA
    RegData$DELAYED_WOUND_HEALING_kir[RegData$DELAYED_WOUND_HEALING_kir == 9] <- NA
    RegData$STRIKTURER_kir[RegData$STRIKTURER_kir == 9] <- NA
    RegData$NERVEDAMAGE_kir[RegData$NERVEDAMAGE_kir == 9] <- NA
    RegData$BLOODPOISEN_kir[RegData$BLOODPOISEN_kir == 9] <- NA
    RegData$BLEEDING_kir[RegData$BLEEDING_kir == 9] <- NA
    RegData$OTHER_COMPLICATIONS_ID_kir[RegData$OTHER_COMPLICATIONS_ID_kir == 9] <- NA
    RegData$c_ingen_kompl <- NA
    RegData$c_ingen_kompl[which(rowSums(RegData[, c("INFECTION_kir", "DELAYED_WOUND_HEALING_kir", "STRIKTURER_kir",
                                                    "NERVEDAMAGE_kir", "BLOODPOISEN_kir", "BLEEDING_kir",
                                                    "OTHER_COMPLICATIONS_ID_kir")], na.rm = TRUE)==0)] <- 1
    RegData$c_ingen_kompl[which(rowSums(RegData[, c("INFECTION_kir", "DELAYED_WOUND_HEALING_kir", "STRIKTURER_kir",
                                                    "NERVEDAMAGE_kir", "BLOODPOISEN_kir", "BLEEDING_kir",
                                                    "OTHER_COMPLICATIONS_ID_kir")], na.rm = TRUE)>0)] <- 0
    tittel <- 'Komplikasjoner ved kirugisk behandling'
    AntVar <- colSums(RegData[, c("INFECTION_kir", "DELAYED_WOUND_HEALING_kir", "STRIKTURER_kir",
                                  "NERVEDAMAGE_kir", "BLOODPOISEN_kir", "BLEEDING_kir",
                                  "OTHER_COMPLICATIONS_ID_kir", 'c_ingen_kompl')], na.rm = TRUE)
    NVar<-apply(RegData[, c("INFECTION_kir", "DELAYED_WOUND_HEALING_kir", "STRIKTURER_kir",
                            "NERVEDAMAGE_kir", "BLOODPOISEN_kir", "BLEEDING_kir",
                            "OTHER_COMPLICATIONS_ID_kir", 'c_ingen_kompl')], 2, function(x){length(which(!is.na(x)))})
    grtxt <- c('Infeksjon i\n operasjonssår', 'Forsinket \nsårtilheling', 'Strikturer', 'Nerveskader',
               'Blodforgiftning\n (Sepsis)', 'Blødning', 'Andre \nkomplikasjoner', 'Ingen komplikasjoner')
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
    tittel <- 'Tidligere behandling ved inklusjon'
  }

  if (valgtVar=='TidlBeh_v2') {
    SamletPrPID <- RegData %>% group_by(PasientID) %>%
      summarise(Kirurgisk = if (1 %in% SURGERY) {1} else {if (0 %in% SURGERY) {0} else {9}},
                Medisinsk = if (1 %in% MEDICATION_HISTORY_HS) {1} else
                  {if (0 %in% MEDICATION_HISTORY_HS) {0} else {9}},
                Begge = if (Kirurgisk == 1 & Medisinsk == 1) {1} else {0},
                Ingen = if (Kirurgisk == 0 & Medisinsk == 0) {1} else {0},
                N = n())
    SamletPrPID$Kirurgisk[SamletPrPID$Begge == 1] <- 0
    SamletPrPID$Medisinsk[SamletPrPID$Begge == 1] <- 0
    SamletPrPID$Variabel <- NA
    SamletPrPID$Variabel[SamletPrPID$Kirurgisk == 1] <- 1
    SamletPrPID$Variabel[SamletPrPID$Medisinsk == 1] <- 2
    SamletPrPID$Variabel[SamletPrPID$Begge == 1] <- 3
    SamletPrPID$Variabel[SamletPrPID$Ingen == 1] <- 4
    SamletPrPID$Variabel[is.na(SamletPrPID$Variabel)] <- 5
    RegData <- RegData[order(RegData$HovedDato, decreasing = F), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData <- merge(RegData, SamletPrPID[, c("PasientID", "Variabel")], by = "PasientID")
    gr <- 1:5
    grtxt <- c("Kun kirurgisk", "Kun medisinsk", "Kirurgisk og \n medisinsk", "Ingen", "Ukjent")
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    retn <- 'H'
    tittel <- 'Tidligere behandling \n ved inklusjon'
  }

#   if (valgtVar=='Anamnese') {
#
#
#
#   }

  if (valgtVar=='DLQI_PrePost') {
    RegData$VarPre <- cut(RegData$VarPre, breaks=c(0,10,20,30), include.lowest=TRUE, right=FALSE, labels = 1:3)
    RegData$VarPost <- cut(RegData$VarPost, breaks=c(0,10,20,30), include.lowest=TRUE, right=FALSE, labels = 1:3)
    # RegData$VarPre <- cut(RegData$VarPre, breaks=c(0,10,20,30), include.lowest=TRUE, right=FALSE, labels = F)
    # RegData$VarPost <- cut(RegData$VarPost, breaks=c(0,10,20,30), include.lowest=TRUE, right=FALSE, labels = F)
    tittel <- c('DLQI alvorlighetsgrad før og etter intervensjon')
    grtxt <- c('Mild', 'Moderat', 'Alvorlig')
    grtxt2 <- c('DLQI<10', '10<=DLQI<20', 'DLQI>=20')
    subtxt <- 'Alvorlighetsgrad'
    cexgr <- 0.8
  }

  if (valgtVar=='DLQISUM_PrePost') { # NY
    RegData$VarPre <- cut(RegData$VarPre, breaks=c(0,10,20,30), include.lowest=TRUE, right=FALSE, labels = 1:3)
    RegData$VarPost <- cut(RegData$VarPost, breaks=c(0,10,20,30), include.lowest=TRUE, right=FALSE, labels = 1:3)
    # RegData$VarPre <- cut(RegData$VarPre, breaks=c(0,10,20,30), include.lowest=TRUE, right=FALSE, labels = F)
    # RegData$VarPost <- cut(RegData$VarPost, breaks=c(0,10,20,30), include.lowest=TRUE, right=FALSE, labels = F)
    tittel <- c('DLQI alvorlighetsgrad før og etter intervensjon')
    grtxt <- c('Mild', 'Moderat', 'Alvorlig')
    grtxt2 <- c('DLQI<10', '10<=DLQI<20', 'DLQI>=20')
    subtxt <- 'Alvorlighetsgrad'
    cexgr <- 0.8
  }

  if (valgtVar=='Hurley_PrePost') {
    RegData$VarPre <- as.factor(RegData$VarPre)
    RegData$VarPost <- as.factor(RegData$VarPost)
    tittel <- c('Hurley-score før og etter behandling')
    grtxt <- c('Stadium I', 'Stadium II', 'Stadium III')
    subtxt <- 'Hurley-score'
    cexgr <- 0.8
  }

  if (valgtVar=='HURLEY_SCORE_PrePost') {
    RegData$VarPre <- as.factor(RegData$VarPre)
    RegData$VarPost <- as.factor(RegData$VarPost)
    tittel <- c('Hurley-score før og etter behandling')
    grtxt <- c('Stadium I', 'Stadium II', 'Stadium III')
    subtxt <- 'Hurley-score'
    cexgr <- 0.8
  }


  if (valgtVar=='pre_hurley_score') {
    RegData$Variabel <- RegData[[valgtVar]]
    gr <- 1:3
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = F), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- c('Hurley-score ved inklusjon')
    grtxt <- as.character(gr)
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    subtxt <- 'Hurley-score'
  }

  if (valgtVar=='HURLEY_SCORE') {
    RegData$Variabel <- RegData[[valgtVar]]
    gr <- 1:3
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = F), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- c('Hurley-score ved inklusjon')
    grtxt <- as.character(gr)
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    subtxt <- 'Hurley-score'
  }

  if (valgtVar=='pre_bmi') {
    RegData$Variabel <- RegData[[valgtVar]]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = T), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    gr <- c(0,16,17,17.5,18.5,25,30,35,40,50)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)

    tittel <- c('BMI', paste0('Median: ', median(RegData$Variabel, na.rm = T), ' Gj.snitt: ', round(mean(RegData$Variabel, na.rm = T), 1)))
    subtxt <- expression(BMI (kg/m^2))
    grtxt <- c('Alvorlig undervekt\n (<16)','Undervekt\n (16-17)','Mild undervekt\n (17-17.5)','Normal\n (18.5-25)','Overvekt\n (25-30)',
               'Moderat fedme,\n klasse I (30-35)','Fedme, klasse II\n (35-40)','Fedme, klasse III\n (40-50)')
    retn <- 'H'
  }

  if (valgtVar=='BMI') { # NY
    RegData$Variabel <- RegData[[valgtVar]]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = T), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    gr <- c(0,16,17,17.5,18.5,25,30,35,40,50)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)

    tittel <- c('BMI', paste0('Median: ', median(RegData$Variabel, na.rm = T), ' Gj.snitt: ', round(mean(RegData$Variabel, na.rm = T), 1)))
    subtxt <- expression(BMI (kg/m^2))
    grtxt <- c('Alvorlig undervekt\n (<16)','Undervekt\n (16-17)','Mild undervekt\n (17-17.5)','Normal\n (18.5-25)','Overvekt\n (25-30)',
               'Moderat fedme,\n klasse I (30-35)','Fedme, klasse II\n (35-40)','Fedme, klasse III\n (40-50)')
    retn <- 'H'
  }


  PlotParams <- list(RegData=RegData, tittel=tittel, grtxt=grtxt, grtxt2=grtxt2, subtxt=subtxt,
                     retn=retn, cexgr=cexgr, AntVar=AntVar, NVar=NVar)

  return(invisible(PlotParams))
}
