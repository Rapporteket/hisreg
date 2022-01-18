#------------tittel og logo-------
addResourcePath("rap", system.file("www", package = "rapbase"))
regTitle <-  "RAPPORTEKET FOR HISREG"
logo <- includeHTML(system.file("www/logo.svg", package = "rapbase"))
logoCode <- paste0("var header = $('.navbar> .container-fluid');\n",
                   "header.append('<div class=\"navbar-brand\"
                   style=\"float:left;font-size:75%\">",
                   logo,
                   "</div>');\n",
                   "console.log(header)")
logoWidget <- tags$script(shiny::HTML(logoCode))


#------------------Data-------------------------------------

if (rapbase::isRapContext()) {
  ForlopsData <- hisreg::hisregHentTabell("ForlopsOversikt_v1")
  RegData <- hisreg::hisregHentTabell("AlleVarNum")
  Followups <- hisreg::hisregHentTabell("FollowupsNum")
  SkjemaOversikt <- hisreg::hisregHentTabell("SkjemaOversikt_v1")

  hisreg_old<-list(RegData=RegData, ForlopsData=ForlopsData, Followups=Followups)

  RegData_gml <- hisreg::hisregKonverterData(RegData=RegData, ForlopsData=ForlopsData, Followups=Followups)
  RegData_gml <- RegData_gml[RegData_gml$AvdRESH != 999002, ] # Fjerner Roskilde


  preinterventiondoctor <- hisreg::hisregHentTabell("preinterventiondoctor")
  registration <- hisreg::hisregHentTabell("registration")
  intervention <- hisreg::hisregHentTabell("intervention")
  mce <- hisreg::hisregHentTabell("mce")
  mcelist <- hisreg::hisregHentTabell("mcelist")
  patient <- hisreg::hisregHentTabell("patient")
  patientcontrol <- hisreg::hisregHentTabell("patientcontrol")
  preintervention <- hisreg::hisregHentTabell("preintervention")
  centre <- hisreg::hisregHentTabell("centre")
  doctorcontrol <- hisreg::hisregHentTabell("doctorcontrol")
  ForlopsOversikt_ny <- hisreg::hisregHentTabell("ForlopsOversikt")
  SkjemaOversikt_ny <- hisreg::hisregHentTabell("SkjemaOversikt")

  names(ForlopsOversikt_ny)[names(ForlopsOversikt_ny)=="erMann"] <- "ErMann"

  SkjemaOversikt_ny$HovedDato <- as.Date(SkjemaOversikt_ny$HovedDato)
  SkjemaOversikt_ny$Skjemanavn <- as.factor(SkjemaOversikt_ny$Skjemanavn)
  SkjemaOversikt_ny <- merge(SkjemaOversikt_ny,
                             ForlopsOversikt_ny[, c("ForlopsID", "ForlopsType1", "ForlopsType1Num")],
                             by = "ForlopsID", all.x = T)
  preinterventiondoctor <- preinterventiondoctor[preinterventiondoctor$STATUS == 1, ]
  registration <- registration[registration$STATUS == 1, ]
  intervention <- intervention[intervention$STATUS == 1, ]
  mcelist <- mcelist[mcelist$REGISTRATION_STATUS == 1, ]
  patientcontrol <- patientcontrol[patientcontrol$STATUS == 1, ]
  preintervention <- preintervention[preintervention$STATUS == 1, ]
  doctorcontrol <- doctorcontrol[doctorcontrol$STATUS == 1, ]
  oppfolging_kir <- merge(patientcontrol[patientcontrol$CONTROLTYPE == 1, ],
                          doctorcontrol[doctorcontrol$CONTROLTYPE == 1, ],
                          by = "MCEID",  suffixes = c("_pas", "_dokt"), all = TRUE)
  oppfolging_med <- merge(patientcontrol[patientcontrol$CONTROLTYPE == 2, ],
                          doctorcontrol[doctorcontrol$CONTROLTYPE == 2, ],
                          by = "MCEID",  suffixes = c("_pas", "_dokt"), all = TRUE)
  oppfolging <- merge(oppfolging_kir, oppfolging_med, by = "MCEID", suffixes = c("_kir", "_med"), all = T)

  allevar <- merge(preintervention, preinterventiondoctor[!(names(preinterventiondoctor) %in%
                                                              intersect(names(preintervention), names(preinterventiondoctor)[-1]))], by = "MCEID")
  allevar <- merge(allevar, intervention[, !(names(intervention) %in% intersect(names(allevar), names(intervention)[-1]))], by = "MCEID")
  allevar <- merge(allevar, registration[, !(names(registration) %in% intersect(names(allevar), names(registration)[-1]))], by = "MCEID")
  allevar <- merge(allevar, oppfolging, by = "MCEID", suffixes = c("", "_oppf"), all.x = T)
  allevar$Sykehusnavn <- centre$CENTRESHORTNAME[match(allevar$CENTREID, centre$ID)]
  allevar <- merge(allevar, ForlopsOversikt_ny[, c("ForlopsID", "AvdRESH", "BasisRegStatus", "HovedDato",
                                                   "PasientAlder", "OppflgRegStatus", "ForlopsType1", "ForlopsType1Num",
                                                   "ForlopsType2", "ForlopsType2Num", "ErMann", "SykehusNavn", "PasientID",
                                                   "UtdanningSSB", "KryptertFnr")],
                   by.x = "MCEID", by.y = "ForlopsID")
  allevar$BMI <- allevar$WEIGHT/(allevar$HIGHT/100)^2
  allevar$MCEID <- allevar$MCEID + 10000
  allevar$PasientID <- as.numeric(allevar$PasientID) + 10000
  # Der det finnes et KryptertFnr i nytt datasett som matcher KryptertFnr i gammelt
  # datasett: Bruk PasientID fra gammelt
  allevar$PasientID[!is.na(RegData_gml$PasientID[match(allevar$KryptertFnr, RegData_gml$KryptertFnr)])] <-
    RegData_gml$PasientID[match(allevar$KryptertFnr,
                                RegData_gml$KryptertFnr)][!is.na(RegData_gml$PasientID[match(allevar$KryptertFnr,
                                                                                             RegData_gml$KryptertFnr)])]

  RegData_ny <- allevar[, c("MCEID", "AGE_ABSCESS", "UtdanningSSB", "SURGERY", "MEDICATION_HISTORY_HS",
                            "SMOKING", "WORK", "BMI", "DLQISUM", "VASSCORE", "HURLEY_SCORE", "TYPE_INTERVENTION",
                            "SURGERY_TYPE_EXCISION_SUTUR", "SURGERY_TYPE_EXCISION_GRANUL", "SURGERY_TYPE_EXCISION_TRANSPLANT",
                            "SURGERY_TYPE_LASER", "SURGERY_TYPE_DEROOFING", "SURGERY_TYPE_OTHER", "BIOLOGICAL_TREATMENT",
                            "SYSTEMIC_ANTIBIOTIC_THERAPY", "ANTIINFLAMMATORY_TREATMENT", "ANALGESICS", "BASED_MEDICAL_TREATMENT",
                            "AKSILLE", "LYSKE", "PUBIS", "GENITALT", "PERIALT", "GLUTEALT", "MAMMAE", "OTHER_SURGERY",
                            "RIFAMPICIN_CLINDAMYCIN", "TETRACYCLIN_LYMECYCLIN", "AMOXICILIN", "SYSTEMIC_ANTIBIOTIC_THERAPY_OTHER",
                            "INTRALESIONAL_CORTICOSTEROID_INJECTION", "ACELACIC_ACID", "CLINDAMYCIN", "RESORCINOL",
                            "AvdRESH", "KryptertFnr", "BasisRegStatus", "HovedDato",
                            "PasientAlder", "OppflgRegStatus", "ForlopsType1", "ForlopsType1Num",
                            "ForlopsType2", "ForlopsType2Num", "ErMann", "SykehusNavn", "PasientID",
                            "INFECTION_kir", "INFECTION_med", "DELAYED_WOUND_HEALING_kir", "DELAYED_WOUND_HEALING_med",
                            "STRIKTURER_kir", "STRIKTURER_med", "NERVEDAMAGE_kir", "NERVEDAMAGE_med",
                            "BLOODPOISEN_kir", "BLOODPOISEN_med", "BLEEDING_kir", "BLEEDING_med",
                            "OTHER_COMPLICATIONS_ID_kir", "OTHER_COMPLICATIONS_ID_med", "DLQISUM_kir", "DLQISUM_med",
                            "VASSCORE_kir", "VASSCORE_med", "HURLEY_SCORE_kir", "HURLEY_SCORE_med", "ADALIMUMAB_BIOSIMILAR",
                            "INFLIXIMAB_BIOSIMILAR", "BIOLOGICAL_TREATMENT_OTHER", "ACITRETIN", "DAPSON", "CICLOSPORIN",
                            "PREDNISOLON", "ISOTRETINOIN", "METFORMIN", "ANTIINFLAMMATORY_TREATMENT_OTHER", "IHS4SCORE",
                            "IHS4SCORE_kir", "IHS4SCORE_med", "AGE_SPECIALIST", "AGE_DOCTOR", "TIME_SPECIALIST",
                            "CONTROL_DATE_dokt_kir", "CONTROL_DATE_dokt_med")]

  shus <- data.frame(AvdRESH = unique(RegData_ny$AvdRESH),
                     SykehusNavn = RegData_ny$SykehusNavn[match(unique(RegData_ny$AvdRESH), RegData_ny$AvdRESH)])
  shus$SykehusNavn <- trimws(shus$SykehusNavn)
  RegData_ny <- RegData_ny %>% mutate_at(c("BasisRegStatus", 'ErMann'), as.numeric)
  RegData <- dplyr::bind_rows(RegData_gml, RegData_ny)
  RegData$SykehusNavn <- shus$SykehusNavn[match(RegData$AvdRESH, shus$AvdRESH)]

  RegData$c_date <- NA;
  RegData$c_date[RegData$ForlopsType1Num %in% c(1,3)] <-
    RegData$CONTROL_DATE_dokt_kir[RegData$ForlopsType1Num %in% c(1,3)]
  RegData$c_date[which(RegData$ForlopsType1Num == 2)] <-
    RegData$CONTROL_DATE_dokt_med[which(RegData$ForlopsType1Num == 2)]
  RegData$c_date[is.na(RegData$c_date) & !is.na(RegData$CONTROL_DATE_dokt_kir)] <-
    RegData$CONTROL_DATE_dokt_kir[is.na(RegData$c_date) & !is.na(RegData$CONTROL_DATE_dokt_kir)]
  RegData$c_date[is.na(RegData$c_date) & !is.na(RegData$CONTROL_DATE_dokt_med)] <-
    RegData$CONTROL_DATE_dokt_med[is.na(RegData$c_date) & !is.na(RegData$CONTROL_DATE_dokt_med)]

  RegData$DLQISUM_POST <- NA;
  RegData$DLQISUM_POST[RegData$ForlopsType1Num %in% c(1,3)] <-
    RegData$DLQISUM_kir[RegData$ForlopsType1Num %in% c(1,3)]
  RegData$DLQISUM_POST[which(RegData$ForlopsType1Num == 2)] <-
    RegData$DLQISUM_med[which(RegData$ForlopsType1Num == 2)]

  RegData$HURLEY_SCORE_POST <- NA;
  RegData$HURLEY_SCORE_POST[RegData$ForlopsType1Num %in% c(1,3)] <-
    RegData$HURLEY_SCORE_kir[RegData$ForlopsType1Num %in% c(1,3)]
  RegData$HURLEY_SCORE_POST[which(RegData$ForlopsType1Num == 2)] <-
    RegData$HURLEY_SCORE_med[which(RegData$ForlopsType1Num == 2)]

  RegData$VASSCORE_POST <- NA;
  RegData$VASSCORE_POST[RegData$ForlopsType1Num %in% c(1,3)] <-
    RegData$VASSCORE_kir[RegData$ForlopsType1Num %in% c(1,3)]
  RegData$VASSCORE_POST[which(RegData$ForlopsType1Num == 2)] <-
    RegData$VASSCORE_med[which(RegData$ForlopsType1Num == 2)]

  RegData$IHS4SCORE_POST <- NA;
  RegData$IHS4SCORE_POST[RegData$ForlopsType1Num %in% c(1,3)] <-
    RegData$IHS4SCORE_kir[RegData$ForlopsType1Num %in% c(1,3)]
  RegData$IHS4SCORE_POST[which(RegData$ForlopsType1Num == 2)] <-
    RegData$IHS4SCORE_med[which(RegData$ForlopsType1Num == 2)]

  rm(list = c("allevar", "centre", "doctorcontrol", "Followups", "ForlopsData",
              "ForlopsOversikt_ny", "intervention", "mce", "mcelist", "oppfolging",
              "oppfolging_kir", "oppfolging_med", "patient", "patientcontrol",
              "preintervention", "preinterventiondoctor", "registration",
              "RegData_gml", "RegData_ny", "shus"))

  RegData <- RegData[RegData$BasisRegStatus==1, ]
  RegData$HovedDato <- as.Date(RegData$HovedDato, format="%Y-%m-%d") # Ordne datoformat
  RegData <- RegData[!is.na(RegData$HovedDato), ]
  RegData$Aar <- as.numeric(format(RegData$HovedDato, "%Y"))
  RegData$SykehusNavn <- as.factor(RegData$SykehusNavn)

  RegData$Intervensjon <- NA
  RegData$Intervensjon[RegData$ForlopsType1=="Kirurgisk intervensjon"] <- 1
  RegData$Intervensjon[RegData$ForlopsType1=="Medisinsk intervensjon"] <- 2
  RegData$Intervensjon[RegData$ForlopsType1=="Kirurgisk og medisinsk intervensjon"] <- 3
  RegData$Intervensjon <- factor(RegData$Intervensjon, levels = 1:3, labels = c('Kirugisk', 'Medisinsk', 'Kir. og. med.'))

}

# RegData <- hisreg::hisregPreprosess(RegData) # tas ut foreløpig, mye av koden ovenfor skal inn etter hvert
# Roskilde skal ikke inngå blant de nasjonale tallene.
RegData <- RegData[RegData$AvdRESH != 999002, ]




#------------------Variabel valg-------------------------------------

avdValg <- unique(RegData$AvdRESH)
names(avdValg) <- unique(RegData$SykehusNavn)

varValgFordeling <- c("Alder" = "PasientAlder",
                      "Alder ved første byll" = "AGE_ABSCESS",
                      "Kjønn" = "ErMann",
                      "Utdanning" = "UtdanningSSB",
                      "Røykevaner" = "SMOKING",
                      "Arbeidsstatus preintervensjon" = "WORK",
                      "Type intervensjon" = "ForlopsType1Num",
                      "Type Kirurgi" = "type_kirurgi",
                      "Biologiske legemidler" = "BiologiskBeh",
                      "Type medisinsk behandling" = "MedisinskBeh_v2",
                      # "Lokalisasjon av kirurgisk inngrep" =
                      # "KirurgiLokalisering",
                      "Komplikasjoner kirurgi" = "KomplKir_v2",
                      "Tidligere behandling" = "TidlBeh_v2",
                      "BMI" = "BMI",
                      "Hurley-score ved inklusjon" = "HURLEY_SCORE")

typInt <- c("---" = 99,
            "Kirurgisk" = 1,
            "Medisinsk" = 2,
            "Kirurgisk og medisinsk" = 3)
typInt1 <- c("Kirurgisk" = 1,
            "Medisinsk" = 2,
            "Kirurgisk og medisinsk" = 3,
            "Ingen intervensjon" = 99)
kjoenn <- c("Begge kjønn" = 99,
            "Kvinne" = 0,
            "Mann" = 1)

varValgGjenPer <- c("BMI, preintervensjon" = "BMI",
                    "DLQI-sum, preintervensjon" = "DLQISUM",
                    "VAS-score, preintervensjon" = "VASSCORE",
                    "IHS4-score, preintervensjon" = "IHS4SCORE")

varValgGjenFE <- c("DLQI" = "DLQI_PrePost_ny",
                    "IHS4-score" = "IHS4SCORE",
                    "VAS" = "Vas_PrePost_ny")
