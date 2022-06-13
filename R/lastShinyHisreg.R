#' Last og tilrettelegg shiny-data
#'
#' Hvis det eksiterer, hent ferdig koblet og tilrettelagt data fra staging. Ellers
#' hent fra db og gjør nødvendige koblinger/mappinger.
#'
#' @return En liste med data til bruk i hisreg sin shiny-app
#' @export
lastShinyHisreg <- function() {

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
  allevar$IMPOSSIBLE_REASON_med <- pmax(allevar$IMPOSSIBLE_REASON_pas_med, allevar$IMPOSSIBLE_REASON_dokt_med, na.rm = T)
  allevar$IMPOSSIBLE_REASON_kir <- pmax(allevar$IMPOSSIBLE_REASON_pas_kir, allevar$IMPOSSIBLE_REASON_dokt_kir, na.rm = T)

  RegData_ny <- allevar[, c("MCEID", "AGE_ABSCESS", "UtdanningSSB", "SURGERY", "MEDICATION_HISTORY_HS",
                            "SMOKING", "SMOKING_med", "SMOKING_kir", "WORK", "WORKSTATUS_med", "WORKSTATUS_kir",
                            "BMI", "DLQISUM", "VASSCORE", "HURLEY_SCORE", "TYPE_INTERVENTION",
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
                            "CONTROL_DATE_dokt_kir", "CONTROL_DATE_dokt_med", "SATISFACTION_kir", "SATISFACTION_med",
                            "RETREATMENT_kir", "RETREATMENT_med", "RECOMMENDATION_kir", "RECOMMENDATION_med",
                            "IMPOSSIBLE_REASON_med", "IMPOSSIBLE_REASON_kir", "HISCR_kir", "HISCR_med",
                            "HISCR_CATEGORY_kir", "HISCR_CATEGORY_med", "TREATMENT_med", "SEPONERT_med", "CONTROL_POSSIBLE_pas_med",
                            "SEPONERT_BIVIRKNING_med", "ABSCESS", "ABSCESS_kir", "ABSCESS_med", "FAMILY_ILLNES",
                            "MEDICATION_HISTORY_ANTIBIOTIC")]

  shus <- data.frame(AvdRESH = unique(RegData_ny$AvdRESH),
                     SykehusNavn = RegData_ny$SykehusNavn[match(unique(RegData_ny$AvdRESH), RegData_ny$AvdRESH)])
  shus$SykehusNavn <- trimws(shus$SykehusNavn)
  RegData_ny <- RegData_ny %>% dplyr::mutate_at(c("BasisRegStatus", 'ErMann'), as.numeric)
  RegData <- dplyr::bind_rows(RegData_gml, RegData_ny)
  RegData$SykehusNavn <- shus$SykehusNavn[match(RegData$AvdRESH, shus$AvdRESH)]

  RegData$c_date <- NA;
  RegData$c_date[RegData$ForlopsType1Num %in% c(1,3)] <-
    format(RegData$CONTROL_DATE_dokt_kir[RegData$ForlopsType1Num %in% c(1,3)], "%Y-%m-%d")
    # RegData$CONTROL_DATE_dokt_kir[RegData$ForlopsType1Num %in% c(1,3)]
  RegData$c_date[which(RegData$ForlopsType1Num == 2)] <-
    format(RegData$CONTROL_DATE_dokt_med[RegData$ForlopsType1Num %in% 2], "%Y-%m-%d")
    # RegData$CONTROL_DATE_dokt_med[which(RegData$ForlopsType1Num == 2)]
  RegData$c_date[is.na(RegData$c_date) & !is.na(RegData$CONTROL_DATE_dokt_kir)] <-
    format(RegData$CONTROL_DATE_dokt_kir[is.na(RegData$c_date) & !is.na(RegData$CONTROL_DATE_dokt_kir)], "%Y-%m-%d")
    # RegData$CONTROL_DATE_dokt_kir[is.na(RegData$c_date) & !is.na(RegData$CONTROL_DATE_dokt_kir)]
  RegData$c_date[is.na(RegData$c_date) & !is.na(RegData$CONTROL_DATE_dokt_med)] <-
    format(RegData$CONTROL_DATE_dokt_med[is.na(RegData$c_date) & !is.na(RegData$CONTROL_DATE_dokt_med)], "%Y-%m-%d")
    # RegData$CONTROL_DATE_dokt_med[is.na(RegData$c_date) & !is.na(RegData$CONTROL_DATE_dokt_med)]
  RegData$c_date <- as.Date(RegData$c_date)

  RegData$HISCR_CATEGORY <- NA;
  RegData$HISCR_CATEGORY[RegData$ForlopsType1Num %in% c(1,3)] <-
    RegData$HISCR_CATEGORY_kir[RegData$ForlopsType1Num %in% c(1,3)]
  RegData$HISCR_CATEGORY[which(RegData$ForlopsType1Num == 2)] <-
    RegData$HISCR_CATEGORY_med[which(RegData$ForlopsType1Num == 2)]

  RegData$HISCR <- NA;
  RegData$HISCR[RegData$ForlopsType1Num %in% c(1,3)] <-
    RegData$HISCR_kir[RegData$ForlopsType1Num %in% c(1,3)]
  RegData$HISCR[which(RegData$ForlopsType1Num == 2)] <-
    RegData$HISCR_med[which(RegData$ForlopsType1Num == 2)]

  RegData$IMPOSSIBLE_REASON <- NA;
  RegData$IMPOSSIBLE_REASON[RegData$ForlopsType1Num %in% c(1,3)] <-
    RegData$IMPOSSIBLE_REASON_kir[RegData$ForlopsType1Num %in% c(1,3)]
  RegData$IMPOSSIBLE_REASON[which(RegData$ForlopsType1Num == 2)] <-
    RegData$IMPOSSIBLE_REASON_med[which(RegData$ForlopsType1Num == 2)]

  RegData$RETREATMENT <- NA;
  RegData$RETREATMENT[RegData$ForlopsType1Num %in% c(1,3)] <-
    RegData$RETREATMENT_kir[RegData$ForlopsType1Num %in% c(1,3)]
  RegData$RETREATMENT[which(RegData$ForlopsType1Num == 2)] <-
    RegData$RETREATMENT_med[which(RegData$ForlopsType1Num == 2)]

  RegData$RECOMMENDATION <- NA;
  RegData$RECOMMENDATION[RegData$ForlopsType1Num %in% c(1,3)] <-
    RegData$RECOMMENDATION_kir[RegData$ForlopsType1Num %in% c(1,3)]
  RegData$RECOMMENDATION[which(RegData$ForlopsType1Num == 2)] <-
    RegData$RECOMMENDATION_med[which(RegData$ForlopsType1Num == 2)]

  RegData$SATISFACTION <- NA;
  RegData$SATISFACTION[RegData$ForlopsType1Num %in% c(1,3)] <-
    RegData$SATISFACTION_kir[RegData$ForlopsType1Num %in% c(1,3)]
  RegData$SATISFACTION[which(RegData$ForlopsType1Num == 2)] <-
    RegData$SATISFACTION_med[which(RegData$ForlopsType1Num == 2)]

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

  RegData$SMOKING_POST <- NA;
  RegData$SMOKING_POST[RegData$ForlopsType1Num %in% c(1,3)] <-
    RegData$SMOKING_kir[RegData$ForlopsType1Num %in% c(1,3)]
  RegData$SMOKING_POST[which(RegData$ForlopsType1Num == 2)] <-
    RegData$SMOKING_med[which(RegData$ForlopsType1Num == 2)]

  RegData$ABSCESS_POST <- NA;
  RegData$ABSCESS_POST[RegData$ForlopsType1Num %in% c(1,3)] <-
    RegData$ABSCESS_kir[RegData$ForlopsType1Num %in% c(1,3)]
  RegData$ABSCESS_POST[which(RegData$ForlopsType1Num == 2)] <-
    RegData$ABSCESS_med[which(RegData$ForlopsType1Num == 2)]

  RegData$WORKSTATUS_POST <- NA;
  RegData$WORKSTATUS_POST[RegData$ForlopsType1Num %in% c(1,3)] <-
    RegData$WORKSTATUS_kir[RegData$ForlopsType1Num %in% c(1,3)]
  RegData$WORKSTATUS_POST[which(RegData$ForlopsType1Num == 2)] <-
    RegData$WORKSTATUS_med[which(RegData$ForlopsType1Num == 2)]


  rm(list = c("allevar", "centre", "doctorcontrol", "Followups", "ForlopsData",
              "ForlopsOversikt_ny", "intervention", "mce", "mcelist", "oppfolging",
              "oppfolging_kir", "oppfolging_med", "patient", "patientcontrol",
              "preintervention", "preinterventiondoctor", "registration",
              "RegData_gml", "RegData_ny", "shus", "hisreg_old"))

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
  RegData <- RegData[RegData$AvdRESH != 999002, ]

  return(invisible(list(RegData=RegData, SkjemaOversikt=SkjemaOversikt, SkjemaOversikt_ny=SkjemaOversikt_ny)))
}

