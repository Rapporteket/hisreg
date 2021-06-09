#' Tilpass gammelt datasett til nytt
#'
#' Denne funksjonen konverterer navn og verdier i gamle Hisreg til navn og verdier i det nye
#'
#' @inheritParams hisregFigAndeler
#'
#' @return RegData En dataramme med konvertert datasett
#'
#' @export

hisregKonverterData <- function(RegData, ForlopsData, Followups) {
  # Beholder nødvendige variabler
  RegData <- RegData[, c("m_mceid", "p_age_abscess",
                         "p_education", "p_surgery",
                         "p_antibiotics", "pre_smoking",
                         "pre_work", "pre_bmi", "pre_dlqisum",
                         "pre_vasscore",
                         "pre_hurley_score", "i_type",
                         "i_surgery_type",
                         "i_biological_treatment",
                         "i_antibiotic_therapy",
                         "i_antiinflammatory_treatment",
                         "i_analgesics",
                         "i_localized_med_treatment",
                         "i_aksille", "i_lyske", "i_pubis",
                         "i_genitalt", "i_peritalt", "i_glutealt",
                         "i_mammae", "i_other_location",
                         "i_rifampicin_clindamycin",
                         "i_tetracyclin_lymecyclin",
                         "i_amoxicilin", "i_sys_antibiotic_other",
                         "i_corticosteroid_injection",
                         "i_acelacic_acid", "i_clindamycin",
                         "i_resorcinol", "pre_hiscr_boils",
                         "pre_hiscr_inflamed", "pre_hiscr_draining",
                         "p_age_specialist", "p_age_doctor", "p_time_specialist")]
  RegData$IHS4SCORE <- RegData$pre_hiscr_inflamed + RegData$pre_hiscr_boils*2 + RegData$pre_hiscr_draining*4
  RegData <- RegData[, -which(names(RegData) %in% c("pre_hiscr_boils", "pre_hiscr_inflamed", "pre_hiscr_draining"))]
  # Fjerner Oppfølging ikke mulig
  Followups <- Followups[Followups$c_possible != 2, ]
  # Fjerner oppfølginger uten pasientid
  Followups <- Followups[!is.na(Followups$c_mceid), ]
  # Beholder nødvendige variabler
  Followups <- Followups[, c("c_mceid", "c_do_month",
                             "c_infection", "c_delayed_wound_healing",
                             "c_stricturer", "c_nervedamage",
                             "c_bloodpoisoning", "c_bleeding",
                             "c_other_complications", "c_dlqisum",
                             "c_vasscore", "c_date",
                             "c_hurley_score", "c_hiscr_boils", "c_hiscr_inflamed", "c_hiscr_draining",
                             "c_medical_treatment",
                             "c_systemic_antibiotic_therapy_stop", "c_antiinflammatory_treatment_stop", "c_analgesics_stop",
                             "c_corticosteroid_injection_stop","c_acelacic_acid_stop", "c_clindamycin_stop",
                             "c_resorcinol_stop", "c_other_medication_stop")]
  Followups$IHS4SCORE <- Followups$c_hiscr_inflamed + Followups$c_hiscr_boils*2 + Followups$c_hiscr_draining*4
  Followups$alvorlig_med_bivirkning <- NA
  Followups$alvorlig_med_bivirkning[Followups$c_medical_treatment %in% 1:2 | Followups$c_systemic_antibiotic_therapy_stop %in% 1:2 |
                                      Followups$c_antiinflammatory_treatment_stop %in% 1:2 | Followups$c_acelacic_acid_stop %in% 1:2 |
                                      Followups$c_clindamycin_stop %in% 1:2 | Followups$c_resorcinol_stop %in% 1:2 |
                                      Followups$c_other_medication_stop %in% 1:2] <- 0
  Followups$alvorlig_med_bivirkning[Followups$c_medical_treatment==1 | Followups$c_systemic_antibiotic_therapy_stop==1 |
                                      Followups$c_antiinflammatory_treatment_stop==1 | Followups$c_analgesics_stop==1 |
                                      Followups$c_corticosteroid_injection_stop==1 | Followups$c_acelacic_acid_stop==1 |
                                      Followups$c_clindamycin_stop==1 | Followups$c_resorcinol_stop==1 |
                                      Followups$c_other_medication_stop==1] <- 1
  Followups <- Followups[, -which(names(Followups) %in% c("c_hiscr_boils", "c_hiscr_inflamed", "c_hiscr_draining", "c_medical_treatment",
                                                          "c_systemic_antibiotic_therapy_stop", "c_antiinflammatory_treatment_stop", "c_analgesics_stop",
                                                          "c_corticosteroid_injection_stop","c_acelacic_acid_stop", "c_clindamycin_stop",
                                                          "c_resorcinol_stop", "c_other_medication_stop"))]

  # Beholder kun 3- og 6-mnd oppfølging
  Followups_tilpasset <- Followups[Followups$c_do_month <9, ]
  Followups_tilpasset <- merge(Followups_tilpasset[Followups_tilpasset$c_do_month == 3, ],
                               Followups_tilpasset[Followups_tilpasset$c_do_month == 6, ],
                               by = "c_mceid", suffixes = c("_med", "_kir"), all = T)
  # Beholder nødvendige variabler
  ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "KryptertFnr",
                                 "BasisRegStatus", "HovedDato",
                                 "PasientAlder", "OppflgRegStatus",
                                 "ForlopsType1", "ForlopsType1Num",
                                 "ForlopsType2", "ForlopsType2Num",
                                 "ErMann", "SykehusNavn", "PasientID")]
  # Kobler og flater ut
  RegData <- merge(RegData,
                   ForlopsData,
                   by.x = "m_mceid",
                   by.y = "ForlopsID")
  RegData <- merge(RegData, Followups_tilpasset,
                   by.x = "m_mceid",
                   by.y = "c_mceid",
                   all.x = T)

  # Lager booske variabler basert på enkeltvalg
  RegData$SURGERY_TYPE_EXCISION_SUTUR <- 0
  RegData$SURGERY_TYPE_EXCISION_SUTUR[which(RegData$i_surgery_type == 1)] <- 1
  RegData$SURGERY_TYPE_EXCISION_GRANUL <- 0
  RegData$SURGERY_TYPE_EXCISION_GRANUL[which(RegData$i_surgery_type == 2)] <- 1
  RegData$SURGERY_TYPE_EXCISION_TRANSPLANT <- 0
  RegData$SURGERY_TYPE_EXCISION_TRANSPLANT[which(RegData$i_surgery_type == 3)] <- 1
  RegData$SURGERY_TYPE_LASER <- 0
  RegData$SURGERY_TYPE_LASER[which(RegData$i_surgery_type == 4)] <- 1
  RegData$SURGERY_TYPE_DEROOFING <- 0
  RegData$SURGERY_TYPE_DEROOFING[which(RegData$i_surgery_type == 5)] <- 1
  RegData$SURGERY_TYPE_OTHER <- NA
  RegData[is.na(RegData$i_surgery_type), c("SURGERY_TYPE_EXCISION_SUTUR", "SURGERY_TYPE_EXCISION_GRANUL",
                                           "SURGERY_TYPE_EXCISION_TRANSPLANT", "SURGERY_TYPE_LASER",
                                           "SURGERY_TYPE_DEROOFING")] <- NA
  RegData <- RegData[, -which(names(RegData)=="i_surgery_type")]
  RegData$ADALIMUMAB_BIOSIMILAR <- 0
  RegData$ADALIMUMAB_BIOSIMILAR[which(RegData$i_biological_treatment == 5)] <- 1
  RegData$INFLIXIMAB_BIOSIMILAR <- 0
  RegData$INFLIXIMAB_BIOSIMILAR[which(RegData$i_biological_treatment %in% c(4,6))] <- 1
  RegData$BIOLOGICAL_TREATMENT_OTHER <- 0
  RegData$BIOLOGICAL_TREATMENT_OTHER[which(RegData$i_biological_treatment == 3)] <- 1

  map_utdanning <- data.frame(gml=c(1:5,9), ny = c("01 Grunnskolenivå", "02a Videregående skolenivå", "02a Videregående skolenivå",
                                                   "03a Universitets- og høgskolenivå kort", "04a Universitets- og høgskolenivå lang",
                                                   "09a Uoppgitt eller ingen fullført utdanning"))
  map_nei_ukjent <- data.frame(gml=1:3, gml2=c(1,2,9), ny=c(1,0,9))
  transformer_til_ja_nei <- function(x){
    y<-x
    y[x==1] <- 0
    y[x!=1] <- 1
    y
  }

  RegData$UtdanningSSB <- map_utdanning$ny[match(RegData$p_education, map_utdanning$gml)]
  RegData$MEDICATION_HISTORY_HS <- map_nei_ukjent$ny[match(RegData$p_antibiotics, map_nei_ukjent$gml)]
  RegData$SURGERY <- map_nei_ukjent$ny[match(RegData$p_surgery, map_nei_ukjent$gml)]
  RegData$BIOLOGICAL_TREATMENT <- transformer_til_ja_nei(RegData$i_biological_treatment)

  RegData$SYSTEMIC_ANTIBIOTIC_THERAPY <- map_nei_ukjent$ny[match(RegData$i_antibiotic_therapy , map_nei_ukjent$gml2)]
  RegData$ANTIINFLAMMATORY_TREATMENT <- transformer_til_ja_nei(RegData$i_antiinflammatory_treatment)
  RegData$ACITRETIN <- NA; RegData$ACITRETIN[which(RegData$i_antiinflammatory_treatment == 8)] <- 1
  RegData$DAPSON <- NA; RegData$DAPSON[which(RegData$i_antiinflammatory_treatment == 4)] <- 1
  RegData$CICLOSPORIN <- NA; RegData$CICLOSPORIN[which(RegData$i_antiinflammatory_treatment == 6)] <- 1
  RegData$PREDNISOLON <- NA; RegData$PREDNISOLON[which(RegData$i_antiinflammatory_treatment == 5)] <- 1
  RegData$ISOTRETINOIN <- NA; #RegData$ISOTRETINOIN[which(RegData$i_antiinflammatory_treatment == ?)] <- 1 Egen variabel i gml løsn, men fra pasientskjema
  RegData$METFORMIN <- NA; # Finnes ikke i gml
  RegData$ANTIINFLAMMATORY_TREATMENT_OTHER <- NA; RegData$ANTIINFLAMMATORY_TREATMENT_OTHER[which(RegData$i_antiinflammatory_treatment == 3)] <- 1
  RegData$ANALGESICS <- transformer_til_ja_nei(RegData$i_analgesics)
  RegData$BASED_MEDICAL_TREATMENT <- map_nei_ukjent$ny[match(RegData$i_localized_med_treatment, map_nei_ukjent$gml)]
  RegData$AKSILLE <- map_nei_ukjent$ny[match(RegData$i_aksille, map_nei_ukjent$gml)]
  RegData$LYSKE <- map_nei_ukjent$ny[match(RegData$i_lyske, map_nei_ukjent$gml)]
  RegData$PUBIS <- map_nei_ukjent$ny[match(RegData$i_pubis, map_nei_ukjent$gml)]
  RegData$GENITALT <- map_nei_ukjent$ny[match(RegData$i_genitalt, map_nei_ukjent$gml)]
  RegData$PERIALT <- map_nei_ukjent$ny[match(RegData$i_peritalt, map_nei_ukjent$gml)]
  RegData$GLUTEALT <- map_nei_ukjent$ny[match(RegData$i_glutealt, map_nei_ukjent$gml)]
  RegData$MAMMAE <- map_nei_ukjent$ny[match(RegData$i_mammae, map_nei_ukjent$gml)]
  RegData$OTHER_SURGERY <- map_nei_ukjent$ny[match(RegData$i_other_location, map_nei_ukjent$gml)]
  RegData <- RegData[, -which(names(RegData) %in% c("p_education", "p_antibiotics", "p_surgery", "i_biological_treatment", "i_antibiotic_therapy",
                                                    "i_antiinflammatory_treatment", "i_analgesics", "i_localized_med_treatment",
                                                    "i_aksille", "i_lyske", "i_pubis", "i_genitalt", "i_peritalt", "i_glutealt",
                                                    "i_mammae", "i_other_location", "c_do_month_med", "c_do_month_kir"))]

  names(RegData)[match(c("m_mceid", "p_age_abscess", "pre_smoking", "pre_work", "pre_bmi", "pre_dlqisum",
                         "pre_vasscore", "pre_hurley_score", "i_type", "i_rifampicin_clindamycin",
                         "i_tetracyclin_lymecyclin", "i_amoxicilin", "i_sys_antibiotic_other", "i_corticosteroid_injection",
                         "i_acelacic_acid", "i_clindamycin", "i_resorcinol", "p_age_specialist", "p_age_doctor", "p_time_specialist",
                         "c_date_med", "c_date_kir"), names(RegData))] <-
    c("MCEID", "AGE_ABSCESS", "SMOKING", "WORK", "BMI", "DLQISUM", "VASSCORE", "HURLEY_SCORE", "TYPE_INTERVENTION",
      "RIFAMPICIN_CLINDAMYCIN", "TETRACYCLIN_LYMECYCLIN", "AMOXICILIN", "SYSTEMIC_ANTIBIOTIC_THERAPY_OTHER",
      "INTRALESIONAL_CORTICOSTEROID_INJECTION", "ACELACIC_ACID", "CLINDAMYCIN", "RESORCINOL", "AGE_SPECIALIST", "AGE_DOCTOR",
      "TIME_SPECIALIST", "CONTROL_DATE_dokt_med", "CONTROL_DATE_dokt_kir")


  RegData[, c("c_infection_med", "c_delayed_wound_healing_med", "c_stricturer_med", "c_nervedamage_med",
              "c_bloodpoisoning_med", "c_bleeding_med", "c_other_complications_med",
              "c_infection_kir", "c_delayed_wound_healing_kir", "c_stricturer_kir", "c_nervedamage_kir",
              "c_bloodpoisoning_kir", "c_bleeding_kir", "c_other_complications_kir")] <-
    apply(RegData[, c("c_infection_med", "c_delayed_wound_healing_med", "c_stricturer_med", "c_nervedamage_med",
                      "c_bloodpoisoning_med", "c_bleeding_med", "c_other_complications_med",
                      "c_infection_kir", "c_delayed_wound_healing_kir", "c_stricturer_kir", "c_nervedamage_kir",
                      "c_bloodpoisoning_kir", "c_bleeding_kir", "c_other_complications_kir")], 2, function(x) {plyr::mapvalues(x, 1:3, c(1,0,9))})

  map_gml_ny_navn <- data.frame(gml = c("c_infection_med", "c_delayed_wound_healing_med", "c_stricturer_med", "c_nervedamage_med",
                                        "c_bloodpoisoning_med", "c_bleeding_med", "c_other_complications_med", "c_dlqisum_med",
                                        "c_vasscore_med", "c_hurley_score_med",
                                        "c_infection_kir", "c_delayed_wound_healing_kir", "c_stricturer_kir", "c_nervedamage_kir",
                                        "c_bloodpoisoning_kir", "c_bleeding_kir", "c_other_complications_kir", "c_dlqisum_kir",
                                        "c_vasscore_kir", "c_hurley_score_kir"),
                                ny = c("INFECTION_med", "DELAYED_WOUND_HEALING_med", "STRIKTURER_med", "NERVEDAMAGE_med",
                                       "BLOODPOISEN_med", "BLEEDING_med", "OTHER_COMPLICATIONS_ID_med", "DLQISUM_med",
                                       "VASSCORE_med", "HURLEY_SCORE_med",
                                       "INFECTION_kir", "DELAYED_WOUND_HEALING_kir", "STRIKTURER_kir", "NERVEDAMAGE_kir",
                                       "BLOODPOISEN_kir", "BLEEDING_kir", "OTHER_COMPLICATIONS_ID_kir", "DLQISUM_kir",
                                       "VASSCORE_kir", "HURLEY_SCORE_kir"))
  names(RegData)[match(map_gml_ny_navn$gml, names(RegData))] <- map_gml_ny_navn$ny
  RegData
}
