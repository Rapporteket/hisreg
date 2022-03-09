rm(list = ls())
library(shiny)
library(raplog)
library(hisreg)
library(tidyverse)
library(shinyalert)
library(shinyjs)
library(kableExtra)
library(DT)
library(htmltools)
library(rapbase)
library(lubridate)

### Data i gammel form #########################################
ForlopsData <- read.table("I:/hisreg/ForlopsOversikt_v12021-04-08 16-25-45.txt",
                          header = TRUE, sep = ";", fileEncoding = "UTF-8-BOM")
RegData <- read.table("I:/hisreg/AlleVarNum2021-04-08 16-25-45.txt",
                      header = TRUE, sep = ";", fileEncoding = "UTF-8-BOM")
SkjemaOversikt <- read.table("I:/hisreg/SkjemaOversikt_v12021-04-08 16-25-45.txt",
                             header = TRUE, sep = ";", fileEncoding = "UTF-8-BOM")
RegDataLabel <- read.table("I:/hisreg/AlleVar2021-04-08 16-25-45.txt",
                           header = TRUE, sep = ";", fileEncoding = "UTF-8-BOM")
Followups <- read.table("I:/hisreg/FollowupsNum2021-04-08 16-25-45.txt",
                        header = TRUE, sep = ";", fileEncoding = "UTF-8-BOM")

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
                       "i_resorcinol")]
# Fjerner Oppfølging ikke mulig
Followups <- Followups[Followups$c_possible != 2, ]
# Fjerner oppfølginger uten pasientid
Followups <- Followups[!is.na(Followups$c_mceid), ]
Followups <- Followups[, c("c_mceid", "c_do_month",
                           "c_infection", "c_delayed_wound_healing",
                           "c_stricturer", "c_nervedamage",
                           "c_bloodpoisoning", "c_bleeding",
                           "c_other_complications", "c_dlqisum",
                           "c_vasscore",
                           "c_hurley_score")]

Followups_tilpasset <- Followups[Followups$c_do_month <9, ]
Followups_tilpasset <- merge(Followups_tilpasset[Followups_tilpasset$c_do_month == 3, ],
                             Followups_tilpasset[Followups_tilpasset$c_do_month == 6, ],
                             by = "c_mceid", suffixes = c("_med", "_kir"), all = T)


Followups <- Followups[order(Followups$c_do_month,
                             decreasing = FALSE), ]
# Velg første oppfølging
Followups <- Followups[match(unique(Followups$c_mceid),
                             Followups$c_mceid), ]

ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "KryptertFnr",
                               "BasisRegStatus", "HovedDato",
                               "PasientAlder", "OppflgRegStatus",
                               "ForlopsType1", "ForlopsType1Num",
                               "ForlopsType2", "ForlopsType2Num",
                               "ErMann", "SykehusNavn", "PasientID")]
RegData <- merge(RegData,
                 ForlopsData,
                 by.x = "m_mceid",
                 by.y = "ForlopsID")
RegData <- merge(RegData, Followups_tilpasset,
                 by.x = "m_mceid",
                 by.y = "c_mceid",
                 all.x = T)



preinterventiondoctor <- read.table('I:/hisreg/preinterventiondoctor2021-04-08 16-25-45.txt', header=TRUE, sep=";", fileEncoding = 'UTF-8')
registration <- read.table('I:/hisreg/registration2021-04-08 16-25-45.txt', header=TRUE, sep=";", fileEncoding = 'UTF-8')
intervention <- read.table('I:/hisreg/intervention2021-04-08 16-25-45.txt', header=TRUE, sep=";", fileEncoding = 'UTF-8')
mce <- read.table('I:/hisreg/mce2021-04-08 16-25-45.txt', header=TRUE, sep=";", fileEncoding = 'UTF-8')
mcelist <- read.table('I:/hisreg/mcelist2021-04-08 16-25-45.txt', header=TRUE, sep=";", fileEncoding = 'UTF-8')
patient <- read.table('I:/hisreg/patient2021-04-08 16-25-45.txt', header=TRUE, sep=";", fileEncoding = 'UTF-8')
patientcontrol <- read.table('I:/hisreg/patientcontrol2021-04-08 16-25-45.txt', header=TRUE, sep=";", fileEncoding = 'UTF-8')
preintervention <- read.table('I:/hisreg/preintervention2021-04-08 16-25-45.txt', header=TRUE, sep=";", fileEncoding = 'UTF-8')
centre <- read.table('I:/hisreg/centre2021-04-08 16-25-45.txt', header=TRUE, sep=";", fileEncoding = 'UTF-8')
doctorcontrol <- read.table('I:/hisreg/doctorcontrol2021-04-08 16-25-45.txt', header=TRUE, sep=";", fileEncoding = 'UTF-8')
ForlopsOversikt_ny <- read.table('I:/hisreg/ForlopsOversikt2021-04-08 16-25-45.txt', header=TRUE, sep=";", fileEncoding = 'UTF-8')
SkjemaOversikt_ny <- read.table('I:/hisreg/SkjemaOversikt2021-04-08 16-25-45.txt', header=TRUE, sep=";", fileEncoding = 'UTF-8')

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
oppfolging_kir <- merge(patientcontrol[patientcontrol$CONTROLTYPE == 1, ], doctorcontrol[doctorcontrol$CONTROLTYPE == 1, ],
                        by = "MCEID",  suffixes = c("_pas", "_dokt"), all = TRUE)
oppfolging_med <- merge(patientcontrol[patientcontrol$CONTROLTYPE == 2, ], doctorcontrol[doctorcontrol$CONTROLTYPE == 2, ],
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
allevar$PasientID <- allevar$PasientID + 10000
allevar$PasientID[!is.na(RegData$PasientID[match(allevar$KryptertFnr, RegData$KryptertFnr)])] <-
  RegData$PasientID[match(allevar$KryptertFnr, RegData$KryptertFnr)][!is.na(RegData$PasientID[match(allevar$KryptertFnr, RegData$KryptertFnr)])]

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
                          "VASSCORE_kir", "VASSCORE_med", "HURLEY_SCORE_kir", "HURLEY_SCORE_med")]

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
# RegData <- RegData[, -which(names(RegData)=="p_education")]
RegData$MEDICATION_HISTORY_HS <- map_nei_ukjent$ny[match(RegData$p_antibiotics, map_nei_ukjent$gml)]
RegData$SURGERY <- map_nei_ukjent$ny[match(RegData$p_surgery, map_nei_ukjent$gml)]
RegData$BIOLOGICAL_TREATMENT <- transformer_til_ja_nei(RegData$i_biological_treatment)
RegData$SYSTEMIC_ANTIBIOTIC_THERAPY <- map_nei_ukjent$ny[match(RegData$i_antibiotic_therapy , map_nei_ukjent$gml2)]
RegData$ANTIINFLAMMATORY_TREATMENT <- transformer_til_ja_nei(RegData$i_antiinflammatory_treatment)
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
        "i_acelacic_acid", "i_clindamycin", "i_resorcinol"), names(RegData))] <-
  c("MCEID", "AGE_ABSCESS", "SMOKING", "WORK", "BMI", "DLQISUM", "VASSCORE", "HURLEY_SCORE", "TYPE_INTERVENTION",
    "RIFAMPICIN_CLINDAMYCIN", "TETRACYCLIN_LYMECYCLIN", "AMOXICILIN", "SYSTEMIC_ANTIBIOTIC_THERAPY_OTHER",
    "INTRALESIONAL_CORTICOSTEROID_INJECTION", "ACELACIC_ACID", "CLINDAMYCIN", "RESORCINOL")


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


samlet <- bind_rows(RegData, RegData_ny)


#
#
# p<-11
# map_gml_ny_navn[p,]
# table(RegData[, map_gml_ny_navn$gml[p]], useNA = 'ifany')
# table(RegData_ny[, map_gml_ny_navn$ny[p]], useNA = 'ifany')
#
# mangler <- map_varnavn[match(setdiff(names(RegData), c("MCEID", "AGE_ABSCESS", "SMOKING", "WORK", "BMI", "DLQISUM", "VASSCORE", "HURLEY_SCORE", "TYPE_INTERVENTION",
#                           "SURGERY_TYPE_EXCISION_SUTUR", "SURGERY_TYPE_EXCISION_GRANUL", "SURGERY_TYPE_EXCISION_TRANSPLANT",
#                           "SURGERY_TYPE_LASER", "SURGERY_TYPE_DEROOFING", "SURGERY_TYPE_OTHER", "BIOLOGICAL_TREATMENT",
#                           "SYSTEMIC_ANTIBIOTIC_THERAPY", "ANTIINFLAMMATORY_TREATMENT", "ANALGESICS", "BASED_MEDICAL_TREATMENT",
#                           "AKSILLE", "LYSKE", "PUBIS", "GENITALT", "PERIALT", "GLUTEALT", "MAMMAE", "OTHER_SURGERY",
#                           "RIFAMPICIN_CLINDAMYCIN", "TETRACYCLIN_LYMECYCLIN", "AMOXICILIN", "SYSTEMIC_ANTIBIOTIC_THERAPY_OTHER",
#                           "INTRALESIONAL_CORTICOSTEROID_INJECTION", "ACELACIC_ACID", "CLINDAMYCIN", "RESORCINOL",
#                           "AvdRESH", "KryptertFnr", "BasisRegStatus", "HovedDato",
#                           "PasientAlder", "OppflgRegStatus", "ForlopsType1", "ForlopsType1Num",
#                           "ForlopsType2", "ForlopsType2Num", "ErMann", "SykehusNavn", "PasientID")), map_varnavn$gml), ]
#
# c("m_mceid", "p_age_abscess", "pre_smoking", "pre_work", "pre_bmi", "pre_dlqisum",
#   "pre_vasscore", "pre_hurley_score", "i_type", "i_rifampicin_clindamycin",
# "i_tetracyclin_lymecyclin", "i_amoxicilin", "i_sys_antibiotic_other", "i_corticosteroid_injection",
# "i_acelacic_acid", "i_clindamycin", "i_resorcinol")
#
# c("MCEID", "AGE_ABSCESS", "SMOKING", "WORK", "BMI", "DLQISUM", "VASSCORE", "HURLEY_SCORE", "TYPE_INTERVENTION",
#   "RIFAMPICIN_CLINDAMYCIN", "TETRACYCLIN_LYMECYCLIN", "AMOXICILIN", "SYSTEMIC_ANTIBIOTIC_THERAPY_OTHER",
#   "INTRALESIONAL_CORTICOSTEROID_INJECTION", "ACELACIC_ACID", "CLINDAMYCIN", "RESORCINOL")
#
#
# map_varnavn <- data.frame(gml=c("m_mceid", "p_age_abscess",
#                                 "p_education", "p_surgery",
#                                 "pre_smoking",
#                                 "pre_work", "pre_bmi", "pre_dlqisum",
#                                 "pre_vasscore",
#                                 "pre_hurley_score", "i_type",
#                                 "SURGERY_TYPE_EXCISION_SUTUR", "SURGERY_TYPE_EXCISION_GRANUL", "SURGERY_TYPE_EXCISION_TRANSPLANT",
#                                 "SURGERY_TYPE_LASER", "SURGERY_TYPE_DEROOFING", "SURGERY_TYPE_OTHER",
#                                 "i_biological_treatment",
#                                 "i_antibiotic_therapy",
#                                 "i_antiinflammatory_treatment",
#                                 "i_analgesics",
#                                 "i_localized_med_treatment",
#                                 "i_aksille", "i_lyske", "i_pubis",
#                                 "i_genitalt", "i_peritalt", "i_glutealt",
#                                 "i_mammae", "i_other_location",
#                                 "i_rifampicin_clindamycin",
#                                 "i_tetracyclin_lymecyclin",
#                                 "i_amoxicilin", "i_sys_antibiotic_other",
#                                 "i_corticosteroid_injection",
#                                 "i_acelacic_acid", "i_clindamycin",
#                                 "i_resorcinol", "AvdRESH", "KryptertFnr",
#                                 "BasisRegStatus", "HovedDato",
#                                 "PasientAlder", "OppflgRegStatus",
#                                 "ForlopsType1", "ForlopsType1Num",
#                                 "ForlopsType2", "ForlopsType2Num",
#                                 "ErMann", "SykehusNavn", "PasientID"),
#                           ny=c("MCEID", "AGE_ABSCESS", "UtdanningSSB", "SURGERY",
#                                "SMOKING", "WORK", "BMI", "DLQISUM", "VASSCORE", "HURLEY_SCORE", "TYPE_INTERVENTION",
#                                "SURGERY_TYPE_EXCISION_SUTUR", "SURGERY_TYPE_EXCISION_GRANUL", "SURGERY_TYPE_EXCISION_TRANSPLANT",
#                                "SURGERY_TYPE_LASER", "SURGERY_TYPE_DEROOFING", "SURGERY_TYPE_OTHER", "BIOLOGICAL_TREATMENT",
#                                "SYSTEMIC_ANTIBIOTIC_THERAPY", "ANTIINFLAMMATORY_TREATMENT", "ANALGESICS", "BASED_MEDICAL_TREATMENT",
#                                "AKSILLE", "LYSKE", "PUBIS", "GENITALT", "PERIALT", "GLUTEALT", "MAMMAE", "OTHER_SURGERY",
#                                "RIFAMPICIN_CLINDAMYCIN", "TETRACYCLIN_LYMECYCLIN", "AMOXICILIN", "SYSTEMIC_ANTIBIOTIC_THERAPY_OTHER",
#                                "INTRALESIONAL_CORTICOSTEROID_INJECTION", "ACELACIC_ACID", "CLINDAMYCIN", "RESORCINOL",
#                                "AvdRESH", "KryptertFnr", "BasisRegStatus", "HovedDato",
#                                "PasientAlder", "OppflgRegStatus", "ForlopsType1", "ForlopsType1Num",
#                                "ForlopsType2", "ForlopsType2Num", "ErMann", "SykehusNavn", "PasientID"))
#
# p<-52
# map_varnavn[p,]
# table(RegData[map_varnavn$gml[p]], useNA = 'ifany')
# table(RegData_ny[map_varnavn$ny[p]], useNA = 'ifany')
#
# c("m_mceid", "p_age_abscess",
#   "p_education", "p_surgery",
#   "p_antibiotics", "pre_smoking",
#   "pre_work", "pre_bmi", "pre_dlqisum",
#   "pre_hsscoresum", "pre_vasscore",
#   "pre_hurley_score", "i_type",
#   "i_surgery_type",
#   "i_biological_treatment",
#   "i_antibiotic_therapy",
#   "i_antiinflammatory_treatment",
#   "i_analgesics",
#   "i_localized_med_treatment",
#   "i_aksille", "i_lyske", "i_pubis",
#   "i_genitalt", "i_peritalt", "i_glutealt",
#   "i_mammae", "i_other_location",
#   "i_rifampicin_clindamycin",
#   "i_tetracyclin_lymecyclin",
#   "i_amoxicilin", "i_sys_antibiotic_other",
#   "i_corticosteroid_injection",
#   "i_acelacic_acid", "i_clindamycin",
#   "i_resorcinol", "AvdRESH", "KryptertFnr",
#   "BasisRegStatus", "HovedDato",
#   "PasientAlder", "OppflgRegStatus",
#   "ForlopsType1", "ForlopsType1Num",
#   "ForlopsType2", "ForlopsType2Num",
#   "ErMann", "SykehusNavn", "PasientID",
#   "c_infection", "c_delayed_wound_healing",
#   "c_stricturer", "c_nervedamage",
#   "c_bloodpoisoning", "c_bleeding",
#   "c_other_complications", "c_dlqisum",
#   "c_hsscoresum", "c_vasscore",
#   "c_hurley_score")
# c("MCEID", "AGE_ABSCESS", "UtdanningSSB", "SURGERY", "MEDICATION_HISTORY_HS",
#   "SMOKING", "WORK", "BMI", "DLQISUM", "VASSCORE", "HURLEY_SCORE", "TYPE_INTERVENTION",
#   "SURGERY_TYPE_EXCISION_SUTUR", "SURGERY_TYPE_EXCISION_GRANUL", "SURGERY_TYPE_EXCISION_TRANSPLANT",
#   "SURGERY_TYPE_LASER", "SURGERY_TYPE_DEROOFING", "SURGERY_TYPE_OTHER", "BIOLOGICAL_TREATMENT",
#   "SYSTEMIC_ANTIBIOTIC_THERAPY", "ANTIINFLAMMATORY_TREATMENT", "ANALGESICS", "BASED_MEDICAL_TREATMENT",
#   "AKSILLE", "LYSKE", "PUBIS", "GENITALT", "PERIALT", "GLUTEALT", "MAMMAE", "OTHER_LOCATION",
#   "RIFAMPICIN_CLINDAMYCIN", "TETRACYCLIN_LYMECYCLIN", "AMOXICILIN", "SYSTEMIC_ANTIBIOTIC_THERAPY_OTHER",
#   "INTRALESIONAL_CORTICOSTEROID_INJECTION", "ACELACIC_ACID", "CLINDAMYCIN", "RESORCINOL",
#   "AvdRESH", "KryptertFnr", "BasisRegStatus", "HovedDato",
#   "PasientAlder", "OppflgRegStatus", "ForlopsType1", "ForlopsType1Num",
#   "ForlopsType2", "ForlopsType2Num", "ErMann", "SykehusNavn", "PasientID",
#   "INFECTION_kir", "INFECTION_med", "DELAYED_WOUND_HEALING_kir", "DELAYED_WOUND_HEALING_med",
#   "STRIKTURER_kir", "STRIKTURER_med", "NERVEDAMAGE_kir", "NERVEDAMAGE_med",
#   "BLOODPOISEN_kir", "BLOODPOISEN_med", "BLEEDING_kir", "BLEEDING_med",
#   "OTHER_COMPLICATIONS_ID_kir", "OTHER_COMPLICATIONS_ID_med", "DLQISUM_kir", "DLQISUM_med",
#   "VASSCORE_kir", "VASSCORE_med", "HURLEY_SCORE_kir", "HURLEY_SCORE_med")
