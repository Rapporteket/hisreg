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

context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
onServer <- context == "TEST" | context == "QA" | context == "PRODUCTION" | context == "DEV"
if (onServer) {
  RegData <- hisreg::hisregHentRegData()
  SkjemaOversikt <- rapbase::LoadRegData(
    registryName = "hisreg",
    dbType = "mysql",
    query = "SELECT *
             FROM SkjemaOversikt_v1"
  )
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

} else {
  ############## GAMMEL DATA ####################
ForlopsData <- read.table("I:/hisreg/ForlopsOversikt_v12021-04-07 14-52-56.txt",
                          header = TRUE, sep = ";", encoding = "UTF-8-BOM")

RegData <- read.table("I:/hisreg/AlleVarNum2021-04-07 14-52-56.txt",
                      header = TRUE, sep = ";", encoding = "UTF-8-BOM")

SkjemaOversikt <- read.table("I:/hisreg/SkjemaOversikt_v12021-04-07 14-52-56.txt",
                             header = TRUE, sep = ";", encoding = "UTF-8-BOM")

RegDataLabel <- read.table("I:/hisreg/AlleVar2021-04-07 14-52-56.txt",
                           header = TRUE, sep = ";", encoding = "UTF-8-BOM")

Followups <- read.table("I:/hisreg/FollowupsNum2021-04-07 14-52-56.txt",
                           header = TRUE, sep = ";", encoding = "UTF-8-BOM")


RegData <- RegData[, c("m_mceid", "p_age_abscess",
                       "p_education", "p_surgery",
                       "p_antibiotics", "pre_smoking",
                       "pre_work", "pre_bmi", "pre_dlqisum",
                       "pre_hsscoresum", "pre_vasscore",
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
                       "i_resorcinol",
                       "i_medical_other")]

# Fjerner Oppfølging ikke mulig
Followups <- Followups[Followups$c_possible != 2, ]
# Fjerner oppfølginger uten pasientid
Followups <- Followups[!is.na(Followups$c_mceid), ]
Followups <- Followups[order(Followups$c_do_month,
                             decreasing = FALSE), ]
# Velg første oppfølging
Followups <- Followups[match(unique(Followups$c_mceid),
                             Followups$c_mceid), ]
Followups <- Followups[, c("c_mceid", "c_do_month",
                           "c_infection", "c_delayed_wound_healing",
                           "c_stricturer", "c_nervedamage",
                           "c_bloodpoisoning", "c_bleeding",
                           "c_other_complications", "c_dlqisum",
                           "c_hsscoresum", "c_vasscore",
                           "c_hurley_score")]


ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH",
                               "BasisRegStatus", "HovedDato",
                               "PasientAlder", "OppflgRegStatus",
                               "ForlopsType1", "ForlopsType1Num",
                               "ForlopsType2", "ForlopsType2Num",
                               "ErMann", "SykehusNavn", "PasientID")]



RegData <- merge(RegData,
                 ForlopsData,
                 by.x = "m_mceid",
                 by.y = "ForlopsID")
RegData <- merge(RegData, Followups,
                 by.x = "m_mceid",
                 by.y = "c_mceid",
                 all.x = T)
############## NY DATA ####################
# Les data:
preinterventiondoctor <- read.table('I:/hisreg/preinterventiondoctor2021-02-02 13-24-06.txt', header=TRUE, sep=";", encoding = 'UTF-8')
registration <- read.table('I:/hisreg/registration2021-02-02 13-24-06.txt', header=TRUE, sep=";", encoding = 'UTF-8')
intervention <- read.table('I:/hisreg/intervention2021-02-02 13-24-06.txt', header=TRUE, sep=";", encoding = 'UTF-8')
mce <- read.table('I:/hisreg/mce2021-02-02 13-24-06.txt', header=TRUE, sep=";", encoding = 'UTF-8')
mcelist <- read.table('I:/hisreg/mcelist2021-02-02 13-24-06.txt', header=TRUE, sep=";", encoding = 'UTF-8')
patient <- read.table('I:/hisreg/patient2021-02-02 13-24-06.txt', header=TRUE, sep=";", encoding = 'UTF-8')
patientcontrol <- read.table('I:/hisreg/patientcontrol2021-02-02 13-24-06.txt', header=TRUE, sep=";", encoding = 'UTF-8')
preintervention <- read.table('I:/hisreg/preintervention2021-02-02 13-24-06.txt', header=TRUE, sep=";", encoding = 'UTF-8')
centre <- read.table('I:/hisreg/centre2021-02-02 13-24-06.txt', header=TRUE, sep=";", encoding = 'UTF-8')
doctorcontrol <- read.table('I:/hisreg/doctorcontrol2021-02-02 13-24-06.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopsOversikt_ny <- read.table('I:/hisreg/ForlopsOversikt2021-02-02 13-24-06.txt', header=TRUE, sep=";", encoding = 'UTF-8')
SkjemaOversikt_ny <- read.table('I:/hisreg/SkjemaOversikt2021-02-02 13-24-06.txt', header=TRUE, sep=";", encoding = 'UTF-8')
}

RegData <- hisreg::hisregPreprosess(RegData)
RegDataAll <- RegData[which(RegData$AvdRESH == 999002), ]
# Roskilde skal ikke inngå blant de nasjonale tallene.
RegData <- RegData[RegData$AvdRESH != 999002, ]
RegData$SykehusNavn <- as.factor(as.character(RegData$SykehusNavn))
RegDataAll$SykehusNavn <- as.factor(as.character(RegDataAll$SykehusNavn))
# RegData$HovedDato <- as.POSIXct.POSIXlt(RegData$HovedDato)

#### NY ######
# Bearbeid data:
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

allevar <- merge(preintervention, preinterventiondoctor[!(names(preinterventiondoctor) %in% intersect(names(preintervention), names(preinterventiondoctor)[-1]))], by = "MCEID")
allevar <- merge(allevar, intervention[!(names(intervention) %in% intersect(names(allevar), names(intervention)[-1]))], by = "MCEID")
allevar <- merge(allevar, registration[!(names(registration) %in% intersect(names(allevar), names(registration)[-1]))], by = "MCEID")
allevar <- merge(allevar, oppfolging, by = "MCEID", suffixes = c("", "_oppf"), all.x = T)
allevar$Sykehusnavn <- centre$CENTRESHORTNAME[match(allevar$CENTREID, centre$ID)]

###################

#------------------Variabel valg-------------------------------------

avdValg <- unique(RegData$AvdRESH)
names(avdValg) <- unique(RegData$SykehusNavn)

varValgFordeling <- c("Alder" = "PasientAlder",
                      "Alder ved første byll" = "p_age_abscess",
                      "Kjønn" = "ErMann",
                      "Utdanning" = "p_education",
                      "Røykevaner" = "pre_smoking",
                      "Arbeidsstatus preintervensjon" = "pre_work",
                      "Type intervensjon" = "i_type",
                      "Type Kirurgi" = "i_surgery_type",
                      "Biologiske legemidler" = "i_biological_treatment",
                      "Type medisinsk behandling" = "MedisinskBeh",
                      "Lokalisasjon av kirurgisk inngrep" =
                      "KirurgiLokalisering",
                      "Komplikasjoner kirurgi" = "KomplKir",
                      "Tidligere behandling" = "TidlBeh",
                      "BMI" = "pre_bmi",
                      "Hurley-score ved inklusjon" = "pre_hurley_score")

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

varValgGjenPer <- c("BMI, preintervensjon" = "pre_bmi",
                    "DLQI-sum, preintervensjon" = "pre_dlqisum",
                    "VAS-score, preintervensjon" = "pre_vasscore",
                    "HS-score, preintervensjon" = "pre_hsscoresum")

varValgGjenFE <- c("DLQI" = "DLQI_PrePost",
                    "HS-score" = "HS_PrePost",
                    "VAS" = "Vas_PrePost")
