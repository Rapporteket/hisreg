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
  RegData <- hisregHentRegData()
  SkjemaOversikt <- rapbase::LoadRegData(
    registryName = "hisreg",
    dbType = "mysql",
    query = "SELECT *
             FROM SkjemaOversikt"
  )
} else {

ForlopsData <- read.table("I:/hisreg/ForlopsOversikt2019-10-10 10-15-43.txt",
                          header = TRUE, sep = ";", encoding = "UTF-8-BOM")

RegData <- read.table("I:/hisreg/AlleVarNum2019-10-10 10-15-40.txt",
                      header = TRUE, sep = ";", encoding = "UTF-8-BOM")

SkjemaOversikt <- read.table("I:/hisreg/SkjemaOversikt2019-10-10 10-15-44.txt",
                             header = TRUE, sep = ";", encoding = "UTF-8-BOM")

RegDataLabel <- read.table("I:/hisreg/AlleVar2019-10-10 10-15-37.txt",
                           header = TRUE, sep = ";", encoding = "UTF-8-BOM")

Followups <- read.table("I:/hisreg/FollowupsNum2019-10-10 10-15-42.txt",
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

}
RegData <- hisregPreprosess(RegData)
RegDataAll <- RegData[which(RegData$AvdRESH == 999002), ]
# Roskilde skal ikke inngå blant de nasjonale tallene.
RegData <- RegData[RegData$AvdRESH != 999002, ]
RegData$SykehusNavn <- as.factor(as.character(RegData$SykehusNavn))
RegDataAll$SykehusNavn <- as.factor(as.character(RegDataAll$SykehusNavn))
RegData$HovedDato <- as.POSIXct.POSIXlt(RegData$HovedDato)

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
