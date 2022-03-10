
hisregdata <- hisreg::lastShinyHisreg()

#------------------Variabel valg-------------------------------------

avdValg <- unique(hisregdata$RegData$AvdRESH)
names(avdValg) <- hisregdata$RegData$SykehusNavn[match(unique(hisregdata$RegData$AvdRESH), hisregdata$RegData$AvdRESH)]


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
