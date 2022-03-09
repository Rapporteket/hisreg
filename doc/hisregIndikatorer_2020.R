rm(list = ls())
library(hisreg)
library(tidyverse)
library(lubridate)

ForlopsData <- read.table("I:/hisreg/ForlopsOversikt_v12021-04-08 16-25-45.txt",
                          header = TRUE, sep = ";", fileEncoding = "UTF-8-BOM")
RegData <- read.table("I:/hisreg/AlleVarNum2021-04-08 16-25-45.txt",
                      header = TRUE, sep = ";", fileEncoding = "UTF-8-BOM")
Followups <- read.table("I:/hisreg/FollowupsNum2021-04-08 16-25-45.txt",
                        header = TRUE, sep = ";", fileEncoding = "UTF-8-BOM")
hisreg_old<-list(RegData=RegData, ForlopsData=ForlopsData, Followups=Followups)

RegData_gml <- hisregKonverterData(RegData=RegData, ForlopsData=ForlopsData, Followups=Followups)
RegData_gml <- RegData_gml[RegData_gml$AvdRESH != 999002, ] # Fjerner Roskilde

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
                          "VASSCORE_kir", "VASSCORE_med", "HURLEY_SCORE_kir", "HURLEY_SCORE_med", "ADALIMUMAB_BIOSIMILAR",
                          "INFLIXIMAB_BIOSIMILAR", "BIOLOGICAL_TREATMENT_OTHER", "ACITRETIN", "DAPSON", "CICLOSPORIN",
                          "PREDNISOLON", "ISOTRETINOIN", "METFORMIN", "ANTIINFLAMMATORY_TREATMENT_OTHER", "IHS4SCORE",
                          "IHS4SCORE_kir", "IHS4SCORE_med", "AGE_SPECIALIST", "AGE_DOCTOR", "TIME_SPECIALIST",
                          "CONTROL_DATE_dokt_kir", "CONTROL_DATE_dokt_med", "SEPONERT_kir", "SEPONERT_med",
                          "SEPONERT_BIVIRKNING_kir", "SEPONERT_BIVIRKNING_med", "TREATMENT_med", "TREATMENT_kir",
                          "SATISFACTION_kir", "SATISFACTION_med")]


shus <- data.frame(AvdRESH = unique(RegData_ny$AvdRESH),
                   SykehusNavn = RegData_ny$SykehusNavn[match(unique(RegData_ny$AvdRESH), RegData_ny$AvdRESH)])
shus$SykehusNavn <- trimws(shus$SykehusNavn)
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

rm(list = c("allevar", "centre", "doctorcontrol", "Followups", "ForlopsData", "ForlopsOversikt_ny", "intervention",
            "mce", "mcelist", "oppfolging", "oppfolging_kir", "oppfolging_med", "patient", "patientcontrol", "preintervention",
            "preinterventiondoctor", "registration", "SkjemaOversikt_ny", "RegData_gml", "RegData_ny", "shus"))

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

rapp_aar <- 2020

RegData <- RegData[which(RegData$Aar <= rapp_aar), ]

########### Indikator 1: Andel henvist til hudspesialist innen 1 år av første besøk hos allmennlege  ###########

# Alder ved besøk allmennlege (p_age_doctor) og alder ved henvisning til spesialist (p_age_specialist.) oppgis som
# heltall. Tid fra første besøk hos allmennlege til
# henvisning til spesialist er beregnet som differansen mellom variablene p_age_doctor og p_age_specialist.
# Dersom en pasient er hos fastlege dagen før bursdagen og henvises spesialist på bursdagen så vil differansen
# være 1 år, selv om det kun skiller 1 dag. Hvis vedkommende er hos fastlege på bursdagen og spesialist dagen før
# bursdagen to år senere så vil også differansen være 1 år. En differanse på 0 vil altså bety mellom 0 og 364 dager,
# mens en differanse på 1 betyr mellom 1 dag og 2 år, 2 kan være fra 1 til 3 år, osv. Det er altså ikke mulig å
# beregne indikatoren på en konsistent måte. Dette må redegjøres for i fremstillingen på resultatportalen.
# En tidlig utgave av registeret registrerte p_time_specialist direkte. Skal denne velges når den finnes?
# Her benyttes primært p_time_specialist, sekundært den beregnede differansen



# RegData$tid_almlege_spes <- RegData$p_age_specialist - RegData$p_age_doctor
RegData$tid_almlege_spes <- RegData$AGE_SPECIALIST - RegData$AGE_DOCTOR
aux <- RegData[!is.na(RegData$tid_almlege_spes) | !is.na(RegData$TIME_SPECIALIST), ]

aux$tid_almlege_spes_kombo <- aux$TIME_SPECIALIST
aux$tid_almlege_spes_kombo[is.na(aux$TIME_SPECIALIST)] <- aux$tid_almlege_spes[is.na(aux$TIME_SPECIALIST)]
aux <- aux[order(aux$HovedDato, decreasing = F), ]
aux <- aux[match(unique(aux$PasientID), aux$PasientID), ]

indikator1 <- aux[, c("Aar", "SykehusNavn", "AvdRESH", "tid_almlege_spes_kombo")]
indikator1$Variabel <- 0
indikator1$Variabel[indikator1$tid_almlege_spes_kombo == 0] <- 1
indikator1 <- indikator1[indikator1$Aar %in% 2015:rapp_aar, ]
indikator1$ind_id <- "hisreg_henvist_spesialist_1aar"

outfile <- "C:/GIT/hisreg/doc/henvist_spes.pdf"
hisregIndikator(indikatordata = indikator1, tittel=c("Andel henvist til hudspesialist innen", "1 år av første besøk hos allmennlege"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                            legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                            lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')

tmp <- indikator1 %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                               andel = sum(Variabel)/n()*100,
                                                               N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind1_tid_almlege_spes <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
# write.csv2(Ind1_tid_almlege_spes, "I:/hisreg/Ind1_tid_almlege_spes.csv", row.names = F)

########### Indikator 4: Andel pasienter (ELLER FORLØP) som oppnår reduksjon fra Hurley 3/2 til 2 eller 1 etter behandling.
# Inkluderer alle som har Hurley 2 eller 3 ved prekontroll, og som har registrert Hurley ved oppfølging

aux <- RegData[RegData$HURLEY_SCORE %in% 2:3 & !is.na(RegData$HURLEY_SCORE_POST), ]
aux$hurley_diff <- aux$HURLEY_SCORE - aux$HURLEY_SCORE_POST

indikator4 <- aux[, c("Aar", "SykehusNavn", "AvdRESH", "hurley_diff")]
indikator4$Variabel <- 0
indikator4$Variabel[indikator4$hurley_diff > 0] <- 1
indikator4$ind_id <- "hisreg_reduksjon_hurley"
Indikatorer <- dplyr::bind_rows(indikator1[, c("Aar", "AvdRESH", "Variabel", "ind_id")],
                                indikator4[, c("Aar", "AvdRESH", "Variabel", "ind_id")])

outfile <- "C:/GIT/hisreg/doc/hurley_red.pdf"
hisregIndikator(indikatordata = indikator4, tittel=c("Reduksjon i Hurley score ", "ved kontroll"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')

tmp <- indikator4 %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                               andel = sum(Variabel)/n()*100,
                                                               N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind4_hurley_redusert <-tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
# write.csv2(Ind4_hurley_redusert, "I:/hisreg/Ind4_hurley_redusert.csv", row.names = F)
########### Indikator 5: Andel pasienter som oppnår behandlingsmålet (DLQI <4) for pasientvurdert dermatologisk livskvalitet.
#
# Bruker pre_dlqisum - c_dlqisum >= 4

aux <- RegData[!is.na(RegData$DLQISUM) & !is.na(RegData$DLQISUM_POST), ]
aux$dlqisum_diff <- aux$DLQISUM - aux$DLQISUM_POST

indikator5 <- aux[, c("Aar", "SykehusNavn", "AvdRESH", "dlqisum_diff")]
indikator5$Variabel <- 0
indikator5$Variabel[indikator5$dlqisum_diff >= 4] <- 1
indikator5$ind_id <- "hisreg_reduksjon_dlqi"
Indikatorer <- dplyr::bind_rows(Indikatorer, indikator5[, c("Aar", "AvdRESH", "Variabel", "ind_id")])

outfile <- "C:/GIT/hisreg/doc/dlqi_red.pdf"
hisregIndikator(indikatordata = indikator5, tittel=c("Endring i livskvalitet (DLQI)"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')

tmp <- indikator5 %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                               andel = sum(Variabel)/n()*100,
                                                               N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind5_DLQImaal <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
# write.csv2(Ind5_DLQImaal, "I:/hisreg/Ind5_DLQImaal.csv", row.names = F)
########### Indikator 6: Andel pasienter som oppnår reduksjon av VAS på mer enn 30% etter behandling.
#
# Hvordan behandler vi tilfellene der pre_vasscore = 0? Fjerner de i denne beregningen.

aux <- RegData[!is.na(RegData$VASSCORE) & RegData$VASSCORE != 0 & !is.na(RegData$VASSCORE_POST), ]

aux$vasscore_diff_pst <- (aux$VASSCORE - aux$VASSCORE_POST)/aux$VASSCORE*100

indikator6 <- aux[, c("Aar", "SykehusNavn", "AvdRESH", "vasscore_diff_pst")]
indikator6$Variabel <- 0
indikator6$Variabel[indikator6$vasscore_diff_pst > 30] <- 1
indikator6$ind_id <- "hisreg_reduksjon_vas"
Indikatorer <- dplyr::bind_rows(Indikatorer, indikator6[, c("Aar", "AvdRESH", "Variabel", "ind_id")])

outfile <- "C:/GIT/hisreg/doc/vas_red.pdf"
hisregIndikator(indikatordata = indikator6, tittel=c("Endring i smerteskala (VAS)"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')

tmp <- indikator6 %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                                   andel = sum(Variabel)/n()*100,
                                                                   N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind6_VAS_redusert <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
# write.csv2(Ind6_VAS_redusert, "I:/hisreg/Ind6_VAS_redusert.csv", row.names = F)
########### Indikator 7: Andel med kontroll hos hudlege 3 mnd etter medisinsk behandling  ###########
# Hva er nevneren, alle med medisinsk eller kombinasjon kirurgi/medisinsk? Skal c_do_month definere om det er
# 3-mnd-oppfølging eller er det tidsdifferansen mellom Hoveddato og c_date som avgjør?
# I denne versjonen skal c_do_month == 3 og 2.5 < tid_beh_oppf < 5.5.

aux <- RegData[which(RegData$ForlopsType1Num %in% 2:3 & RegData$Aar >= 2016 ), ]
aux$tid_beh_oppf <- lubridate::interval(aux$HovedDato, aux$c_date) %>% lubridate::time_length(unit = 'month')

aux$Variabel <- 0
aux$Variabel[which(aux$tid_beh_oppf > 2.5 & aux$tid_beh_oppf < 5.5)] <- 1


Indikator7_med <- aux %>% group_by(Aar, AvdRESH, MCEID) %>%
  summarise(Variabel = max(Variabel))

Indikator7_med$SykehusNavn <- RegData$SykehusNavn[match(Indikator7_med$AvdRESH, RegData$AvdRESH)]
Indikator7_med$ind_id <- "hisreg_kontr_3mnd_med"
Indikatorer <- dplyr::bind_rows(Indikatorer, Indikator7_med[, c("Aar", "AvdRESH", "Variabel", "ind_id")])

outfile <- "C:/GIT/hisreg/doc/andel_oppf_med.pdf"
hisregIndikator(indikatordata = Indikator7_med, tittel=c("Utført kontroll etter medisinsk behandling"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')

tmp <- Indikator7_med %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                                   andel = sum(Variabel)/n()*100,
                                                                   N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind7_kontroll_hudlege_medbeh <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
# write.csv2(Ind7_kontroll_hudlege_medbeh, "I:/hisreg/Ind7_kontroll_hudlege_medbeh.csv", row.names = F)

########### Indikator 8: Andel med kontroll hos hudlege 6 mnd etter medisinsk behandling  ###########
# Hva er nevneren, alle med kirurgisk eller kombinasjon kirurgi/medisinsk? Skal c_do_month definere om det er
# 6-mnd-oppfølging eller er det tidsdifferansen mellom Hoveddato og c_date som avgjør?
# I denne versjonen skal c_do_month == 6 og 5 < tid_beh_oppf < 8.

aux <- RegData[which(RegData$ForlopsType1Num %in% c(1,3) & RegData$Aar >= 2016 ), ]
aux$tid_beh_oppf <- lubridate::interval(aux$HovedDato, aux$c_date) %>% lubridate::time_length(unit = 'month')

aux$Variabel <- 0
aux$Variabel[which(aux$tid_beh_oppf > 5 & aux$tid_beh_oppf < 8)] <- 1

Indikator8_kir <- aux %>% group_by(Aar, AvdRESH, MCEID) %>%
  summarise(Variabel = max(Variabel))

Indikator8_kir$SykehusNavn <- RegData$SykehusNavn[match(Indikator8_kir$AvdRESH, RegData$AvdRESH)]
Indikator8_kir$ind_id <- "hisreg_kontr_6mnd_kir"
Indikatorer <- dplyr::bind_rows(Indikatorer, Indikator8_kir[, c("Aar", "AvdRESH", "Variabel", "ind_id")])

outfile <- "C:/GIT/hisreg/doc/andel_oppf_kir.pdf"
hisregIndikator(indikatordata = Indikator8_kir, tittel=c("Utført kontroll etter kirurgisk behandling"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')

tmp <- Indikator8_kir %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                                   andel = sum(Variabel)/n()*100,
                                                                   N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind8_kontroll_hudlege_kirbeh <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
# write.csv2(Ind8_kontroll_hudlege_kirbeh, "I:/hisreg/Ind8_kontroll_hudlege_kirbeh.csv", row.names = F)

########### Indikator 9: Komplikasjoner etter kirurgi. ############
# Inkluderer kun forløp der "c_infection", "c_delayed_wound_healing", "c_stricturer", "c_nervedamage", "c_bloodpoisoning",
#   "c_bleeding" eller "c_other_complications" har blitt krysset av for Ja eller Nei.
#################################################################

Indikator9 <- RegData[(RegData$INFECTION_kir %in% 0:1 | RegData$DELAYED_WOUND_HEALING_kir %in% 0:1 |
                         RegData$STRIKTURER_kir %in% 0:1 | RegData$NERVEDAMAGE_kir %in% 0:1 |
                         RegData$BLOODPOISEN_kir %in% 0:1 | RegData$BLEEDING_kir %in% 0:1 |
                         RegData$OTHER_COMPLICATIONS_ID_kir %in% 0:1) & RegData$ForlopsType1Num %in% c(1,3) &
                        RegData$OppflgRegStatus >= 1, ] %>%
  group_by(AvdRESH, Aar, MCEID) %>%
  summarise(Variabel = INFECTION_kir==1 | DELAYED_WOUND_HEALING_kir==1 | STRIKTURER_kir==1 |
              NERVEDAMAGE_kir==1 | BLOODPOISEN_kir==1 | BLEEDING_kir==1 | OTHER_COMPLICATIONS_ID_kir==1)


Indikator9$Variabel[is.na(Indikator9$Variabel)] <- FALSE
Indikator9$Variabel <- as.numeric(Indikator9$Variabel)
Indikator9$SykehusNavn <- RegData$SykehusNavn[match(Indikator9$AvdRESH, RegData$AvdRESH)]
Indikator9$ind_id <- "hisreg_kompl_kir"
Indikatorer <- dplyr::bind_rows(Indikatorer, Indikator9[, c("Aar", "AvdRESH", "Variabel", "ind_id")])

# outfile <- "C:/GIT/hisreg/doc/kompl_kir.pdf"
# hisregIndikator(indikatordata = Indikator9, tittel=c("Komplikasjoner etter kirurgi"), terskel=10, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
#                 legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
#                 lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')

tmp <- Indikator9 %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                    andel = sum(Variabel)/n()*100,
                                                    N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind9_kompl_kir <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
# write.csv2(Ind9_kompl_kir, "I:/hisreg/Ind9_kompl_kir.csv", row.names = F)


table(Indikatorer$ind_id, useNA = 'ifany')

kobl_resh_navn <- RegData[match(unique(RegData$AvdRESH), RegData$AvdRESH), c("AvdRESH", "SykehusNavn")]
kobl_resh_navn$dg_navn <- c("Universitetssykehuset Nord-Norge HF", "St. Olavs Hospital HF",
                            "Helse Stavanger HF", "Haugesund sanitetsforenings revmatismesykehus",
                            "Oslo universitetssykehus HF", "Helse Bergen HF", "",
                            "Nordlandssykehuset HF", "Helse Førde HF")
kobl_resh_navn$orgnr <- c(974795787, 974749025, 974703300, 973156829, 874716782, 974557746,
                          974754118, 974795361, 974744570)

Indikatorer$orgnr <- kobl_resh_navn$orgnr[match(Indikatorer$AvdRESH, kobl_resh_navn$AvdRESH)]
names(Indikatorer)[match(c("Variabel", "Aar"), names(Indikatorer))] <- c("var", "year")
Indikatorer$denominator <- 1

dg <- read.csv2("I:/hisreg/DG_HISREG_2019.csv")
dg$Variabel <- dg$Begge + dg$Kun_hisreg
dg$AvdRESH <- kobl_resh_navn$AvdRESH[match(dg$HF.ideelt.sykehus, kobl_resh_navn$dg_navn)]
dg$orgnr <- kobl_resh_navn$orgnr[match(dg$HF.ideelt.sykehus, kobl_resh_navn$dg_navn)]
dg$orgnr[dg$HF.ideelt.sykehus == "Vestre Viken HF"] <- 894166762
dg$orgnr[dg$HF.ideelt.sykehus == "Helse Møre og Romsdal HF"] <- 997005562
dg$orgnr[dg$HF.ideelt.sykehus == "Helgelandssykehuset HF"] <- 983974929
dg$orgnr[dg$HF.ideelt.sykehus == "Finnmarkssykehuset HF"] <- 983974880

dg$year <- 2019
dg$ind_id <- "hisreg_dg"
names(dg)[match(c("Variabel", "Total"), names(dg))] <- c("var", "denominator")

Indikatorer <- dplyr::bind_rows(Indikatorer[, c("orgnr", "year", "var", "denominator", "ind_id")],
                                dg[, c("orgnr", "year", "var", "denominator", "ind_id")])
Indikatorer$context <- "caregiver"

write.csv2(Indikatorer, "I:/hisreg/indikatorer_m_dg_hisreg_2021_06_23.csv", row.names = F,
           fileEncoding = "UTF-8")

########### Indikator 10: Andel pasienter med bivirkninger rapportert av pasienter ved ################
########### kontroll 3 måneder etter startet medisinsk behandling                   ################
# Inkluderer kun forløp der  "c_medical_treatment",
# "c_systemic_antibiotic_therapy_stop", "c_antiinflammatory_treatment_stop", "c_analgesics_stop",
# "c_corticosteroid_injection_stop","c_acelacic_acid_stop", "c_clindamycin_stop",
# "c_resorcinol_stop" eller "c_other_medication_stop" har blitt krysset av for Ja eller Nei.


# indikator10 <- RegData[which(RegData$ForlopsType1Num %in% c(2,3)),
#                       c("Aar", "SykehusNavn", "AvdRESH", "m_mceid", "c_do_month", "HovedDato", "c_date", "c_medical_treatment",
#                         "c_systemic_antibiotic_therapy_stop", "c_antiinflammatory_treatment_stop", "c_analgesics_stop",
#                         "c_corticosteroid_injection_stop","c_acelacic_acid_stop", "c_clindamycin_stop",
#                         "c_resorcinol_stop", "c_other_medication_stop")]
#
# indikator10 <- indikator10[indikator10$c_medical_treatment %in% 1:2 | indikator10$c_systemic_antibiotic_therapy_stop %in% 1:2 |
#                              indikator10$c_antiinflammatory_treatment_stop %in% 1:2 | indikator10$c_acelacic_acid_stop %in% 1:2 |
#                              indikator10$c_clindamycin_stop %in% 1:2 | indikator10$c_resorcinol_stop %in% 1:2 |
#                              indikator10$c_other_medication_stop %in% 1:2, ] %>%
#   group_by(AvdRESH, Aar, m_mceid) %>%
#   summarise(Variabel = c_medical_treatment==1 | c_systemic_antibiotic_therapy_stop==1 | c_antiinflammatory_treatment_stop==1 |
#               c_analgesics_stop==1 | c_corticosteroid_injection_stop==1 | c_acelacic_acid_stop==1 | c_clindamycin_stop==1 |
#               c_resorcinol_stop==1 | c_other_medication_stop==1)
#
# indikator10 <- RegData[which(RegData$ForlopsType1Num %in% c(2,3)),
#                        c("Aar", "SykehusNavn", "AvdRESH", "MCEID", "HovedDato", "c_date",
#                          "SYSTEMIC_ANTIBIOTIC_THERAPY", "c_antiinflammatory_treatment_stop", "c_analgesics_stop",
#                          "c_corticosteroid_injection_stop","c_acelacic_acid_stop", "c_clindamycin_stop",
#                          "c_resorcinol_stop", "c_other_medication_stop")]
#
# indikator10 <- indikator10[indikator10$c_medical_treatment %in% 1:2 | indikator10$c_systemic_antibiotic_therapy_stop %in% 1:2 |
#                              indikator10$c_antiinflammatory_treatment_stop %in% 1:2 | indikator10$c_acelacic_acid_stop %in% 1:2 |
#                              indikator10$c_clindamycin_stop %in% 1:2 | indikator10$c_resorcinol_stop %in% 1:2 |
#                              indikator10$c_other_medication_stop %in% 1:2, ] %>%
#   group_by(AvdRESH, Aar, m_mceid) %>%
#   summarise(Variabel = c_medical_treatment==1 | c_systemic_antibiotic_therapy_stop==1 | c_antiinflammatory_treatment_stop==1 |
#               c_analgesics_stop==1 | c_corticosteroid_injection_stop==1 | c_acelacic_acid_stop==1 | c_clindamycin_stop==1 |
#               c_resorcinol_stop==1 | c_other_medication_stop==1)
#
# indikator10$Variabel[is.na(indikator10$Variabel)] <- FALSE
# indikator10$Variabel <- as.numeric(indikator10$Variabel)
# indikator10$SykehusNavn <- RegData$SykehusNavn[match(indikator10$AvdRESH, RegData$AvdRESH)]
# Ind10 <- indikator10[, -c(3,5)]
# names(Ind10) <- c( 'ReshId', 'Aar', 'Teller Ind10')
# Ind10[, 'Nevner Ind10'] <- 1
# Ind10$Indikator <- 'Ind10'
# Ind10$AarID <- paste0(Ind10$Aar, Ind10$ReshId)
# Ind10 <- Ind10[, c(2,5,4,3,1,6)]
#
# # write.csv2(Ind10, "Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/7. HisReg/Indikatorer/indikator10_bivirkn_med.csv",
# #            row.names = F)
#
#
# tmp <- indikator10 %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
#                                                                 andel = sum(Variabel)/n()*100,
#                                                                 N=n())
#
# tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
# Ind10_bivirkn_med <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
# # write.csv2(Ind10_bivirkn_med, "I:/hisreg/Ind10_bivirkn_med.csv", row.names = F)




# CONTROL_STOP_MEDICAL_TREATMENT
# CONTROL_SYSTEMIC_ANTIBIOTIC_THERAPY_STOP
# CONTROL_ANTIINFLAMMATORY_TREATMENT_STOP
# CONTROL_ANALGESICS_STOP
# CONTROL_INTRALESIONAL_CORTICOSTEROID_INJECTION_STOP
# CONTROL_ACELACIC_ACID_STOP
# CONTROL_CLINDAMYCIN_STOP
# CONTROL_RESORCINOL_STOP
# CONTROL_MEDICAL_INT_OTHER_STOP
