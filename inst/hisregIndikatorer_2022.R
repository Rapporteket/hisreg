rm(list = ls())
library(hisreg)
library(tidyverse)
library(lubridate)

hisregdata <- hisreg::lastShinyHisreg()
RegData <- hisregdata$RegData

rapp_aar <- 2022
RegData <- RegData[which(RegData$Aar <= rapp_aar), ]
figfolder <- "~/mydata/hisreg/hisreg_ind_2022/"
if (!dir.exists(figfolder)) {
  dir.create(figfolder)
}
utformat <- "svg"

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

outfile <- paste0(figfolder, "henvist_spes.pdf")
hisregIndikator(indikatordata = indikator1, tittel=c("Andel henvist til hudspesialist innen", "1 år av første besøk hos allmennlege"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisregIndikator(indikatordata = indikator1, tittel=c("Andel henvist til hudspesialist innen", "1 år av første besøk hos allmennlege"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                  legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                  lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
}
outfile <- paste0(figfolder, "henvist_spes_v2.pdf")
hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator1, tittel=c("Andel henvist til hudspesialist innen", "1 år av første besøk hos allmennlege"),
                                     minstekrav = 60, maal = 80, outfile = outfile)
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator1, tittel=c("Andel henvist til hudspesialist innen", "1 år av første besøk hos allmennlege"),
                                       minstekrav = 60, maal = 80, outfile = outfile)
}

tmp <- indikator1 %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                               andel = sum(Variabel)/n()*100,
                                                               N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind1_tid_almlege_spes <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
# write.csv2(Ind1_tid_almlege_spes, "I:/hisreg/Ind1_tid_almlege_spes.csv", row.names = F)


########### Indikator 2: Reduksjon av iHS4 score ved kontroll. ##############3
# Andel pasienter som har en reduksjon av alvorlighetsgrad ved iHS4 etter behandling
# Måloppnåelse – Høy: >90 %, Moderat: 90-70 %, Lav: <70 %

aux <- RegData[!is.na(RegData$IHS4SCORE) & !is.na(RegData$IHS4SCORE_POST), ]
aux$ihs4_diff <- aux$IHS4SCORE - aux$IHS4SCORE_POST

indikator2 <- aux[, c("Aar", "SykehusNavn", "AvdRESH", "ihs4_diff")]
indikator2$Variabel <- 0
indikator2$Variabel[indikator2$ihs4_diff > 0] <- 1
indikator2$ind_id <- "hisreg_reduksjon_ihs4"
Indikatorer <- dplyr::bind_rows(indikator1[, c("Aar", "AvdRESH", "Variabel", "ind_id")],
                                indikator2[, c("Aar", "AvdRESH", "Variabel", "ind_id")])

outfile <- paste0(figfolder, "ihs4_red.pdf")
hisregIndikator(indikatordata = indikator2, tittel=c("Reduksjon av iHS4 score ", "ved kontroll"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisregIndikator(indikatordata = indikator2, tittel=c("Reduksjon av iHS4 score ", "ved kontroll"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                  legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                  lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
}
outfile <- paste0(figfolder, "ihs4_red_v2.pdf")
hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator2, tittel=c("Reduksjon av iHS4 score ", "ved kontroll"),
                                     minstekrav = 70, maal = 90, outfile = outfile)
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator2, tittel=c("Reduksjon av iHS4 score ", "ved kontroll"),
                                       minstekrav = 70, maal = 90, outfile = outfile)
}


############## Indikator 3: HiSCR på >50 % ved kontroll
# Andel pasienter som er blitt vurdert til HiSCR på >50 % etter behandling.
# Måloppnåelse – Høy: >90 %, Moderat: 90-70 %, Lav: <70 %

aux <- RegData[!is.na(RegData$HISCR_CATEGORY), ]

indikator3 <- aux[, c("Aar", "SykehusNavn", "AvdRESH", "HISCR_CATEGORY")]
indikator3$Variabel <- 0
indikator3$Variabel[indikator3$HISCR_CATEGORY == "Full-Responder"] <- 1
indikator3$ind_id <- "full_responder"
Indikatorer <- dplyr::bind_rows(Indikatorer[, c("Aar", "AvdRESH", "Variabel", "ind_id")],
                                indikator3[, c("Aar", "AvdRESH", "Variabel", "ind_id")])

outfile <- paste0(figfolder, "full_responder.pdf")
hisregIndikator(indikatordata = indikator3, tittel=c("Oppnådd HiSCR ved kontroll"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisregIndikator(indikatordata = indikator3, tittel=c("Oppnådd HiSCR ved kontroll"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                  legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                  lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
}
outfile <- paste0(figfolder, "full_responder_v2.pdf")
hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator3, tittel=c("Oppnådd HiSCR ved kontroll"),
                                     minstekrav = 70, maal = 90, outfile = outfile)
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator3, tittel=c("Oppnådd HiSCR ved kontroll"),
                                       minstekrav = 70, maal = 90, outfile = outfile)
}



########### Indikator 4: Andel pasienter (ELLER FORLØP) som oppnår reduksjon fra Hurley 3/2 til 2 eller 1 etter behandling.
# Inkluderer alle som har Hurley 2 eller 3 ved prekontroll, og som har registrert Hurley ved oppfølging

aux <- RegData[RegData$HURLEY_SCORE %in% 2:3 & !is.na(RegData$HURLEY_SCORE_POST), ]
aux$hurley_diff <- aux$HURLEY_SCORE - aux$HURLEY_SCORE_POST

indikator4 <- aux[, c("Aar", "SykehusNavn", "AvdRESH", "hurley_diff")]
indikator4$Variabel <- 0
indikator4$Variabel[indikator4$hurley_diff > 0] <- 1
indikator4$ind_id <- "hisreg_reduksjon_hurley"
Indikatorer <- dplyr::bind_rows(Indikatorer[, c("Aar", "AvdRESH", "Variabel", "ind_id")],
                                indikator4[, c("Aar", "AvdRESH", "Variabel", "ind_id")])

outfile <- paste0(figfolder, "hurley_red.pdf")
hisregIndikator(indikatordata = indikator4, tittel=c("Reduksjon i Hurley score ", "ved kontroll"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisregIndikator(indikatordata = indikator4, tittel=c("Reduksjon i Hurley score ", "ved kontroll"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                  legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                  lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
}
outfile <- paste0(figfolder, "hurley_red_v2.pdf")
hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator4, tittel=c("Reduksjon i Hurley score ", "ved kontroll"),
                                     minstekrav = 70, maal = 90, outfile = outfile)
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator4, tittel=c("Reduksjon i Hurley score ", "ved kontroll"),
                                       minstekrav = 70, maal = 90, outfile = outfile)
}

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

outfile <- paste0(figfolder, "dlqi_red.pdf")
hisregIndikator(indikatordata = indikator5, tittel=c("Endring i livskvalitet (DLQI)"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisregIndikator(indikatordata = indikator5, tittel=c("Endring i livskvalitet (DLQI)"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                  legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                  lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
}
outfile <- paste0(figfolder, "dlqi_red_v2.pdf")
hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator5, tittel=c("Endring i livskvalitet (DLQI)"),
                                     minstekrav = 70, maal = 90, outfile = outfile)
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator5, tittel=c("Endring i livskvalitet (DLQI)"),
                                       minstekrav = 70, maal = 90, outfile = outfile)
}

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

outfile <- paste0(figfolder, "vas_red.pdf")
hisregIndikator(indikatordata = indikator6, tittel=c("Endring i smerteskala (VAS)"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisregIndikator(indikatordata = indikator6, tittel=c("Endring i smerteskala (VAS)"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                  legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                  lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
}
outfile <- paste0(figfolder, "vas_red_v2.pdf")
hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator6, tittel=c("Endring i smerteskala (VAS)"),
                                     minstekrav = 70, maal = 90, outfile = outfile)
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator6, tittel=c("Endring i smerteskala (VAS)"),
                                       minstekrav = 70, maal = 90, outfile = outfile)
}

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

outfile <- paste0(figfolder, "andel_oppf_med.pdf")
hisregIndikator(indikatordata = Indikator7_med, tittel=c("Utført kontroll etter medisinsk behandling"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisregIndikator(indikatordata = Indikator7_med, tittel=c("Utført kontroll etter medisinsk behandling"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                  legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                  lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
}
outfile <- paste0(figfolder, "andel_oppf_med_v2.pdf")
hisreg::hisregFigIndikator_aarsamlet(indikatordata = Indikator7_med, tittel=c("Utført kontroll etter medisinsk behandling"),
                                     minstekrav = 80, maal = 90, outfile = outfile)
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisreg::hisregFigIndikator_aarsamlet(indikatordata = Indikator7_med, tittel=c("Utført kontroll etter medisinsk behandling"),
                                       minstekrav = 80, maal = 90, outfile = outfile)
}

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

outfile <- paste0(figfolder, "andel_oppf_kir.pdf")
hisregIndikator(indikatordata = Indikator8_kir, tittel=c("Utført kontroll etter kirurgisk behandling"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisregIndikator(indikatordata = Indikator8_kir, tittel=c("Utført kontroll etter kirurgisk behandling"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                  legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                  lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
}
outfile <- paste0(figfolder, "andel_oppf_kir_v2.pdf")
hisreg::hisregFigIndikator_aarsamlet(indikatordata = Indikator8_kir, tittel=c("Utført kontroll etter kirurgisk behandling"),
                                     minstekrav = 80, maal = 90, outfile = outfile)
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisreg::hisregFigIndikator_aarsamlet(indikatordata = Indikator8_kir, tittel=c("Utført kontroll etter kirurgisk behandling"),
                                       minstekrav = 80, maal = 90, outfile = outfile)
}

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

outfile <- paste0(figfolder, "kompl_kir.pdf")
hisregIndikator(indikatordata = Indikator9, tittel=c("Komplikasjoner etter kirurgi"), terskel=10, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=T, outfile = outfile,
                lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisregIndikator(indikatordata = Indikator9, tittel=c("Komplikasjoner etter kirurgi"), terskel=10, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                  legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=T, outfile = outfile,
                  lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
}
outfile <- paste0(figfolder, "kompl_kir_v2.pdf")
hisreg::hisregFigIndikator_aarsamlet(indikatordata = Indikator9, tittel=c("Komplikasjoner etter kirurgi"),
                                     minstekrav = 30, maal = 5, decreasing = T, outfile = outfile)
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisreg::hisregFigIndikator_aarsamlet(indikatordata = Indikator9, tittel=c("Komplikasjoner etter kirurgi"),
                                       minstekrav = 30, maal = 5, decreasing = T, outfile = outfile)
}

tmp <- Indikator9 %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                               andel = sum(Variabel)/n()*100,
                                                               N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind9_kompl_kir <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
# write.csv2(Ind9_kompl_kir, "I:/hisreg/Ind9_kompl_kir.csv", row.names = F)


############# Indikator 11: Pasienttilfredshet etter behandling
# Andel pasienter som er fornøyd med behandlingen som ble gitt på sykehuset.
# Måloppnåelse – Høy: >80%, Moderat: 80-60%, Lav: <60 %

aux <- RegData[RegData$SATISFACTION %in% 1:5, ]
indikator11 <- aux[, c("Aar", "SykehusNavn", "AvdRESH", "SATISFACTION")]
indikator11$Variabel <- 0
indikator11$Variabel[indikator11$SATISFACTION %in% 1:2] <- 1
indikator11$ind_id <- "pasientfornoydhet"
Indikatorer <- dplyr::bind_rows(Indikatorer[, c("Aar", "AvdRESH", "Variabel", "ind_id")],
                                indikator11[, c("Aar", "AvdRESH", "Variabel", "ind_id")])

outfile <- paste0(figfolder, "pasientfornoydhet.pdf")
hisregIndikator(indikatordata = indikator11, tittel=c("Andel pasienter fornøyd", "med behandlingen"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisregIndikator(indikatordata = indikator11, tittel=c("Andel pasienter fornøyd", "med behandlingen"), terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                  legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = outfile,
                  lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy')
}
outfile <- paste0(figfolder, "pasientfornoydhet_v2.pdf")
hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator11, tittel=c("Andel pasienter fornøyd", "med behandlingen"),
                                     minstekrav = 60, maal = 80, outfile = outfile)
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator11, tittel=c("Andel pasienter fornøyd", "med behandlingen"),
                                       minstekrav = 60, maal = 80, outfile = outfile)
}

########### Ny indikator, andel biologisk behandlede med tidligere antibiotisk behandling #############

aux <- RegData[which(RegData$BIOLOGICAL_TREATMENT == 1), ]
indikator_ny <- aux[, c("Aar", "SykehusNavn", "AvdRESH", "MEDICATION_HISTORY_ANTIBIOTIC", "DLQISUM")]
indikator_ny$Variabel <- 0
indikator_ny$Variabel[indikator_ny$MEDICATION_HISTORY_ANTIBIOTIC %in% 1:3 ] <- 1
indikator_ny$ind_id <- "tidl_antibiotisk"

outfile <- paste0(figfolder, "tidl_antibiotisk.pdf")
hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator_ny,
                                     tittel=c("Andel biologisk behandlede ", "med tidligere antibiotisk behandling"),
                                     minstekrav = NA, maal = NA, outfile = outfile)
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator_ny,
                                       tittel=c("Andel biologisk behandlede ", "med tidligere antibiotisk behandling"),
                                       minstekrav = NA, maal = NA, outfile = outfile)
}


########### Ny indikator, andel biologisk behandlede med dlqisum over 10 #############

## Alle inkludert
aux <- RegData[which(RegData$BIOLOGICAL_TREATMENT == 1 & !is.na(RegData$DLQISUM)), ]

indikator_ny <- aux[, c("Aar", "SykehusNavn", "AvdRESH", "DLQISUM")]
indikator_ny$Variabel <- 0
indikator_ny$Variabel[indikator_ny$DLQISUM >= 11] <- 1
indikator_ny$ind_id <- "dlqikrav_oppfylt"
outfile <- paste0(figfolder, "dlqikrav_oppfylt.pdf")
hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator_ny,
                                     tittel=c("Andel biologisk behandlede med DLQI > 10", "før behandling"),
                                     minstekrav = NA, maal = NA, outfile = outfile)
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator_ny,
                                       tittel=c("Andel biologisk behandlede med DLQI > 10", "før behandling"),
                                       minstekrav = NA, maal = NA, outfile = outfile)
}

########### Ny indikator, andel biologisk behandlede med tidligere antibiotisk behandling og med dlqi preskår over 10 #############

aux <- RegData[which(RegData$BIOLOGICAL_TREATMENT == 1 & !is.na(RegData$DLQISUM)), ]
indikator_ny <- aux[, c("Aar", "SykehusNavn", "AvdRESH", "MEDICATION_HISTORY_ANTIBIOTIC", "DLQISUM")]
indikator_ny$Variabel <- 0
indikator_ny$Variabel[indikator_ny$MEDICATION_HISTORY_ANTIBIOTIC %in% 1:3 & indikator_ny$DLQISUM >= 11] <- 1
indikator_ny$ind_id <- "tidl_antibiotisk_dlqikrav_oppfylt"

outfile <- paste0(figfolder, "tidl_antibiotisk_dlqikrav_oppfylt.pdf")
hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator_ny,
                                     tittel=c("Andel biologisk behandlede med DLQI > 10 før",
                                              "behandling og med tidligere antibiotisk behandling"),
                                     minstekrav = NA, maal = NA, outfile = outfile)
if (!is.na(utformat)) {
  outfile <- paste0(substr(outfile, 1,nchar(outfile)-3), utformat)
  hisreg::hisregFigIndikator_aarsamlet(indikatordata = indikator_ny,
                                       tittel=c("Andel biologisk behandlede med DLQI > 10 før",
                                                "behandling og med tidligere antibiotisk behandling"),
                                       minstekrav = NA, maal = NA, outfile = outfile)
}

#####################################3

kobl_resh_navn <- data.frame(AvdRESH = c("601031",
                       "102449",
                       "105489",
                       "106868",
                       "700395",
                       "102980",
                       "4214908",
                       "4209879",
                       "105246",
                       "4216423",
                       "4216811",
                       "4210131"),
           SykehusNavn = c("UNN, Tromsø",
                           "St. Olavs Hospital",
                           "Stavanger u.s.",
                           "Haugesund rev.",
                           "OUS, Rikshospitalet",
                           "Haukeland u.s.",
                           "Hudpoliklinikken Levanger",
                           "Nordlandssykehuset, Bodø",
                           "Førde sjukehus",
                           "Hønefoss hudlegekontor",
                           "Ålesund sjukehus",
                           "Hudlegekontoret i Halden"),
           dg_navn = c("Universitetssykehuset Nord-Norge HF",
                       "St. Olavs Hospital HF",
                       "Helse Stavanger HF",
                       "Haugesund sanitetsforenings revmatismesykehus",
                       "Oslo universitetssykehus HF",
                       "Helse Bergen HF",
                       "",
                       "Nordlandssykehuset HF",
                       "Helse Førde HF",
                       "",
                       "Helse Møre og Romsdal HF",
                       ""),
           orgnr = c(974795787, 974749025, 974703300, 973156829, 874716782, 974557746,
                     974754118, 974795361, 974744570, 0, 974747138, 0)
)


Indikatorer$orgnr <- kobl_resh_navn$orgnr[match(Indikatorer$AvdRESH, kobl_resh_navn$AvdRESH)]
names(Indikatorer)[match(c("Variabel", "Aar"), names(Indikatorer))] <- c("var", "year")
Indikatorer$denominator <- 1

# dg <- read.csv2("~/.ssh/DG_HISREG_2019.csv", fileEncoding = "Latin1")
# dg$Variabel <- dg$Begge + dg$Kun_hisreg
# dg$AvdRESH <- kobl_resh_navn$AvdRESH[match(dg$HF.ideelt.sykehus, kobl_resh_navn$dg_navn)]
# dg$orgnr <- kobl_resh_navn$orgnr[match(dg$HF.ideelt.sykehus, kobl_resh_navn$dg_navn)]
# dg$orgnr[dg$HF.ideelt.sykehus == "Vestre Viken HF"] <- 894166762
# # dg$orgnr[dg$HF.ideelt.sykehus == "Helse Møre og Romsdal HF"] <- 997005562
# dg$orgnr[dg$HF.ideelt.sykehus == "Helgelandssykehuset HF"] <- 983974929
# dg$orgnr[dg$HF.ideelt.sykehus == "Finnmarkssykehuset HF"] <- 983974880
#
# dg$year <- 2019
# dg$ind_id <- "hisreg_dg"
# names(dg)[match(c("Variabel", "Total"), names(dg))] <- c("var", "denominator")
#
# Indikatorer <- dplyr::bind_rows(Indikatorer[, c("orgnr", "year", "var", "denominator", "ind_id")],
#                                 dg[, c("orgnr", "year", "var", "denominator", "ind_id")])
# Indikatorer$context <- "caregiver"

