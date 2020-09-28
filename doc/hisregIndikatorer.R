rm(list = ls())
library(hisreg)
library(tidyverse)
library(lubridate)

ForlopsData <- read.table("I:/hisreg/ForlopsOversikt2020-08-21 13-19-34.txt",
                          header = TRUE, sep = ";", encoding = "UTF-8")
AlleVarNum <- read.table("I:/hisreg/AlleVarNum2020-08-21 13-19-34.txt",
                      header = TRUE, sep = ";", encoding = "UTF-8")
SkjemaOversikt <- read.table("I:/hisreg/SkjemaOversikt2020-08-21 13-19-34.txt",
                             header = TRUE, sep = ";", encoding = "UTF-8")
RegDataLabel <- read.table("I:/hisreg/AlleVar2020-08-21 13-19-34.txt",
                           header = TRUE, sep = ";", encoding = "UTF-8")
Followups_all <- read.table("I:/hisreg/FollowupsNum2020-08-21 13-19-34.txt",
                        header = TRUE, sep = ";", encoding = "UTF-8")
# FollowupsAll <- FollowupsAll %>% group_by(c_mceid) %>%
#   summarise(c_date_3mnd = c_date[c_do_month])

RegData <- AlleVarNum[AlleVarNum$r_resh_id != 999002, ]

#[, c("m_mceid", "p_age_abscess",
                       # "p_education", "p_surgery", "p_age_doctor", "p_age_specialist",
                       # "p_antibiotics", "pre_smoking",
                       # "pre_work", "pre_bmi", "pre_dlqisum",
                       # "pre_hsscoresum", "pre_vasscore",
                       # "pre_hurley_score", "i_type",
                       # "i_surgery_type",
                       # "i_biological_treatment",
                       # "i_antibiotic_therapy",
                       # "i_antiinflammatory_treatment",
                       # "i_analgesics",
                       # "i_localized_med_treatment",
                       # "i_aksille", "i_lyske", "i_pubis",
                       # "i_genitalt", "i_peritalt", "i_glutealt",
                       # "i_mammae", "i_other_location",
                       # "i_rifampicin_clindamycin",
                       # "i_tetracyclin_lymecyclin",
                       # "i_amoxicilin", "i_sys_antibiotic_other",
                       # "i_corticosteroid_injection",
                       # "i_acelacic_acid", "i_clindamycin",
                       # "i_resorcinol",
                       # "i_medical_other")]

# Fjerner Oppfølging ikke mulig
Followups <- Followups_all[Followups_all$c_possible != 2, ]
# Fjerner oppfølginger uten koblet forløpsid
Followups <- Followups[!is.na(Followups$c_mceid), ]
Followups <- Followups[order(Followups$c_do_month,
                             decreasing = FALSE), ]
# Velg første oppfølging
Followups <- Followups[match(unique(Followups$c_mceid),
                             Followups$c_mceid), ]
# Followups <- Followups[, c("c_mceid", "c_date", "c_do_month",
#                            "c_infection", "c_delayed_wound_healing",
#                            "c_stricturer", "c_nervedamage",
#                            "c_bloodpoisoning", "c_bleeding",
#                            "c_other_complications", "c_dlqisum",
#                            "c_hsscoresum", "c_vasscore",
#                            "c_hurley_score")]
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH",
                               "BasisRegStatus", "HovedDato",
                               "PasientAlder", "OppflgRegStatus",
                               "ForlopsType1", "ForlopsType1Num",
                               "ForlopsType2", "ForlopsType2Num",
                               "ErMann", "SykehusNavn", "PasientID")]
ForlopsData <- ForlopsData[ForlopsData$AvdRESH != 999002, ] # Fjern Roskilde
RegData <- merge(RegData,
                 ForlopsData,
                 by.x = "m_mceid",
                 by.y = "ForlopsID")
FollowupsAll <- Followups_all[!is.na(Followups_all$c_mceid), ]
fulldata <- merge(RegData, FollowupsAll,
                  by.x = "m_mceid",
                  by.y = "c_mceid",
                  all.x = T)
RegData <- merge(RegData, Followups,
                 by.x = "m_mceid",
                 by.y = "c_mceid",
                 all.x = T)
RegData <- hisregPreprosess(RegData)

fulldata <- hisregPreprosess(fulldata)

RegData <- RegData[which(RegData$Aar <= 2019), ]
fulldata <- fulldata[which(fulldata$Aar <= 2019), ]

write.csv2(fulldata, "I:/hisreg/explore.csv", row.names = F, fileEncoding = "Latin1")

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



RegData$tid_almlege_spes <- RegData$p_age_specialist - RegData$p_age_doctor
# table(is.na(RegData$p_time_specialist), is.na(RegData$tid_almlege_spes), useNA = 'ifany')

aux <- RegData[!is.na(RegData$tid_almlege_spes) | !is.na(RegData$p_time_specialist), ]

aux$tid_almlege_spes_kombo <- aux$p_time_specialist
aux$tid_almlege_spes_kombo[is.na(aux$p_time_specialist)] <- aux$tid_almlege_spes[is.na(aux$p_time_specialist)]
aux <- aux[order(aux$HovedDato, decreasing = F), ]
aux <- aux[match(unique(aux$m_pid), aux$m_pid), ]

indikator1 <- aux[, c("Aar", "SykehusNavn", "AvdRESH", "tid_almlege_spes_kombo")]
indikator1$Variabel <- 0
indikator1$Variabel[indikator1$tid_almlege_spes == 0] <- 1
indikator1 <- indikator1[indikator1$Aar %in% 2015:2019, ]
Ind1 <- indikator1[, -c(2,4)]
names(Ind1) <- c('Aar', 'ReshId', 'Teller Ind1')
Ind1[, 'Nevner Ind1'] <- 1
Ind1$Indikator <- 'Ind1'
Ind1$AarID <- paste0(Ind1$Aar, Ind1$ReshId)
Ind1 <- Ind1[, c(1,5,4,3,2,6)]

write.csv2(Ind1, "Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/7. HisReg/Indikatorer/indikator1_tid_almlege_spes.csv",
           row.names = F)

tmp <- indikator1 %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                               andel = sum(Variabel)/n()*100,
                                                               N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind1_tid_almlege_spes <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
write.csv2(Ind1_tid_almlege_spes, "I:/hisreg/Ind1_tid_almlege_spes.csv", row.names = F)

########### Indikator 4: Andel pasienter (ELLER FORLØP) som oppnår reduksjon fra Hurley 3/2 til 2 eller 1 etter behandling.
# Inkluderer alle som har Hurley 2 eller 3 ved prekontroll, og som har registrert Hurley ved oppfølging

aux <- RegData[RegData$pre_hurley_score %in% 2:3 & !is.na(RegData$c_hurley_score), ]
aux$hurley_diff <- aux$pre_hurley_score - aux$c_hurley_score

indikator4 <- aux[, c("Aar", "SykehusNavn", "AvdRESH", "hurley_diff")]
indikator4$Variabel <- 0
indikator4$Variabel[indikator4$hurley_diff > 0] <- 1
Ind4 <- indikator4[, -c(2,4)]
names(Ind4) <- c('Aar', 'ReshId', 'Teller Ind4')
Ind4[, 'Nevner Ind4'] <- 1
Ind4$Indikator <- 'Ind4'
Ind4$AarID <- paste0(Ind4$Aar, Ind4$ReshId)
Ind4 <- Ind4[, c(1,5,4,3,2,6)]

write.csv2(Ind4, "Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/7. HisReg/Indikatorer/indikator4_hurley.csv",
           row.names = F)

tmp <- indikator4 %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                               andel = sum(Variabel)/n()*100,
                                                               N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind4_hurley_redusert <-tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
write.csv2(Ind4_hurley_redusert, "I:/hisreg/Ind4_hurley_redusert.csv", row.names = F)
########### Indikator 5: Andel pasienter som oppnår behandlingsmålet (DLQI <4) for pasientvurdert dermatologisk livskvalitet.
#
# Bruker pre_dlqisum - c_dlqisum >= 4

aux <- RegData[!is.na(RegData$pre_dlqisum) & !is.na(RegData$c_dlqisum), ]
aux$dlqisum_diff <- aux$pre_dlqisum - aux$c_dlqisum

indikator5 <- aux[, c("Aar", "SykehusNavn", "AvdRESH", "dlqisum_diff")]
indikator5$Variabel <- 0
indikator5$Variabel[indikator5$dlqisum_diff >= 4] <- 1
Ind5 <- indikator5[, -c(2,4)]
names(Ind5) <- c('Aar', 'ReshId', 'Teller Ind5')
Ind5[, 'Nevner Ind5'] <- 1
Ind5$Indikator <- 'Ind5'
Ind5$AarID <- paste0(Ind5$Aar, Ind5$ReshId)
Ind5 <- Ind5[, c(1,5,4,3,2,6)]

write.csv2(Ind5, "Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/7. HisReg/Indikatorer/indikator5_dlqi.csv",
           row.names = F)

tmp <- indikator5 %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                               andel = sum(Variabel)/n()*100,
                                                               N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind5_DLQImaal <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
write.csv2(Ind5_DLQImaal, "I:/hisreg/Ind5_DLQImaal.csv", row.names = F)
########### Indikator 6: Andel pasienter som oppnår reduksjon av VAS på mer enn 30% etter behandling.
#
# Hvordan behandler vi tilfellene der pre_vasscore = 0? Fjerner de i denne beregningen.

aux <- RegData[!is.na(RegData$pre_vasscore) & RegData$pre_vasscore != 0 & !is.na(RegData$c_vasscore), ]

aux$vasscore_diff_pst <- (aux$pre_vasscore - aux$c_vasscore)/aux$pre_vasscore*100

indikator6 <- aux[, c("Aar", "SykehusNavn", "AvdRESH", "vasscore_diff_pst")]
indikator6$Variabel <- 0
indikator6$Variabel[indikator6$vasscore_diff_pst > 30] <- 1
Ind6 <- indikator6[, -c(2,4)]
names(Ind6) <- c('Aar', 'ReshId', 'Teller Ind6')
Ind6[, 'Nevner Ind6'] <- 1
Ind6$Indikator <- 'Ind6'
Ind6$AarID <- paste0(Ind6$Aar, Ind6$ReshId)
Ind6 <- Ind6[, c(1,5,4,3,2,6)]

write.csv2(Ind6, "Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/7. HisReg/Indikatorer/indikator6_vas.csv",
           row.names = F)

tmp <- indikator6 %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                                   andel = sum(Variabel)/n()*100,
                                                                   N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind6_VAS_redusert <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
write.csv2(Ind6_VAS_redusert, "I:/hisreg/Ind6_VAS_redusert.csv", row.names = F)
########### Indikator 7: Andel med kontroll hos hudlege 3 mnd etter medisinsk behandling  ###########
# Hva er nevneren, alle med medisinsk eller kombinasjon kirurgi/medisinsk? Skal c_do_month definere om det er
# 3-mnd-oppfølging eller er det tidsdifferansen mellom Hoveddato og c_date som avgjør?
# I denne versjonen skal c_do_month == 3 og 2.5 < tid_beh_oppf < 5.5.

fulldata$tid_beh_oppf <- lubridate::interval(fulldata$HovedDato, fulldata$c_date) %>% lubridate::time_length(unit = 'month')
fulldata$c_date <- as.Date(fulldata$c_date)

oppfolgdata <- fulldata[which(fulldata$Aar >= 2016 & !is.na(fulldata$Intervensjon)),
                        c("m_mceid", "Intervensjon", "ForlopsType1Num", "AvdRESH", "Aar", "HovedDato", "c_date",
                          "c_do_month", "tid_beh_oppf", "c_possible")]

aux <- oppfolgdata[which(oppfolgdata$ForlopsType1Num %in% 2:3), ]
aux$Variabel <- 0
aux$Variabel[which(aux$c_do_month == 3 & aux$tid_beh_oppf > 2.5 & aux$tid_beh_oppf < 5.5)] <- 1


Indikator7_med <- aux %>% group_by(Aar, AvdRESH, m_mceid) %>%
  summarise(Variabel = max(Variabel))

Indikator7_med$SykehusNavn <- RegData$SykehusNavn[match(Indikator7_med$AvdRESH, RegData$AvdRESH)]
Ind7 <- Indikator7_med[, -c(3,5)]
names(Ind7) <- c('Aar', 'ReshId', 'Teller Ind7')
Ind7[, 'Nevner Ind7'] <- 1
Ind7$Indikator <- 'Ind7'
Ind7$AarID <- paste0(Ind7$Aar, Ind7$ReshId)
Ind7 <- Ind7[, c(1,5,4,3,2,6)]

write.csv2(Ind7, "Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/7. HisReg/Indikatorer/indikator7_kontr_3mnd_med.csv",
           row.names = F)

tmp <- Indikator7_med %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                                   andel = sum(Variabel)/n()*100,
                                                                   N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind7_kontroll_hudlege_medbeh <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
write.csv2(Ind7_kontroll_hudlege_medbeh, "I:/hisreg/Ind7_kontroll_hudlege_medbeh.csv", row.names = F)

########### Indikator 8: Andel med kontroll hos hudlege 6 mnd etter medisinsk behandling  ###########
# Hva er nevneren, alle med kirurgisk eller kombinasjon kirurgi/medisinsk? Skal c_do_month definere om det er
# 6-mnd-oppfølging eller er det tidsdifferansen mellom Hoveddato og c_date som avgjør?
# I denne versjonen skal c_do_month == 6 og 5 < tid_beh_oppf < 8.
aux <- oppfolgdata[which(oppfolgdata$ForlopsType1Num %in% c(1,3)), ]
aux$Variabel <- 0
aux$Variabel[which(aux$c_do_month == 6 & aux$tid_beh_oppf > 5 & aux$tid_beh_oppf < 8)] <- 1


Indikator8_kir <- aux %>% group_by(Aar, AvdRESH, m_mceid) %>%
  summarise(Variabel = max(Variabel))

Indikator8_kir$SykehusNavn <- RegData$SykehusNavn[match(Indikator8_kir$AvdRESH, RegData$AvdRESH)]
Ind8 <- Indikator8_kir[, -c(3,5)]
names(Ind8) <- c('Aar', 'ReshId', 'Teller Ind8')
Ind8[, 'Nevner Ind8'] <- 1
Ind8$Indikator <- 'Ind8'
Ind8$AarID <- paste0(Ind8$Aar, Ind8$ReshId)
Ind8 <- Ind8[, c(1,5,4,3,2,6)]

write.csv2(Ind8, "Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/7. HisReg/Indikatorer/Indikator8_kontr_6mnd_kir.csv",
           row.names = F)

tmp <- Indikator8_kir %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                                   andel = sum(Variabel)/n()*100,
                                                                   N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind8_kontroll_hudlege_kirbeh <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
write.csv2(Ind8_kontroll_hudlege_kirbeh, "I:/hisreg/Ind8_kontroll_hudlege_kirbeh.csv", row.names = F)

########### Indikator 9: Komplikasjoner etter kirurgi. ############
# Inkluderer kun forløp der "c_infection", "c_delayed_wound_healing", "c_stricturer", "c_nervedamage", "c_bloodpoisoning",
#   "c_bleeding" eller "c_other_complications" har blitt krysset av for Ja eller Nei.
#################################################################
# c("c_infection", "c_delayed_wound_healing", "c_stricturer", "c_nervedamage", "c_bloodpoisoning",
#   "c_bleeding", "c_other_complications")

Indikator9 <- RegData[(RegData$c_infection %in% c(1,2) | RegData$c_delayed_wound_healing %in% c(1,2) |
                        RegData$c_stricturer %in% c(1,2) | RegData$c_nervedamage %in% c(1,2) |
                        RegData$c_bloodpoisoning %in% c(1,2) | RegData$c_bleeding %in% c(1,2) |
                        RegData$c_other_complications %in% c(1,2)) & RegData$ForlopsType1Num %in% c(1,3) &
                        RegData$c_do_month == 6 & RegData$OppflgRegStatus >= 1, ] %>%
  group_by(AvdRESH, Aar, m_mceid) %>%
  summarise(Variabel = c_infection==1 | c_delayed_wound_healing==1 | c_stricturer==1 |
              c_nervedamage==1 | c_bloodpoisoning==1 | c_bleeding==1 | c_other_complications==1)

Indikator9$Variabel[is.na(Indikator9$Variabel)] <- FALSE
Indikator9$Variabel <- as.numeric(Indikator9$Variabel)
Indikator9$SykehusNavn <- RegData$SykehusNavn[match(Indikator9$AvdRESH, RegData$AvdRESH)]
Ind9 <- Indikator9[, -c(3,5)]
names(Ind9) <- c( 'ReshId', 'Aar', 'Teller Ind9')
Ind9[, 'Nevner Ind9'] <- 1
Ind9$Indikator <- 'Ind9'
Ind9$AarID <- paste0(Ind9$Aar, Ind9$ReshId)
Ind9 <- Ind9[, c(2,5,4,3,1,6)]

write.csv2(Ind9, "Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/7. HisReg/Indikatorer/indikator9_kompl_kir.csv",
           row.names = F)

tmp <- Indikator9 %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                    andel = sum(Variabel)/n()*100,
                                                    N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind9_kompl_kir <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
write.csv2(Ind9_kompl_kir, "I:/hisreg/Ind9_kompl_kir.csv", row.names = F)

## Versjon 2:
Indikator9_v2 <- fulldata[which((fulldata$c_infection %in% c(1,2) | fulldata$c_delayed_wound_healing %in% c(1,2) |
                             fulldata$c_stricturer %in% c(1,2) | fulldata$c_nervedamage %in% c(1,2) |
                             fulldata$c_bloodpoisoning %in% c(1,2) | fulldata$c_bleeding %in% c(1,2) |
                             fulldata$c_other_complications %in% c(1,2)) & fulldata$ForlopsType1Num %in% c(1,3) &
                            fulldata$c_do_month == 6 & fulldata$OppflgRegStatus >= 1), ] %>%
  group_by(AvdRESH, Aar, m_mceid) %>%
  summarise(Variabel = c_infection==1 | c_delayed_wound_healing==1 | c_stricturer==1 |
              c_nervedamage==1 | c_bloodpoisoning==1 | c_bleeding==1 | c_other_complications==1)

Indikator9_v2$Variabel[is.na(Indikator9_v2$Variabel)] <- FALSE
Indikator9_v2$Variabel <- as.numeric(Indikator9_v2$Variabel)
Indikator9_v2$SykehusNavn <- RegData$SykehusNavn[match(Indikator9_v2$AvdRESH, RegData$AvdRESH)]
tmp <- Indikator9_v2 %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                               andel = sum(Variabel)/n()*100,
                                                               N=n())
tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind9_kompl_kir <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')

########### Indikator 10: Andel pasienter med bivirkninger rapportert av pasienter ved ################
########### kontroll 3 måneder etter startet medisinsk behandling                   ################
# Inkluderer kun forløp der  "c_medical_treatment",
# "c_systemic_antibiotic_therapy_stop", "c_antiinflammatory_treatment_stop", "c_analgesics_stop",
# "c_corticosteroid_injection_stop","c_acelacic_acid_stop", "c_clindamycin_stop",
# "c_resorcinol_stop" eller "c_other_medication_stop" har blitt krysset av for Ja eller Nei.


indikator10 <- RegData[which(RegData$ForlopsType1Num %in% c(2,3)),
                      c("Aar", "SykehusNavn", "AvdRESH", "m_mceid", "c_do_month", "HovedDato", "c_date", "c_medical_treatment",
                        "c_systemic_antibiotic_therapy_stop", "c_antiinflammatory_treatment_stop", "c_analgesics_stop",
                        "c_corticosteroid_injection_stop","c_acelacic_acid_stop", "c_clindamycin_stop",
                        "c_resorcinol_stop", "c_other_medication_stop")]

indikator10 <- indikator10[indikator10$c_medical_treatment %in% 1:2 | indikator10$c_systemic_antibiotic_therapy_stop %in% 1:2 |
                             indikator10$c_antiinflammatory_treatment_stop %in% 1:2 | indikator10$c_acelacic_acid_stop %in% 1:2 |
                             indikator10$c_clindamycin_stop %in% 1:2 | indikator10$c_resorcinol_stop %in% 1:2 |
                             indikator10$c_other_medication_stop %in% 1:2, ] %>%
  group_by(AvdRESH, Aar, m_mceid) %>%
  summarise(Variabel = c_medical_treatment==1 | c_systemic_antibiotic_therapy_stop==1 | c_antiinflammatory_treatment_stop==1 |
              c_analgesics_stop==1 | c_corticosteroid_injection_stop==1 | c_acelacic_acid_stop==1 | c_clindamycin_stop==1 |
              c_resorcinol_stop==1 | c_other_medication_stop==1)

indikator10$Variabel[is.na(indikator10$Variabel)] <- FALSE
indikator10$Variabel <- as.numeric(indikator10$Variabel)
indikator10$SykehusNavn <- RegData$SykehusNavn[match(indikator10$AvdRESH, RegData$AvdRESH)]
Ind10 <- indikator10[, -c(3,5)]
names(Ind10) <- c( 'ReshId', 'Aar', 'Teller Ind10')
Ind10[, 'Nevner Ind10'] <- 1
Ind10$Indikator <- 'Ind10'
Ind10$AarID <- paste0(Ind10$Aar, Ind10$ReshId)
Ind10 <- Ind10[, c(2,5,4,3,1,6)]

write.csv2(Ind10, "Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/7. HisReg/Indikatorer/indikator10_bivirkn_med.csv",
           row.names = F)


tmp <- indikator10 %>% group_by(SykehusNavn, Aar) %>% summarise(antall = sum(Variabel),
                                                                andel = sum(Variabel)/n()*100,
                                                                N=n())

tmp$verdi <- paste0(round(tmp$andel, 1), '% (', tmp$N, ')')
Ind10_bivirkn_med <- tmp[, -(3:5)] %>% spread(key=Aar, value = verdi, fill = '')
write.csv2(Ind10_bivirkn_med, "I:/hisreg/Ind10_bivirkn_med.csv", row.names = F)


#################################################################################
###########################  Enhetsliste  #######################################
#################################################################################

enhetsliste <- fulldata[match(unique(fulldata$AvdRESH), fulldata$AvdRESH), c("AvdRESH", "SykehusNavn")]
write.csv2(enhetsliste, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/7. HisReg/enhetsliste.csv', row.names = F)


#################################################################################
###########################  Nøkkeltall  #######################################
#################################################################################

fulldata$OppflgAar <- as.numeric(format(as.Date(fulldata$c_date), format="%Y"))


table(fulldata$Aar, fulldata$OppflgAar, useNA = 'ifany')
table(fulldata$c_possible)
table(fulldata$Intervensjon, useNA = 'ifany')
table(fulldata$i_type, useNA = 'ifany')

tmp<- RegData %>% group_by(Aar, PasientID) %>%
  summarise("smoking" = min(pre_smoking, na.rm = T),
            "ErMann" = ErMann[1]) %>% group_by(Aar) %>%
  summarise("Andel røykere" = sum(smoking==1)/n(),
            "Andel kvinner" = sum(ErMann==0)/n(),
            N=n())

tmp2 <- RegData %>% group_by(Aar) %>%
  summarise("Kirugiske behandlinger" = sum(i_type==1),
            "Medisinske behandlinger" = sum(i_type==2),
            "Kirugiske- og medisinske behandlinger" = sum(i_type==3),
            "Antall avdelinger" = length(unique(SykehusNavn)))
nokkeltall <- merge(tmp, tmp2, by="Aar")
write.csv2(nokkeltall, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/7. HisReg/nokkeltall.csv', row.names = F)


# t(nokkeltall)

# CONTROL_STOP_MEDICAL_TREATMENT
# CONTROL_SYSTEMIC_ANTIBIOTIC_THERAPY_STOP
# CONTROL_ANTIINFLAMMATORY_TREATMENT_STOP
# CONTROL_ANALGESICS_STOP
# CONTROL_INTRALESIONAL_CORTICOSTEROID_INJECTION_STOP
# CONTROL_ACELACIC_ACID_STOP
# CONTROL_CLINDAMYCIN_STOP
# CONTROL_RESORCINOL_STOP
# CONTROL_MEDICAL_INT_OTHER_STOP
