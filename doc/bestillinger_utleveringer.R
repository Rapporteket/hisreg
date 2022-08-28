library(tidyverse)
library(hisreg)
rm(list = ls())

############ Dekningsgradsdata til NPR 29.08.2022 ################################

hisregdata <- hisreg::lastShinyHisreg()
RegData <- hisregdata$RegData

nprdata <- RegData %>% filter((HovedDato >= "2021-01-01" & HovedDato < "2022-01-01") |
                                (CONTROL_DATE_dokt_med >= "2021-01-01" & CONTROL_DATE_dokt_med < "2022-01-01") |
                                (CONTROL_DATE_dokt_kir >= "2021-01-01" & CONTROL_DATE_dokt_kir < "2022-01-01")) %>%
  select(HovedDato, ForlopsType1, ForlopsType1Num, PasientID, AvdRESH,
         MCEID, CONTROL_DATE_dokt_med, CONTROL_DATE_dokt_kir, c_date)


koblingsdata <- read.table('~/.ssh/hisreg/koblingsdata_npr17082022.csv',
                           header=TRUE, sep=";", encoding = 'UTF-8',
                           colClasses = c('integer', 'character'))
koblingsdata_npr <- koblingsdata[koblingsdata$PID %in% unique(nprdata$pid), ]
write.csv2(koblingsdata_npr, 'I:/hisreg/koblingsdata_npr30102020.csv', row.names = F)

############ Dekningsgradsdata til NPR 27.10.2020 ################################

hisregdata <- read.table('I:/hisreg/AlleVarNum2020-08-21 13-19-34.txt', header=TRUE, sep=";", encoding = 'UTF-8')
hisregdata$i_date <- as.Date(hisregdata$i_date)
hisregdata$aar <- format(hisregdata$i_date, '%Y')
hisregdata$pid <- hisregdata$m_pid
ForlopsData <- read.table('I:/hisreg/ForlopsOversikt2020-08-21 13-19-34.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopsData <- ForlopsData[, c('ForlopsID', 'AvdRESH', 'BasisRegStatus', 'HovedDato', 'OppflgRegStatus', 'SykehusNavn')]
hisregdata <- merge(hisregdata, ForlopsData, by.x = 'm_mceid', by.y = 'ForlopsID')
Followups <- read.table('I:/hisreg/FollowupsNum2020-08-21 13-19-34.txt', header=TRUE, sep=";", encoding = 'UTF-8')
Followups <- Followups[Followups$c_possible != 2, ] # Fjerner Oppfølging ikke mulig
Followups <- Followups[!is.na(Followups$c_mceid), ]
Followups$c_date <- as.Date(Followups$c_date)

tmp <- merge(Followups[, c("c_mceid", "c_centreid", "c_date", "c_do_month")],
             hisregdata[, c("m_mceid", "i_date", "pid", "SykehusNavn", "AvdRESH")],
             by.x = "c_mceid", by.y = "m_mceid")
tmp$aar <- format(tmp$c_date, '%Y')

samlet_hisreg <- bind_rows(hisregdata[, c("aar", "pid", "AvdRESH")], tmp[, c("aar", "pid", "AvdRESH")])

nprdata <- samlet_hisreg %>% group_by(aar, AvdRESH, pid) %>% summarise(antall = n())

nprdata <- nprdata[nprdata$AvdRESH != 999002, ]
nprdata <- nprdata[nprdata$aar %in% 2016:2019, -4]

write.csv2(nprdata, 'I:/hisreg/nprdata_hisreg30102020.csv', row.names = F)

# table(nprdata$AvdRESH, nprdata$aar, useNA = 'ifany')

koblingsdata <- read.table('I:/hisreg/PatientList_hisreg_2020-10-29.csv', header=TRUE, sep=",", encoding = 'UTF-8',
                           colClasses = c('integer', 'character'))
koblingsdata_npr <- koblingsdata[koblingsdata$PID %in% unique(nprdata$pid), ]
write.csv2(koblingsdata_npr, 'I:/hisreg/koblingsdata_npr30102020.csv', row.names = F)



############ Dekningsgradsdata til NPR 10.10.2019 ################################

hisregdata <- read.table('I:/hisreg/AlleVarNum2019-10-10 10-15-40.txt', header=TRUE, sep=";", encoding = 'UTF-8')
hisregdata$i_date <- as.Date(hisregdata$i_date)
hisregdata$aar <- format(hisregdata$i_date, '%Y')
hisregdata$pid <- hisregdata$m_pid
ForlopsData <- read.table('I:/hisreg/ForlopsOversikt2019-10-10 10-15-43.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopsData <- ForlopsData[, c('ForlopsID', 'AvdRESH', 'BasisRegStatus', 'HovedDato', 'OppflgRegStatus', 'SykehusNavn')]
hisregdata <- merge(hisregdata, ForlopsData, by.x = 'm_mceid', by.y = 'ForlopsID')
Followups <- read.table('I:/hisreg/FollowupsNum2019-10-10 10-15-42.txt', header=TRUE, sep=";", encoding = 'UTF-8')
Followups <- Followups[Followups$c_possible != 2, ] # Fjerner Oppfølging ikke mulig
Followups <- Followups[!is.na(Followups$c_mceid), ]
Followups$c_date <- as.Date(Followups$c_date)

tmp <- merge(Followups[, c("c_mceid", "c_centreid", "c_date", "c_do_month")], hisregdata[, c("m_mceid", "i_date", "pid", "SykehusNavn", "AvdRESH")],
             by.x = "c_mceid", by.y = "m_mceid")
tmp$aar <- format(tmp$c_date, '%Y')

samlet_hisreg <- bind_rows(hisregdata[, c("aar", "pid", "AvdRESH")], tmp[, c("aar", "pid", "AvdRESH")])

nprdata <- samlet_hisreg %>% group_by(aar, AvdRESH, pid) %>% summarise(antall = n())

nprdata <- nprdata[nprdata$AvdRESH != 999002, ]
nprdata <- nprdata[nprdata$aar %in% 2016:2018, -4]

write.csv2(nprdata, 'I:/hisreg/nprdata_hisreg.csv', row.names = F)

# table(nprdata$AvdRESH, nprdata$aar, useNA = 'ifany')

koblingsdata <- read.table('I:/hisreg/PatientListHisreg-2019-10-10.csv', header=TRUE, sep=",", encoding = 'UTF-8',
                           colClasses = c('integer', 'character'))
koblingsdata_npr <- koblingsdata[koblingsdata$PID %in% unique(nprdata$pid), ]
write.csv2(koblingsdata_npr, 'I:/hisreg/koblingsdata_npr.csv', row.names = F)

############  L73.2 diagnoser UNN ? ##############################

tmp2016 <- xlsx::read.xlsx('I:/hisreg/HD L732 - 2016 - 2018.xlsx', sheetIndex = 1, encoding = 'UTF-8', stringsAsFactors = F)
tmp2016$aar <- 2016
tmp2017 <- xlsx::read.xlsx('I:/hisreg/HD L732 - 2016 - 2018.xlsx', sheetIndex = 2, encoding = 'UTF-8', stringsAsFactors = F)
tmp2017$aar <- 2017
tmp2018 <- xlsx::read.xlsx('I:/hisreg/HD L732 - 2016 - 2018.xlsx', sheetIndex = 3, encoding = 'UTF-8', stringsAsFactors = F)
tmp2018$aar <- 2018
hidra <- bind_rows(tmp2016,tmp2017,tmp2018)

hisreg_avd <- c('DMS-HA', 'FELPOL-NA', 'ååHUDPOLNA', 'HUDPOL-BA', 'HUDPOL-SL', 'HUDPOL-TR', 'NHRTR', 'NHRTRH')
tos_avd <- c('HUDPOL-TR', 'NHRTR', 'NHRTRH')

ikke_hisreg_avd <- unique(hidra$Lokalisering)
ikke_hisreg_avd <- ikke_hisreg_avd[!(ikke_hisreg_avd %in% hisreg_avd)]

pr_pas_pr_aar <- hidra %>% group_by(aar, PID.NPR.nr) %>% summarise(hisreg_avd = length(intersect(hisreg_avd, Lokalisering))!=0,
                                                                   andre = length(intersect(ikke_hisreg_avd, Lokalisering))!=0,
                                                                   N=n()) %>% ungroup()

pr_pas_pr_aar <- hidra %>% group_by(aar, PID.NPR.nr) %>% summarise(hisreg_avd = sum(Lokalisering %in% hisreg_avd),
                                                                   andre = sum(Lokalisering %in% ikke_hisreg_avd),
                                                                   N=n()) %>% ungroup()
unik_pas_pr_avd_pr_aar <- hidra %>% group_by(aar, Lokalisering) %>% summarise(antall_unik = length(unique(PID.NPR.nr)))
unik_pas_pr_avd_pr_aar <- spread(unik_pas_pr_avd_pr_aar,key = aar, value = antall_unik)
write.csv2(unik_pas_pr_avd_pr_aar, 'C:/GIT/hisreg/doc/unik_pas_pr_avd_pr_aar.csv', row.names = F, na = '0')

aktuell_for_hisreg <- pr_pas_pr_aar[pr_pas_pr_aar$hisreg_avd != 0, 1:2]
aktuell_for_hisreg_unik <- aktuell_for_hisreg[match(unique(aktuell_for_hisreg$PID.NPR.nr), aktuell_for_hisreg$PID.NPR.nr), 2]
kun_andre_avdelinger <- pr_pas_pr_aar[pr_pas_pr_aar$hisreg_avd == 0, 1:2]
kun_andre_avdelinger_unik <- kun_andre_avdelinger[match(unique(kun_andre_avdelinger$PID.NPR.nr), kun_andre_avdelinger$PID.NPR.nr), 2]


write.csv2(aktuell_for_hisreg, 'C:/GIT/hisreg/doc/aktuell_for_hisreg.csv', row.names = F)
write.csv2(aktuell_for_hisreg_unik, 'C:/GIT/hisreg/doc/aktuell_for_hisreg_unik.csv', row.names = F)
write.csv2(kun_andre_avdelinger, 'C:/GIT/hisreg/doc/kun_andre_avdelinger.csv', row.names = F)
write.csv2(pr_pas_pr_aar, 'C:/GIT/hisreg/doc/alle.csv', row.names = F)

###### Data fra Hisreg ##############################
hisregdata1 <- read.table('I:/hisreg/AlleVarNum2019-05-24 15-00-58.txt', header=TRUE, sep=";", encoding = 'UTF-8')
hisregdata2 <- read.table('I:/hisreg/AlleVar2019-05-24 15-00-55.txt', header=TRUE, sep=";", encoding = 'UTF-8')
hisregdata1$i_date <- as.Date(hisregdata1$i_date)
hisregdata1$aar <- format(hisregdata1$i_date, '%Y')
hisregdata1$pid <- hisregdata1$m_pid
ForlopsData <- read.table('I:/hisreg/ForlopsOversikt2019-05-24 15-01-02.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopsData <- ForlopsData[, c('ForlopsID', 'AvdRESH', 'BasisRegStatus', 'HovedDato', 'OppflgRegStatus', 'SykehusNavn')]
hisregdata1 <- merge(hisregdata1, ForlopsData, by.x = 'm_mceid', by.y = 'ForlopsID')
Followups <- read.table('I:/hisreg/FollowupsNum2019-05-24 15-01-02.txt', header=TRUE, sep=";", encoding = 'UTF-8')
Followups2 <- read.table('I:/hisreg/Followups2019-05-24 15-01-01.txt', header=TRUE, sep=";", encoding = 'UTF-8')
Followups <- Followups[Followups$c_possible != 2, ] # Fjerner Oppfølging ikke mulig
Followups <- Followups[!is.na(Followups$c_mceid), ]
Followups$c_date <- as.Date(Followups$c_date)
hisregdata2 <- hisregdata1
hisregdata1 <- hisregdata1[hisregdata1$i_centreid == 601031, ]
# Followups <- Followups[Followups$c_centreid]

tmp <- merge(Followups[, c("c_mceid", "c_centreid", "c_date", "c_do_month")], hisregdata1[, c("m_mceid", "i_date", "m_pid", "SykehusNavn", "AvdRESH")], by.x = "c_mceid", by.y = "m_mceid")
tmp$aar <- format(tmp$c_date, '%Y')
tmp$pid <- tmp$m_pid
tmp <- tmp[tmp$c_centreid == 601031, ]
tmp <- tmp[which(tmp$aar %in% 2016:2018), ]
hisregdata1 <- hisregdata1[which(hisregdata1$aar %in% 2016:2018), ]

samlet_hisreg <- bind_rows(hisregdata1[, c("aar", "pid")], tmp[, c("aar", "pid")])

unike_pas_hisreg <- samlet_hisreg %>% group_by(aar) %>% summarise(antall_hisreg = length(unique(pid)))

dg_tabell <- unike_pas_hisreg
dg_tabell$antall_npr <- as.numeric(table(aktuell_for_hisreg$aar, useNA = 'ifany'))
dg_tabell$dg <- dg_tabell$antall_hisreg/dg_tabell$antall_npr*100


tmp <- merge(Followups[, c("c_mceid", "c_centreid", "c_date", "c_do_month")], hisregdata2[, c("m_mceid", "i_date", "m_pid", "SykehusNavn", "AvdRESH")],
             by.x = "c_mceid", by.y = "m_mceid")
tmp$aar <- format(tmp$c_date, '%Y')
tmp$pid <- tmp$m_pid
tmp <- tmp[which(tmp$aar %in% 2016:2018), ]
hisregdata1 <- hisregdata1[which(hisregdata1$aar %in% 2016:2018), ]

samlet_hisreg <- bind_rows(hisregdata1[, c("aar", "pid", "SykehusNavn", "AvdRESH")], tmp[, c("aar", "pid", "SykehusNavn", "AvdRESH")])
unike_pas_pr_aar_pr_avd_hisreg <- samlet_hisreg %>% group_by(aar, SykehusNavn) %>% summarise(AvdRESH = AvdRESH[1],
                                                                                             antall_hisreg = length(unique(pid)))
unike_pas_pr_aar_pr_avd_hisreg <- unike_pas_pr_aar_pr_avd_hisreg[unike_pas_pr_aar_pr_avd_hisreg$AvdRESH != 999002, -3]


# skjemaer <- read.table('I:/hisreg/SkjemaOversikt2019-10-02 15-33-58.txt', header=TRUE, sep=";", encoding = 'UTF-8')
# skjemaer$HovedDato <- as.Date(skjemaer$HovedDato)
# skjemaer$aar <- format(skjemaer$HovedDato, '%Y')
# skjemaer <- skjemaer[order(skjemaer$HovedDato, decreasing = F), ]
# skjemaer <- skjemaer[match(skjemaer$)]
#
# # table(skjemaer$SkjemaStatus, useNA = 'ifany')
# aux <- skjemaer[skjemaer$SkjemaStatus==1 & skjemaer$Skjemanavn=='Preintervensjon', ] %>% group_by(aar, Sykehusnavn) %>% summarise(antall = length(unique(ForlopsID))) %>% spread(key = aar, value = antall)


# Til Kjersti
dg_tabell
length(unique(samlet_hisreg$pid))/length(unique(aktuell_for_hisreg$PID.NPR.nr))*100


# rm(list = ls())

tmp2016 <- xlsx::read.xlsx('I:/hisreg/BD L732 2016 - 2018.xlsx', sheetIndex = 1, encoding = 'UTF-8', stringsAsFactors = F)
tmp2016$aar <- 2016
tmp2017 <- xlsx::read.xlsx('I:/hisreg/BD L732 2016 - 2018.xlsx', sheetIndex = 2, encoding = 'UTF-8', stringsAsFactors = F)
tmp2017$aar <- 2017
tmp2018 <- xlsx::read.xlsx('I:/hisreg/BD L732 2016 - 2018.xlsx', sheetIndex = 3, encoding = 'UTF-8', stringsAsFactors = F)
tmp2018$aar <- 2018
hidra_bd <- bind_rows(tmp2016,tmp2017,tmp2018)

kun_bidiagnose <- hidra_bd[hidra_bd$Hovedtilstandskoder != 'L732', ]

kun_bidiagnose <- hidra_bd %>% group_by(aar, PID.NPR.nr) %>% summarise(kun_bi = !('L732' %in% Hovedtilstandskoder), N=n())
1 <- kun_bidiagnose[kun_bidiagnose$kun_bi, ]

kun_bidiagnose %>% group_by(aar) %>% summarise(N=n())

############ Utlevering Roskilde  28.06.2019   ####################################################

AlleVarNum <- read.table('I:/hisreg/AlleVarNum2019-05-24 15-00-58.txt', header=TRUE, sep=";", encoding = 'UTF-8')
Followups <- read.table('I:/hisreg/FollowupsNum2019-05-24 15-01-02.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopsOversikt <- read.table('I:/hisreg/ForlopsOversikt2019-05-24 15-01-02.txt', header=TRUE, sep=";", encoding = 'UTF-8')

AlleVarNum <- AlleVarNum[which(AlleVarNum$i_centreid == 999002), ]
# Followups <- Followups[which(Followups$c_mceid %in% AlleVarNum$m_mceid), ]
Followups <- Followups[which(Followups$c_centreid == 999002), ]
ForlopsOversikt <- ForlopsOversikt[which(ForlopsOversikt$AvdRESH == 999002), ]


write.csv2(AlleVarNum, 'I:/hisreg/AlleVarNum2019-05-24 15-00-58 Roskilde.csv', row.names = F)
write.csv2(Followups, 'I:/hisreg/Followups2019-05-24 15-00-58 Roskilde.csv', row.names = F)
write.csv2(ForlopsOversikt, 'I:/hisreg/ForlopsOversikt2019-05-24 15-00-58 Roskilde.csv', row.names = F)
