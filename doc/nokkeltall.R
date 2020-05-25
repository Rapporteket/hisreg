rm(list=ls())
library(hisreg)
library(tidyverse)

# Les inn data
RegData <- read.table('I:/hisreg/AlleVarNum2019-05-24 15-00-58.txt', header=TRUE, sep=";", encoding = 'UTF-8')
RegData <- RegData[, c('m_mceid', 'p_age_abscess', 'p_education', 'p_surgery', 'p_antibiotics', 'pre_smoking', 'pre_work', 'pre_bmi', 'pre_dlqisum',
                       'pre_hsscoresum', 'pre_vasscore', 'pre_hurley_score', 'i_type', 'i_surgery_type', 'i_biological_treatment',
                       'i_antibiotic_therapy', 'i_antiinflammatory_treatment', 'i_analgesics', 'i_localized_med_treatment', 'i_aksille',
                       'i_lyske', 'i_pubis', 'i_genitalt', 'i_peritalt', 'i_glutealt', 'i_mammae', 'i_other_location',
                       "i_rifampicin_clindamycin", "i_tetracyclin_lymecyclin", "i_amoxicilin", "i_sys_antibiotic_other",
                       "i_corticosteroid_injection", "i_acelacic_acid", "i_clindamycin", "i_resorcinol",
                       "i_medical_other")]
Followups <- read.table('I:/hisreg/FollowupsNum2019-05-24 15-01-02.txt', header=TRUE, sep=";", encoding = 'UTF-8')
Followups <- Followups[Followups$c_possible != 2, ] # Fjerner Oppfølging ikke mulig
Followups <- Followups[!is.na(Followups$c_mceid), ] # Fjerner oppfølginger uten pasientid
Followups <- Followups[order(Followups$c_do_month, decreasing = FALSE), ]
Followups <- Followups[match(unique(Followups$c_mceid), Followups$c_mceid), ] # Velg første oppfølging
Followups <- Followups[, c('c_mceid', 'c_do_month', 'c_infection', 'c_delayed_wound_healing', 'c_stricturer', 'c_nervedamage', 'c_bloodpoisoning',
                           'c_bleeding', 'c_other_complications', 'c_dlqisum', 'c_hsscoresum', 'c_vasscore', 'c_hurley_score')]

ForlopsData <- read.table('I:/hisreg/ForlopsOversikt2019-05-24 15-01-02.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopsData <- ForlopsData[, c('ForlopsID', 'AvdRESH', 'BasisRegStatus', 'HovedDato', 'PasientAlder', 'OppflgRegStatus', 'ForlopsType1', 'ForlopsType1Num',
                               'ForlopsType2', 'ForlopsType2Num', 'ErMann', 'SykehusNavn', 'PasientID')]

RegData <- merge(RegData, ForlopsData, by.x = 'm_mceid', by.y = 'ForlopsID')
RegData <- merge(RegData, Followups, by.x = 'm_mceid', by.y = 'c_mceid', all.x=T)
RegData <- hisregPreprosess(RegData)

RegData <- RegData[order(RegData$HovedDato, decreasing = F), ]
tmp <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
aux <-tmp %>% group_by(AvdRESH, Aar) %>% summarise('Antall reg' = n())


tmp <-RegData %>% group_by(Aar) %>% summarise('Kirurgisk' = sum(ForlopsType1Num == 1),
                                        'Medisinsk' = sum(ForlopsType1Num == 2),
                                        'Kirurgisk og medisinsk' = sum(ForlopsType1Num == 3),
                                        'Ingen intervention bestemt av lege' = sum(ForlopsType1Num == 4),
                                        'Ingen intervention bestemt av pasient' = sum(ForlopsType1Num == 5),
                                        N=n())


tmp2 <- tmp[-11,1:4] %>% gather(key=behandling, value = Verdi, -Aar)

tmp2$Verdi <- paste0(tmp2$Aar, ', ', tmp2$behandling, ', ', tmp2$Verdi)
tmp2 %>% spread(key = behandling, value = Verdi)
