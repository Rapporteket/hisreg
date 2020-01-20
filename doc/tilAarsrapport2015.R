setwd('C:/GIT/hisreg/doc')
library(hisreg)
rm(list=ls())

# Les inn data
RegData <- read.table('C:/SVN/jasper/hisreg/data/AlleVarNum2016-12-13 10-12-42.txt', header=TRUE, sep=";", encoding = 'UFT-8')
RegData <- RegData[, c('m_mceid', 'p_age_abscess', 'p_education', 'p_surgery', 'p_antibiotics', 'pre_smoking', 'pre_work', 'pre_dlqisum',
                       'pre_hsscoresum', 'pre_vasscore', 'pre_hurley_score', 'i_type', 'i_surgery_type', 'i_biological_treatment',
                       'i_antibiotic_therapy', 'i_antiinflammatory_treatment', 'i_analgesics', 'i_localized_med_treatment', 'i_aksille',
                       'i_lyske', 'i_pubis', 'i_genitalt', 'i_peritalt', 'i_glutealt', 'i_mammae', 'i_other_location')]
RegData_label <- read.table('C:/SVN/jasper/hisreg/data/AlleVar2016-12-13 10-12-42.txt', header=TRUE, sep=";", encoding = 'UFT-8')
RegData_label <- RegData_label[, c('m_mceid', 'i_type')]
Followups <- read.table('C:/SVN/jasper/hisreg/data/FollowupsNum2016-12-13 10-12-44.txt', header=TRUE, sep=";", encoding = 'UFT-8')
Followups <- Followups[, c('c_mceid', 'c_do_month', 'c_infection', 'c_delayed_wound_healing', 'c_stricturer', 'c_nervedamage', 'c_bloodpoisoning',
                           'c_bleeding', 'c_other_complications', 'c_dlqisum', 'c_hsscoresum', 'c_vasscore', 'c_hurley_score')]
Followups_label <- read.table('C:/SVN/jasper/hisreg/data/Followups2016-12-13 10-12-43.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopsData <- read.table('C:/SVN/jasper/hisreg/data/ForlopsOversikt2016-12-13 10-12-44.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopsData <- ForlopsData[, c('ForlopsID', 'BasisRegStatus', 'HovedDato', 'PasientAlder', 'OppflgRegStatus', 'ForlopsType1', 'ForlopsType1Num',
                               'ForlopsType2', 'ForlopsType2Num', 'ErMann', 'SykehusNavn', 'PasientID')]

########## Må avklare hvordan oppfølginger skal joines i framtida. For dataen til Årsrapport 2015 finnes maks én oppfølging pr forløp.
########## Jeg velger derfor å bruke en enkel LEFT JOIN og skiller ikke mellom 3 og 6 mnd oppfølging.

RegData <- merge(RegData, ForlopsData, by.x = 'm_mceid', by.y = 'ForlopsID')
RegData <- merge(RegData, RegData_label, by.x = 'm_mceid', by.y = 'm_mceid', suffixes = c('', '_label'))
RegData <- merge(RegData, Followups, by.x = 'm_mceid', by.y = 'c_mceid', all.x=T)

# RegDataMedOppf <- merge(RegData, Followups[Followups$c_do_month==3, ], by.x = 'm_mceid', by.y = 'c_mceid', all.x=T)
# RegDataMedOppf <- merge(RegDataMedOppf, Followups[Followups$c_do_month==6, ], by.x = 'm_mceid', by.y = 'c_mceid', all.x=T, suffixes = c('', '_6m'))


RegData <- hisreg::hisregPreprosess(RegData)
valgtVar <- 'DLQI_PrePost'
datoFra='2015-01-01'
datoTil='2015-12-31'
reshID <- 102449
minald=0
maxald=120
erMann=99
outfile= ''  #'DLQI_PrePost.pdf'
forlop1 = 99
forlop2 = 99
enhetsUtvalg=0
preprosess=F
hentData=F
gr_var <- 'Intervensjon'
fjern_sjeldne = 1

if (outfile=='') {x11()}
hisregFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID,
                 minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop1 = forlop1, forlop2 = forlop2,
                 enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData)

if (outfile=='') {x11()}
hisregFigAndelerPrePost(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID,
                        minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop1 = forlop1, forlop2 = forlop2,
                        enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData)

if (outfile=='') {x11()}
hisregFigGjsnPrePostGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID,
                         minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop1 = forlop1, forlop2 = forlop2,
                         enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData, gr_var = gr_var)

if (outfile=='') {x11()}
hisregFigGjsnGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID,
                      minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop1 = forlop1, forlop2 = forlop2,
                      enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData, gr_var = gr_var, fjern_sjeldne = fjern_sjeldne)





# write.csv2(RegDataMedOppf, 'seDataNy.csv', row.names = F)












# names(RegData)
# names(ForlopsData)
# names(Followups)
#
# table(ForlopsData$OppflgStatus, useNA = 'ifany')
# table(ForlopsData$ForlopsType1, ForlopsData$OppflgStatus, useNA = 'ifany')
#
# table(ForlopsData$ForlopsType1[which(ForlopsData$ForlopsID %in% Followups$c_mceid)],
#       ForlopsData$OppflgStatus[which(ForlopsData$ForlopsID %in% Followups$c_mceid)], useNA = 'ifany')
# table(ForlopsData$OppflgStatus[which(ForlopsData$ForlopsID %in% Followups$c_mceid)], useNA = 'ifany')
#
# which(ForlopsData$ForlopsID %in% Followups$c_mceid)
#
# table(Followups$c_do_month, useNA = 'ifany')
# table(Followups$c_do_month[Followups$c_mceid %in% setdiff(Followups$c_mceid, ForlopsData$ForlopsID)], useNA = 'ifany')
