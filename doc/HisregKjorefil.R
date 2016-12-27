rm(list=ls())
library(hisreg)

# Les inn data
RegData <- read.table('C:/SVN/jasper/hisreg/data/AlleVarNum2016-09-15 10-46-23.txt', header=TRUE, sep=";", encoding = 'UFT-8')
RegData <- RegData[, c('m_mceid', 'p_age_abscess', 'p_education', 'p_surgery', 'p_antibiotics', 'pre_smoking', 'pre_work', 'pre_bmi', 'pre_dlqisum',
                       'pre_hsscoresum', 'pre_vasscore', 'pre_hurley_score', 'i_type', 'i_surgery_type', 'i_biological_treatment',
                       'i_antibiotic_therapy', 'i_antiinflammatory_treatment', 'i_analgesics', 'i_localized_med_treatment', 'i_aksille',
                       'i_lyske', 'i_pubis', 'i_genitalt', 'i_peritalt', 'i_glutealt', 'i_mammae', 'i_other_location')]
Followups <- read.table('C:/SVN/jasper/hisreg/data/FollowupsNum2016-09-15 10-46-24.txt', header=TRUE, sep=";", encoding = 'UFT-8')
Followups <- Followups[, c('c_mceid', 'c_do_month', 'c_infection', 'c_delayed_wound_healing', 'c_stricturer', 'c_nervedamage', 'c_bloodpoisoning',
                           'c_bleeding', 'c_other_complications', 'c_dlqisum', 'c_hsscoresum', 'c_vasscore', 'c_hurley_score')]
ForlopsData <- read.table('C:/SVN/jasper/hisreg/data/ForlopsOversikt2016-09-15 10-46-24.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopsData <- ForlopsData[, c('ForlopsID', 'AvdRESH', 'BasisRegStatus', 'HovedDato', 'PasientAlder', 'OppflgRegStatus', 'ForlopsType1', 'ForlopsType1Num',
                               'ForlopsType2', 'ForlopsType2Num', 'ErMann', 'SykehusNavn', 'PasientID')]

RegData <- merge(RegData, ForlopsData, by.x = 'm_mceid', by.y = 'ForlopsID')
RegData <- merge(RegData, Followups, by.x = 'm_mceid', by.y = 'c_mceid', all.x=T)
RegData <- hisregPreprosess(RegData)
# RegData <- RegData[which(RegData$Aar == 2015), ]

datoFra='2012-01-01'
datoTil='2015-12-31'
reshID <- 601031
minald=0
maxald=120
erMann=99
forlop1 = 99
forlop2 = 99
enhetsUtvalg=0
preprosess=F
hentData=F
outfile <- ''
valgtVar <- 'pre_hurley_score'

if (outfile == ''){x11()}
hisregFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID,
                 minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop1 = forlop1, forlop2 = forlop2,
                 enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData)




















setwd('C:/GIT/hisreg/')
rm(list=ls())

# Les inn data
RegData <- read.table('C:/SVN/jasper/hisreg/data/AlleVarNum2016-08-15 12-15-26.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('C:/SVN/jasper/hisreg/data/ForlopsOversikt2016-08-15 12-15-27.txt', header=TRUE, sep=";", encoding = 'UFT-8')


RegData <- merge(RegData, ForlopData, by.x = "m_mceid", by.y = "ForlopsID")

valgtVar <- 'TidlBeh'
datoFra='2000-01-01'
datoTil='2050-01-01'
reshID <- 601031
minald=0
maxald=120
erMann=99
outfile=''  #'DLQI_PrePost.pdf'
forlop1 = 99
forlop2 = 99
enhetsUtvalg=0
preprosess=T
hentData=F

if (outfile=='') {x11()}
hisregFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID,
                 minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop1 = forlop1, forlop2 = forlop2,
                 enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData)

if (outfile=='') {x11()}
hisregFigAndelerPrePost(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID,
                        minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop1 = forlop1, forlop2 = forlop2,
                        enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData)

if (outfile=='') {x11()}
hisregFigGjsnPrePostShus(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID,
                         minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop1 = forlop1, forlop2 = forlop2,
                         enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData)


#####################################

RegData <- read.table('C:/SVN/jasper/hisreg/data/AlleVarNum2016-09-13 08-44-59.txt', header=TRUE, sep=";", encoding = 'UFT-8')
RegData_label <- read.table('C:/SVN/jasper/hisreg/data/AlleVar2016-09-13 08-44-59.txt', header=TRUE, sep=";", encoding = 'UFT-8')
Followups <- read.table('C:/SVN/jasper/hisreg/data/FollowupsNum2016-09-13 08-44-59.txt', header=TRUE, sep=";", encoding = 'UFT-8')
Followups_label <- read.table('C:/SVN/jasper/hisreg/data/Followups2016-09-13 08-44-59.txt', header=TRUE, sep=";", encoding = 'UFT-8')

ForlopsData <- read.table('C:/SVN/jasper/hisreg/data/ForlopsOversikt2016-09-13 08-44-58.txt', header=TRUE, sep=";", encoding = 'UFT-8')


