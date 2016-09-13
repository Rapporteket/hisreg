setwd('C:/GIT/hisreg/')
rm(list=ls())

# Les inn data
RegData <- read.table('C:/SVN/jasper/hisreg/data/AlleVarNum2016-08-15 12-15-26.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('C:/SVN/jasper/hisreg/data/ForlopsOversikt2016-08-15 12-15-27.txt', header=TRUE, sep=";", encoding = 'UFT-8')


RegData <- merge(RegData, ForlopData, by.x = "m_mceid", by.y = "ForlopsID")

valgtVar <- 'DLQI_PrePost'
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


