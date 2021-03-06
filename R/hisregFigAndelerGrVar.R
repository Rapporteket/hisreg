#' Lag søylediagram eller som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et søylediagram som viser andeler av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' @inheritParams hisregFigAndeler
#'
#' @return En figur med andel av ønsket variabel pr. grupperingsvariabel
#'
#' @export

hisregFigAndelerGrVar <- function(RegData, valgtVar, datoFra='2000-01-01', datoTil='2050-01-01', reshID,
                             minald=0, maxald=120, erMann=99, outfile='', forlop1 = 99, forlop2 = 99,
                             enhetsUtvalg=0, preprosess=F, hentData=F, terskel=5)

{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- hisregHentRegData()
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- hisregPreprosess(RegData=RegData)
  }

  # Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$AvdRESH == reshID), ]}

  # Sykehustekst avhengig av bruker og brukervalg
  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'
  } else {
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)])
  }

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  hisregUtvalg <- hisregUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, forlop1 = forlop1, forlop2 = forlop2)
  RegData <- hisregUtvalg$RegData
  utvalgTxt <- hisregUtvalg$utvalgTxt

  if (valgtVar %in% c('TidlBeh', 'MedisinskBeh', 'KomplKir', 'KirurgiLokalisering', 'Antibiotisk',
                      'LokalisertMedisinsk', 'type_kirurgi', 'MedisinskBeh_v2', "BiologiskBeh", "KomplKir_v2",
                      "type_kirurgi_gr", "MedisinskBeh_gr")) {
    flerevar <- 1
  } else {
    flerevar <- 0
  }

  if (flerevar == 1) {
    PlotParams <- hisregPrepVar(RegData=RegData, valgtVar=valgtVar)
    RegData <- PlotParams$RegData
    PlotParams$RegData <- NA

    AndelVar <- PlotParams$AntVar[,-1]/PlotParams$NVar[,-1]*100
    row.names(AndelVar) <- PlotParams$AntVar[,1]
    AndelVar <- t(AndelVar)

    grtxt <- colnames(AndelVar)
    grtxt <- paste0(grtxt, ' (n=', PlotParams$NVar[,2], ')')
    NGr <- PlotParams$NVar[,2]

    stabeltxt <- PlotParams$grtxt
    tittel <- PlotParams$tittel;
    cexgr <- PlotParams$cexgr;

  } else {
    PlotParams <- hisregPrepVar(RegData=RegData, valgtVar=valgtVar)
    RegData <- PlotParams$RegData
    PlotParams$RegData <- NA


    grtxt <- levels(RegData$Gr)
    stabeltxt <- levels(RegData$VariabelGr)
    NVarGr <- ftable(RegData[ , c('VariabelGr','Gr')])	#ftable(list(RegData$Var, RegData$Gr))
    NGr <- colSums(NVarGr)
    AndelVar <- prop.table(NVarGr,2)*100

    grtxt <- paste0(grtxt, ' (n=', NGr, ')')

    tittel <- PlotParams$tittel;
    cexgr <- PlotParams$cexgr;
  }
  AndelVar <- AndelVar[, NGr>=terskel]
  grtxt <- grtxt[NGr>=terskel]

  FigTypUt <- rapFigurer::figtype(outfile=outfile, fargepalett="BlaaHNpms287"
                      , pointsizePDF=12)

  farger <- FigTypUt$farger
  NutvTxt <- length(utvalgTxt)
  xlabel <- "Andel pasienter (%)"

  vmarg <- max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.8)
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))

  pos <- barplot(as.matrix(AndelVar), horiz=TRUE, beside=TRUE, las=1, xlab=xlabel, yaxt = "n", #main=tittel,
                 col=farger[1:length(stabeltxt)], border=NA, font.main=1, ylim=c(0.05,1.5)*length(grtxt)*length(stabeltxt))#, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
  mtext(at=colMeans(pos), text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25)

  legend('top', legend=rev(stabeltxt), bty='n', cex=.6,
         xjust=0.5, fill=farger[length(stabeltxt):1], border=farger[length(stabeltxt):1], ncol=2)
  krymp <- .9
  title(main = tittel, line=1, font.main=1, cex.main=1.3*cexgr)
  mtext(utvalgTxt, side=3, las=1, cex=krymp*cexgr, adj=0, col=FigTypUt$farger[3], line=c(3+0.8*((length(utvalgTxt) -1):0)))

  par('fig'=c(0, 1, 0, 1))

  if ( outfile != '') {dev.off()}

  ###################### UNDER UTVIKLING #########################################
  ###################### UNDER UTVIKLING #########################################
  ###################### UNDER UTVIKLING #########################################
  ###################### UNDER UTVIKLING #########################################


}






