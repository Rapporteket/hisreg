#' Lag søylediagram eller som viser gjennomsnitt av valgt variabel per grupperingsvariabel
#'
#' Denne funksjonen lager et søylediagram som viser andeler av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' @inheritParams hisregFigAndeler
#'
#' @return En figur med andel av ønsket variabel pr. grupperingsvariabel
#'
#' @export

hisregFigGjsnGrVar <- function(RegData, valgtVar, datoFra='2000-01-01', datoTil='2050-01-01', reshID,
                             minald=0, maxald=120, erMann=99, outfile='', forlop1 = 99, forlop2 = 99,
                             preprosess=F, hentData=F, gr_var='SykehusNavn', fjern_sjeldne = 0)

{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- hisregHentRegData()
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- hisregPreprosess(RegData=RegData)
  }

  RegData$Variabel <- RegData[, valgtVar]
  RegData <- RegData[!is.na(RegData$Variabel), ]

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  hisregUtvalg <- hisregUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, forlop1 = forlop1, forlop2 = forlop2)
  RegData <- hisregUtvalg$RegData
  utvalgTxt <- hisregUtvalg$utvalgTxt


  smltxt <- 'alle sykehus'

  RegData[ ,gr_var] <- as.factor(as.character(RegData[ ,gr_var]))
  N <- dim(RegData)[1]
  if(N > 0) {Ngr <- table(RegData[ ,gr_var])}	else {Ngr <- 0}

  Ngrense <- 10		#Minste antall registreringer for at ei gruppe skal bli vist

  Ngrtxt <- paste(', N=', as.character(Ngr), sep='')
  indGrUt <- as.numeric(which(Ngr < Ngrense))
  if (length(indGrUt)==0) { indGrUt <- 0}
  Ngrtxt[indGrUt] <- paste(' (<', Ngrense,')',sep='')

  vt <- switch(valgtVar,
               'pre_bmi' = 'BMI, preintervensjon',
               'pre_dlqisum' = 'DLQI-sum, preintervensjon',
               'pre_vasscore' = 'VAS-score, preintervensjon',
               'pre_hsscoresum' = 'HS-score, preintervensjon'
  )
  xaksetxt <- switch(valgtVar,
                     'pre_bmi' = 'BMI',
                     'pre_dlqisum' = 'DLQI-sum',
                     'pre_vasscore' = 'VAS-score',
                     'pre_hsscoresum' = 'HS-score'
  )

  tittel <- paste0('Gjennomsnittlig ', vt)


  if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
    FigTypUt <- figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    if (dim(RegData)[1]>0) {
      tekst <- paste('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene', sep='')
    } else {tekst <- 'Ingen registrerte data for dette utvalget'}
    title(main=tittel, cex=0.95)	#line=-8,
    text(0.5, 0.6, tekst, cex=1.2)
    #text(0.5, 0.3, , cex=1.2)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    if ( outfile != '') {dev.off()}
  } else {
    dummy0 <- -0.001
    Gjsn <- tapply(RegData$Variabel, RegData[ ,gr_var], mean, na.rm=T)
    SE <- tapply(RegData$Variabel, RegData[ ,gr_var], sd, na.rm=T)/sqrt(Ngr)
    if (fjern_sjeldne == 1) {
      Gjsn <- Gjsn[-indGrUt]
      SE <- SE[-indGrUt]
    } else {
      Gjsn[indGrUt] <- dummy0
      SE[indGrUt] <- 0
    }
    sortInd <- order(Gjsn, decreasing=TRUE)
    Midt <- as.numeric(Gjsn[sortInd])
    KIned <- Gjsn[sortInd] - 2*SE[sortInd]
    KIopp <- Gjsn[sortInd] + 2*SE[sortInd]
    MidtHele <- round(mean(RegData$Variabel),1)
    KIHele <- MidtHele + sd(RegData$Variabel)/sqrt(N)*c(-2,2)

    if (fjern_sjeldne == 1) {
      Ngr <- Ngr[-indGrUt]
      Ngrtxt <- Ngrtxt[-indGrUt]
    }
    GrNavnSort <- paste(names(Ngr)[sortInd], Ngrtxt[sortInd], sep='')
    AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
    AntBars <- length(Ngr)

    #--------------------------FIGUR---------------------------------------------------
    soyletxt <- c(sprintf('%.1f',Midt[1:AntGr]), rep('',length(Ngr)-AntGr))	#	#round(Midt[1:AntGr],1)

#     if (AntBars <=3) {
#     soyletxt <- c(soyletxt, '')         ###### TESTING  #################
#     Midt <- c(Midt, 0)                                                      #
#     Ngr <- c(Ngr, 0)                                                          #
#     GrNavnSort <- c(GrNavnSort, NA)                                         #
#     }
    #########################################################################
    xmax <-  1.1*max(c(Midt, KIned, KIopp))
    cexGrNavn <- 0.8
    cexSoyletxt <- 0.75

    if (outfile=='') {x11(width=3*595, height=3*800)}
    FigTypUt <- figtype(outfile, height=3*800, fargepalett=hisregUtvalg$fargepalett)	#res=96,
    farger <- FigTypUt$farger
    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexGrNavn)*0.7)
    #NB: strwidth oppfører seg ulikt avh. av device...
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    if (AntBars <=4) {
      pos <- barplot(Midt, horiz=T, border=NA, col=farger[3], width = .5, space = 1,
                     xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(Ngr), font.main=1, xlab='', las=1, cex.names=cexGrNavn)
    } else {
      pos <- barplot(Midt, horiz=T, border=NA, col=farger[3],
                     xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(Ngr), font.main=1, xlab='', las=1, cex.names=cexGrNavn)
    }

    indGrUtPlot <- AntGr+(1:length(indGrUt))
    posKI <- pos[1:AntGr]
    ybunn <- 0
    # ytopp <- max(posKI)*1.03	 #min(posKI)
    ytopp <- max(posKI)+.25
    polygon( c(rep(KIHele[1],2), rep(KIHele[2],2)), c(ybunn, ytopp, ytopp, ybunn),
             col=farger[4], border=farger[4])
    lines(x=rep(MidtHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)
    legend("top", xpd=TRUE, xjust=0,  yjust=0, pch=c(NA, 15), pt.cex=2, cex=0.9, #y=ytopp+0.5,
           lwd=c(2,NA), col=c(farger[2], farger[4]),
           legend = c(paste(smltxt, ': ', MidtHele, sep=''), paste('95% konf.int., N=', N,sep='' )),
           bty='o', bg='white', box.col='white', horiz=TRUE)
    if (AntBars <=4) {
      barplot(Midt, horiz=T, border=NA, col=farger[3], xlim=c(0, xmax), add=TRUE, width = .5, space = 1,
              font.main=1, xlab = xaksetxt, las=1) 	#xlim=c(0,ymax), #, cex.names=0.5
    } else {
      barplot(Midt, horiz=T, border=NA, col=farger[3], xlim=c(0, xmax), add=TRUE,
              font.main=1, xlab = xaksetxt, las=1) 	#xlim=c(0,ymax), #, cex.names=0.5
    }

    title(tittel, font.main=1)
    title('med 95% konfidensintervall', line=0.5, font.main=1, cex.main=0.95)
    mtext(at=pos+0.1, GrNavnSort, side=2, las=1, cex=cexGrNavn, adj=1, line=0.25)	#Sykehusnavn
    text(x=max(strwidth(soyletxt, units='user', cex=cexSoyletxt)), y=pos+0.1,
         soyletxt, las=1, cex=cexSoyletxt, adj=1, col=farger[4])

    avst <- 0.8
    utvpos <- 3	#Startlinje for teksten
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    options(warn = -1)	#Unngå melding om KI med lengde 0. Fungerer av en eller annen grunn ikke i pdf.
#     arrows(x0=Midt[-indGrUtPlot]*0.999, y0=posKI, x1=KIopp[-indGrUtPlot], y1=posKI,
#            length=0.5/max(pos), code=2, angle=90, lwd=1.5, col=farger[1])
#     arrows(x0=Midt[-indGrUtPlot]*1.001, y0=posKI, x1=KIned[-indGrUtPlot], y1=posKI,
    #            length=0.5/max(pos), code=2, angle=90, lwd=1.5, col=farger[1])
    if (AntBars <=4) {
      arrows(x0=KIned, y0=posKI, x1=KIopp, y1=posKI,
             length=0.25/max(pos), code=3, angle=90, lwd=1.5, col=farger[1])
    } else {
      arrows(x0=KIned, y0=posKI, x1=KIopp, y1=posKI,
             length=0.5/max(pos), code=3, angle=90, lwd=1.5, col=farger[1])
    }


    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}

  }






  ###################### UNDER UTVIKLING #########################################
  ###################### UNDER UTVIKLING #########################################
  ###################### UNDER UTVIKLING #########################################
  ###################### UNDER UTVIKLING #########################################


}






