#' Søylediagram med gjennomsnitt før og etter intervensjon fordelt på sykehus
#'
#' Funksjon som genererer en figur med som viser endring i en variabels gjennomsnitt før og etter intervensjonen
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams hisregFigAndeler
#' @param gr_var Velg grupperingsvariabel
#'
#' @return Søylediagram med gjennomsnitt før og etter intervensjon fordelt på sykehus
#'
#' @export
#'
hisregFigGjsnPrePostGrVar <- function(RegData, valgtVar, datoFra='2000-01-01', datoTil='2050-01-01', reshID, enhetsUtvalg=0,
                                    minald=0, maxald=120, erMann=99, outfile='', forlop1 = 99, forlop2 = 99,
                                    preprosess=F, hentData=F, gr_var='SykehusNavn')

{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- hisregHentRegData()
  }

  if (preprosess){
    RegData <- hisregPreprosess(RegData=RegData)
  }

  RegData <- RegData[which(RegData$OppflgRegStatus >= 1), ]
  RegData$Gr_var <- RegData[, gr_var]

  # Denne figurtypen krever at oppfølginger finnes
  RegData <- RegData[RegData$OppflgRegStatus >= 1, ]

  # For gr_var='Intervensjon' er det aktuelt med enhetsUtvalg 0 (Hele landet) eller 2 (Egen avdeling)

  if (enhetsUtvalg==2) {
    RegData <- RegData[which(RegData$AvdRESH == reshID), ]
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)])
  }

  # # Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  # if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$AvdRESH == reshID), ]}
  #
  # # Sykehustekst avhengig av bruker og brukervalg
  # if (enhetsUtvalg==0) {
  #   shtxt <- 'Hele landet'
  # } else {
  #   shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)])
  # }

  # Definerer pre -og postvariabler, fjerner registreringer som mangler én eller begge
  PrePostVar <- switch(valgtVar,
                       DLQI_PrePost = c('pre_dlqisum', 'c_dlqisum'),
                       HS_PrePost = c('pre_hsscoresum', 'c_hsscoresum'),
                       Vas_PrePost = c('pre_vasscore', 'c_vasscore'))


  #   PrePostVar <- switch(valgtVar,
  #                        DLQI_PrePost = c('DLQI alvorlighetsgrad', 'før og etter behandling'),
  #                        Hurley_PrePost = c('Hurley score', 'før og etter behandling'))

  RegData$VarPre <- RegData[ ,PrePostVar[1]]
  RegData$VarPost <- RegData[ ,PrePostVar[2]]
  RegData <- RegData[!is.na(RegData$VarPre) & !is.na(RegData$VarPost), ]

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  hisregUtvalg <- hisregUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, forlop1 = forlop1, forlop2 = forlop2)
  RegData <- hisregUtvalg$RegData
  utvalgTxt <- hisregUtvalg$utvalgTxt

  if (enhetsUtvalg==2) {
    utvalgTxt <- c(paste0('Avdeling: ', shtxt), utvalgTxt)
  }

  if (dim(RegData)[1] < 5) {
    ########## Plot feilmelding
    # farger <- FigTypUt$farger
    plot.new()
    # title(tittel)	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9)
    # legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, 'Færre enn 5 registreringer', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {



  PrePost <- aggregate(RegData[, c('VarPre', "VarPost")],
                       by=list(RegData$Gr_var), mean, na.rm = TRUE)
  PrePostSD <- aggregate(RegData[, c('VarPre', "VarPost")],
                       by=list(RegData$Gr_var), sd, na.rm = TRUE)
  # Ngr <- tapply(RegData[, c('VarPre')], RegData$Gr_var, function(x){length(x[!is.na(x)])})
  Ngr <- aggregate(RegData[, c('VarPre')], by=list(RegData$Gr_var), length)
  kategorier <- as.character(Ngr$Group.1)
  Ngr <- as.matrix(t(Ngr[,-1]))
  PlotMatrise <- as.matrix(t(PrePost[,-1]))
  PlotMatrise <- cbind(PlotMatrise, colMeans(RegData[, c('VarPre', "VarPost")]))
  PrePostSD <- as.matrix(t(PrePostSD[,-1]))
  PrePostSD <- cbind(PrePostSD, apply(RegData[, c('VarPre', "VarPost")], 2, sd, na.rm = TRUE))
#   Ngr <- table(as.character(RegData$Gr_var))  ######## Må forsikre at rekkefølgen av sykehus blir lik som i PlotMatrise
  Ngr <- c(Ngr, sum(Ngr, na.rm = TRUE))
  names(Ngr) <- c(kategorier, 'Totalt')
  sammenlign <- 1

  terskel <- 5
#
#   Hvis man vil utelate kategori fra figur pga. for få reg.:

  if (gr_var=='SykehusNavn') {
    utelat <- which(Ngr < terskel)
    if (length(utelat)>0){
      PlotMatrise <- PlotMatrise[,-utelat]
      PrePostSD <- PrePostSD[,-utelat]
      Ngr <- Ngr[-utelat]
    }
  }

  KINed <- PlotMatrise - 1.96*PrePostSD/t(matrix(c(Ngr, Ngr), ncol = 2, nrow = length(Ngr)))
  KIOpp <- PlotMatrise + 1.96*PrePostSD/t(matrix(c(Ngr, Ngr), ncol = 2, nrow = length(Ngr)))

  KINed[ , Ngr < terskel] <- 0
  KIOpp[ , Ngr < terskel] <- 0
  ############## Lag figur  ###############################

  grtxt <- c(names(Ngr)[1:(length(Ngr)-1)], 'Totalt')
#   if (length(utelat>0)){
#     grtxt <- c('Haukeland' ,'SUS', 'St. Olavs', 'UNN', 'Nasjonalt')[-utelat]
#   }
  # grtxt <- c('Haukeland' ,'SUS', 'St. Olavs', 'UNN', 'Nasjonalt')[-utelat]

  tittel <- switch(valgtVar,
                   'DLQI_PrePost' = 'DLQI før og etter intervensjon',
                   'HS_PrePost' = 'HS score sum før og etter intervensjon',
                   'Vas_PrePost' = c('Hvor plaget er pasienten?', 'Visuell analog skala (VAS) fra 0-10')
  )

  tittel <- c(tittel, 'med 95% konfidensintervall')

  ytekst <- 'Gjennomsnittsscore'
#   ytekst <- switch(valgtVar,
#                    'DLQI_PrePost' = 'Gjennomsnittsscore',
#                    'HS_PrePost' = 'Gjennomsnittsscore'
#   )
  cexgr<-0.9
  cexleg <- 0.9	#Størrelse på legendtekst
  retn<-'V'
  txtretn<-1

  FigTypUt <- rapFigurer::figtype(outfile, fargepalett='BlaaOff')
  NutvTxt <- length(utvalgTxt)
  vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

  PlotMatrise[ , Ngr < terskel] <- 0
  grtxt2 <-  paste0('(N=', Ngr, ')')
  grtxt2[Ngr<terskel] <- paste0('(N<', terskel, ')')

  farger <- FigTypUt$farger
  ymax <- max(PlotMatrise, na.rm=T)*1.25

  pos <- barplot(PlotMatrise, beside=TRUE, las=txtretn, ylab=ytekst,
                 col=farger[1:(sammenlign+1)], border='white', ylim=c(0, ymax))
  mtext(at=colMeans(pos), grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
  mtext(at=colMeans(pos), grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)

  title(tittel, line=1, font.main=1)
  #Tekst som angir hvilket utvalg som er gjort
  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

  legend('top', c('Pre', 'Oppfølging')[1:(sammenlign+1)],
           border=c(fargeHoved,NA), col=farger[1:(sammenlign+1)], bty='n', pch=c(15,15), pt.cex=2,
           lwd=3,	lty=NA, ncol=2, cex=cexleg)

  inkl_konf <- 1
  if (inkl_konf == 1){
    arrows(x0=pos, y0=KINed, x1=pos, y1=KIOpp, code=3, angle=90, lwd=1, length=0.03, col=farger[3]) # konfidensintervall
  }


  par('fig'=c(0, 1, 0, 1))
  if ( outfile != '') {dev.off()}

  }


  utData <- list(tittel = tittel, utvalgTxt = utvalgTxt, CIN=KINed, CIO=KIOpp, Andeler = PlotMatrise, Grtxt1 = grtxt, Grtxt2 = Ngr)
  return(invisible(utData))
}
