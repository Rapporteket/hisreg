#' Søylediagram med gjennomsnitt før og etter intervensjon fordelt på sykehus
#'
#' Funksjon som genererer en figur med som viser endring i en variabels gjennomsnitt før og etter intervensjonen
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams hisregFigAndeler
#'
#' @return Søylediagram med gjennomsnitt før og etter intervensjon fordelt på sykehus
#'
#' @export
#'
hisregFigGjsnPrePostShus <- function(RegData, valgtVar, datoFra='2000-01-01', datoTil='2050-01-01', reshID,
                                    minald=0, maxald=120, erMann=99, outfile='', forlop1 = 99, forlop2 = 99,
                                    enhetsUtvalg=1, preprosess=F, hentData=F)

{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- hisregHentRegData()
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- hisregPreprosess(RegData=RegData)
  }

  # Denne figurtypen krever at oppfølginger finnes
  RegData <- RegData[RegData$OppflgRegStatus >= 1, ]

  # Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$AvdRESH == reshID), ]}

  # Sykehustekst avhengig av bruker og brukervalg
  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'
  } else {
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)])
  }

  # Definerer pre -og postvariabler, fjerner registreringer som mangler én eller begge
  PrePostVar <- switch(valgtVar,
                       DLQI_PrePost = c('pre_dlqisum', 'c6_dlqisum'),
                       HS_PrePost = c('pre_hsscoresum', 'c6_hsscoresum'),
                       Vas_PrePost = c('pre_vasscore', 'c6_vasscore'))


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

  PrePost <- aggregate(RegData[, c('VarPre', "VarPost")],
                       by=list(RegData$SykehusNavn), mean, na.rm = TRUE)
  PrePostSD <- aggregate(RegData[, c('VarPre', "VarPost")],
                       by=list(RegData$SykehusNavn), sd, na.rm = TRUE)
  PlotMatrise <- as.matrix(t(PrePost[,-1]))
  PlotMatrise <- cbind(PlotMatrise, colMeans(RegData[, c('VarPre', "VarPost")]))
  PrePostSD <- as.matrix(t(PrePostSD[,-1]))
  PrePostSD <- cbind(PrePostSD, apply(RegData[, c('VarPre', "VarPost")], 2, sd, na.rm = TRUE))
  Ngr <- table(as.character(RegData$SykehusNavn))  ######## Må forsikre at rekkefølgen av sykehus blir lik som i PlotMatrise
  Ngr <- c(Ngr, sum(Ngr))
  sammenlign <- 1

  utelat <- which(Ngr <10)
  if (length(utelat>0)){
    PlotMatrise <- PlotMatrise[,-utelat]
    PrePostSD <- PrePostSD[,-utelat]
    Ngr <- Ngr[-utelat]
  }
  KINed <- PlotMatrise - 1.96*PrePostSD/t(matrix(c(Ngr, Ngr), ncol = 2, nrow = length(Ngr)))
  KIOpp <- PlotMatrise + 1.96*PrePostSD/t(matrix(c(Ngr, Ngr), ncol = 2, nrow = length(Ngr)))
  ############## Lag figur  ###############################

  grtxt <- c(names(Ngr)[1:(length(Ngr)-1)], 'Nasjonalt')
  if (length(utelat>0)){
    grtxt <- c('Haukeland' ,'SUS', 'St. Olavs', 'UNN', 'Nasjonalt')[-utelat]
  }
  # grtxt <- c('Haukeland' ,'SUS', 'St. Olavs', 'UNN', 'Nasjonalt')[-utelat]

  tittel <- switch(valgtVar,
                   'DLQI_PrePost' = 'DLQI før og etter inngrep',
                   'HS_PrePost' = 'HS score sum før og etter inngrep',
                   'Vas_PrePost' = c('Hvor plaget er pasienten?', 'Visuell analog skala (VAS) fra 0-10')
  )

  ytekst <- 'Gjennomsnittsscore'
#   ytekst <- switch(valgtVar,
#                    'DLQI_PrePost' = 'Gjennomsnittsscore',
#                    'HS_PrePost' = 'Gjennomsnittsscore'
#   )
  cexgr<-0.9
  cexleg <- 0.9	#Størrelse på legendtekst
  retn<-'V'
  txtretn<-1

  FigTypUt <- figtype(outfile, fargepalett='BlaaOff')
  NutvTxt <- length(utvalgTxt)
  vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

  PlotMatrise[ , Ngr < 5] <- 0
  grtxt2 <-  paste0('(N=', Ngr, ')')
  grtxt2[Ngr<5] <- '(N<5)'

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
    arrows(x0=pos, y0=KINed, x1=pos, y1=KIOpp, code=0, angle=90, lwd=2, length=0.1, col=farger[3]) # konfidensintervall
  }


  par('fig'=c(0, 1, 0, 1))
  if ( outfile != '') {dev.off()}







}
