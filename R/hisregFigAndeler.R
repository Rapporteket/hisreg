#' Lag søylediagram eller som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et søylediagram som viser andeler av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
#' @param valgtVar Hvilken variabel skal plottes
#' @param datoFra Tidligste dato i utvalget (vises alltid i figuren). Tekststreng skrevet som 'YYYY-mm-dd'
#' @param datoTil Seneste dato i utvalget (vises alltid i figuren). Tekststreng skrevet som 'YYYY-mm-dd'
#' @param minald Alder, fra og med (Default: 0)
#' @param maxald Alder, til og med (Default: 130)
#' @param erMann kjønn
#'                 1: menn
#'                 0: kvinner
#'                 99: begge (Default)
#' @param outfile Navn på fil figuren skrives til. Default: '' (Figur skrives
#'    til systemets default output device (som regel skjerm))
#' @param reshID Parameter følger fra innlogging helseregister.no og angir
#'    hvilken enhet i spesialisthelsetjenesten brukeren tilhører
#' @param forlop1 Hovedforløpstype
#'                  1: Kirurgisk intervensjon
#'                  2: Medisinsk intervensjon
#'                  3: Kirurgisk og medisinsk intervensjon
#'                  4: Ingen intervensjon
#' @param forlop2 Underforløp
#'                  1 - Eksisjon lukket med sutur
#'                  2 - Eksisjon med åpen granulering og sekunder tilheling
#'                  3 - Eksisjon med påfølgende hudplantasjon
#'                  4 - CO2-laser
#'                  5 - Deroofing
#' @param enhetsUtvalg Lag figur for
#'                 0: Hele landet
#'                 1: Egen enhet mot resten av landet (Default)
#'                 2: Egen enhet
#' @param preprosess Preprosesser data
#'                 FALSE: Nei (Default)
#'                 TRUE: Ja
#' @param hentData Gjør spørring mot database
#'                 FALSE: Nei, RegData gis som input til funksjonen (Default)
#'                 TRUE: Ja
#'
#' @return En figur med søylediagram av ønsket variabel
#'
#' @export

hisregFigAndeler <- function(RegData, valgtVar, datoFra='2000-01-01', datoTil='2050-01-01', reshID,
                             minald=0, maxald=120, erMann=99, outfile='', forlop1 = 99, forlop2 = 99,
                             enhetsUtvalg=1, preprosess=F, hentData=F)

{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- hisregHentRegData()
    print(names(RegData))
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

  # Initialiserer nødvendige størrelser
  Andeler <- list(Hoved = 0, Rest =0)
  ind <- list(Hoved=which(RegData$AvdRESH == reshID), Rest=which(RegData$AvdRESH != reshID))
  Nrest <- 0

  if (valgtVar %in% c('TidlBeh', 'MedisinskBeh', 'KomplKir', 'KirurgiLokalisering')) {
    flerevar <- 1
  } else {
    flerevar <- 0
  }

  if (flerevar == 0 ) {
    ## Forbered variabler for fremstilling i figur
    PlotParams <- hisregPrepVar(RegData=RegData, valgtVar=valgtVar)
    RegData <- PlotParams$RegData
    PlotParams$RegData <- NA
    if (enhetsUtvalg==1) {
      AntHoved <- table(RegData$VariabelGr[ind$Hoved])
      NHoved <- sum(AntHoved)
      Andeler$Hoved <- 100*AntHoved/NHoved
      AntRest <- table(RegData$VariabelGr[ind$Rest])
      Nrest <- sum(AntRest)	#length(indRest)- Kan inneholde NA
      Andeler$Rest <- 100*AntRest/Nrest
    } else {
      AntHoved <- table(RegData$VariabelGr)
      NHoved <- sum(AntHoved)
      Andeler$Hoved <- 100*AntHoved/NHoved
    }
  }

  #FIGURER SATT SAMMEN AV FLERE VARIABLE, ULIKT TOTALUTVALG
  if (flerevar == 1){
#     utvalg <- c('Hoved', 'Rest')	#Hoved vil angi enhet, evt. hele landet hvis ikke gjøre sml, 'Rest' utgjør sammenligningsgruppa
#     RegDataLand <- RegData

    if (enhetsUtvalg %in% c(0,2)) {
      ind$Hoved <- 1:dim(RegData)[1]
      ind$Rest <- NULL
      medSml <- 0
    } else {						#Skal gjøre sammenlikning
      medSml <- 1
#       ind$Hoved <-which(as.numeric(RegData$AvdRESH)==reshID)
#       ind$Rest <- which(as.numeric(RegData$AvdRESH) != reshID)
    }

    PlotParams <- hisregPrepVar(RegData[ind$Hoved, ], valgtVar=valgtVar)
    AntHoved <- PlotParams$AntVar
    NHoved <- max(PlotParams$NVar, na.rm=T)
    Andeler$Hoved <- 100*PlotParams$AntVar/PlotParams$NVar

    if (medSml == 1) {
      PlotParams2 <- hisregPrepVar(RegData[ind$Rest, ], valgtVar=valgtVar)
      AntRest <- PlotParams2$AntVar
      Nrest <- max(PlotParams2$NVar, na.rm=T)	#length(indRest)- Kan inneholde NA
      Andeler$Rest <- 100*PlotParams2$AntVar/PlotParams2$NVar
      rm(PlotParams2)
    }

  }


  ##-----------Figur---------------------------------------
  tittel <- PlotParams$tittel; grtxt <- PlotParams$grtxt; grtxt2 <- PlotParams$grtxt2;
  subtxt <- PlotParams$subtxt; retn <- PlotParams$retn; cexgr <- PlotParams$cexgr;
  FigTypUt <- figtype(outfile=outfile, fargepalett=hisregUtvalg$fargepalett, pointsizePDF=12)

  #Hvis for få observasjoner..
  if (NHoved < 5 | (Nrest<5 & enhetsUtvalg==1)) {
    farger <- FigTypUt$farger
    plot.new()
    # title(tittel)	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, 'Færre enn 5 registreringer i egen- eller sammenlikningsgruppa', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {
    #Plottspesifikke parametre:

    farger <- FigTypUt$farger
    NutvTxt <- length(utvalgTxt)
    grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf('%.1f', Andeler$Hoved)), '%)', sep='')
    # if (incl_pst) {grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf('%.1f', Andeler$Hoved)), '%)', sep='')}
    # if (incl_N) {grtxtpst <- paste(rev(grtxt), ' (n=', rev(sprintf('%.0f', Andeler$Hoved*NHoved/100)), ')', sep='')}
    vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.8))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))  #Har alltid datoutvalg med
    if (grtxt2 == '') {grtxt2 <- paste(sprintf('%.1f',Andeler$Hoved), '%', sep='')}

    fargeHoved <- farger[1]
    fargeRest <- farger[3]
    antGr <- length(grtxt)
    lwdRest <- 3	# Størrelse på legendmarkør?

    if (retn == 'V' ) {
      #Vertikale søyler
      ymax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 100)
      ylabel <- "Andel pasienter"
      pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab=ylabel,  #main=tittel,
                     sub=subtxt, cex.axis=cexgr, cex.sub=cexgr,	cex.lab=cexgr, #names.arg=grtxt, cex.names=cexgr,
                     col=fargeHoved, border='white', ylim=c(0, ymax))	#farger[c(1,3)]
      mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
      mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
      if (enhetsUtvalg == 1) {
        points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
               lwd=lwdRest, ncol=1, cex=cexgr)
      } else {
        legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexgr)
      }
    }


    if (retn == 'H') {
      #Horisontale søyler
      ymax <- antGr*1.4
      xmax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 100)
      xlabel <- "Andel pasienter (%)"

      pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab=xlabel, #main=tittel,
                     col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
      mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)

      if (enhetsUtvalg == 1) {
        points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
               lwd=lwdRest,	lty=NA, ncol=1, cex=cexgr)
      } else {
        legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexgr)
      }
    }


    krymp <- .9
    title(main = tittel, line=1, font.main=1, cex.main=1.3*cexgr)
    mtext(utvalgTxt, side=3, las=1, cex=krymp*cexgr, adj=0, col=FigTypUt$farger[1], line=c(3+0.8*((length(utvalgTxt) -1):0)))

    par('fig'=c(0, 1, 0, 1))

    if ( outfile != '') {dev.off()}

  }


}













