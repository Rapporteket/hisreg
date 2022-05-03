#' Gi en visuell fremstilling av registerets indikatorer over tid
#'
#' @param indikatordata En dataramme med følgende kolonner:
#'                 - Aar
#'                 - Variabel
#'                 - SykehusNavn
#'
#' @export
#'
hisregFigIndikator_aarsamlet <- function(indikatordata, tittel='', terskel=5, minstekrav = NA, maal = NA, skriftStr=0.8, pktStr=1.1,
                               legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = '',
                               lavDG=NA, width=800, height=700, maalretn='hoy', desimal=FALSE, xmax=NA,
                               lavDGtekst='Dekningsgrad < 60 %')
{

  # indikatordata <- indikator1
  # tittel='testtittel'; terskel=5; minstekrav = NA; maal = NA; skriftStr=0.8; pktStr=1.1;
  # legPlass='top'; minstekravTxt='Min.'; maalTxt='Mål'; graaUt=NA; decreasing=F; outfile = '';
  # lavDG=NA; width=800; height=700; maalretn='hoy'; desimal=FALSE; xmax=NA;
  # lavDGtekst='Dekningsgrad < 60 %'

  indikatordata <- indikatordata[indikatordata$Aar > max(indikatordata$Aar)-3, ] # behold bare siste 3 år

  Tabell <- indikatordata %>% dplyr::group_by(SykehusNavn, Aar) %>%
    dplyr::summarise(Antall = sum(Variabel),
                     N = n(),
                     Andel = Antall/N*100)


  AntTilfeller <- tidyr::spread(Tabell[, -c(4,5)], 'Aar', 'Antall')
  AntTilfeller <- dplyr::bind_cols(SykehusNavn=c(as.character(AntTilfeller[["SykehusNavn"]]), "Nasjonalt"),
                                   dplyr::bind_rows(AntTilfeller[,-1], colSums(AntTilfeller[,-1], na.rm = T)))

  N <- tidyr::spread(Tabell[, -c(3,5)], 'Aar', 'N')
  N <- dplyr::bind_cols(SykehusNavn=c(as.character(N[["SykehusNavn"]]), "Nasjonalt"),
                        dplyr::bind_rows(N[,-1], colSums(N[,-1], na.rm = T)))
  N[is.na(N)] <- 0

  AntTilfeller[,paste0(names(AntTilfeller)[2], '-', names(AntTilfeller)[dim(AntTilfeller)[2]])] <-
    rowSums(AntTilfeller[,-1], na.rm = T)
  AntTilfeller <- AntTilfeller[, c(1, (dim(AntTilfeller)[2]-1):dim(AntTilfeller)[2])]
  AntTilfeller <- as.data.frame(AntTilfeller)
  row.names(AntTilfeller) <- AntTilfeller[["SykehusNavn"]]
  AntTilfeller <- AntTilfeller[, -1]
  N[,paste0(names(N)[2], '-', names(N)[dim(N)[2]])] <-
    rowSums(N[,-1], na.rm = T)
  N <- N[, c(1, (dim(N)[2]-1):dim(N)[2])]
  N <- as.data.frame(N)
  row.names(N) <- N[["SykehusNavn"]]
  N <- N[, -1]

  AntTilfeller <- AntTilfeller[,c(2,1)]; N <- N[,c(2,1)]
  andeler <- AntTilfeller/N * 100

  andeler[N < terskel] <- NA
  andeler[rownames(andeler) %in% lavDG, ] <- NA

  # if (decreasing){
  #   rekkefolge <- order(andeler[, dim(andeler)[2]], decreasing = decreasing, na.last = F)
  # } else {
  #   rekkefolge <- order(andeler[, dim(andeler)[2]], decreasing = decreasing, na.last = F)
  # }
  if (decreasing){
    rekkefolge <- order(andeler[, 1], decreasing = decreasing, na.last = F)
  } else {
    rekkefolge <- order(andeler[, 1], decreasing = decreasing, na.last = F)
  }
  andeler <- andeler[rekkefolge, ]
  N <- N[rekkefolge, ]
  andeler[N[, 1]<terskel, 1:2] <- NA
  KI <- binomkonf(AntTilfeller[rekkefolge, 1], N[, 1])*100
  KI[, is.na(andeler[, 1])] <- NA
  pst_txt <- paste0(sprintf('%.0f', andeler[, 1]), ' %')
  pst_txt[N[, 1]<terskel] <- paste0('N<', terskel)
  pst_txt[rownames(andeler) %in% lavDG] <- lavDGtekst
  pst_txt <- c(NA, pst_txt, NA, NA)

  FigTypUt <- rapFigurer::figtype(outfile=outfile, width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], length(andeler[,1]))
  soyleFarger[which(rownames(andeler)=='Nasjonalt')] <- farger[4]
  if (!is.na(graaUt[1])) {soyleFarger[which(rownames(andeler) %in% graaUt)] <- 'gray88'}
  soyleFarger <- c(NA, soyleFarger)

  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig
  oldpar_oma <- par()$oma

  cexgr <- skriftStr
  rownames(andeler) <- paste0(rownames(andeler), ' (', N[, 1], ') ')
  andeler <- rbind(andeler, c(NA,NA,NA))
  rownames(andeler)[dim(andeler)[1]] <- paste0('(N, ', names(andeler)[1], ') ')
  KI <- cbind(c(NA, NA), KI, c(NA, NA))

  # vmarg <- max(0, strwidth(rownames(andeler), units='figure', cex=cexgr)*0.75)
  # par('fig'=c(vmarg, 1, 0, 1))
  par('fig'=c(0.15, 1, 0, 1))
  # par('mar'=c(5.1, 4.1, 5.1, 9.1))
  par('oma'=c(0,1,0,0))

  par('mar'=c(5.1, 4.1, 5.1, 2.1))
  xmax <- min(max(KI, max(andeler, na.rm = T), na.rm = T)*1.15,100)

  andeler <- rbind(c(NA,NA), andeler, c(NA,NA))
  rownames(andeler)[dim(andeler)[1]] <- '  '
  rownames(andeler)[1] <- ' '

  ypos <- barplot( t(andeler[, 1]), beside=T, las=1,
                   xlim=c(0,xmax),
                   names.arg=rep('',dim(andeler)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = 'Andel (%)') # '#96BBE7'

  fargerMaalNiva <-  c('aquamarine3','#fbf850', 'red')

  if (maal > minstekrav & !is.na(maal) & !is.na(minstekrav)) {
    rect(xleft=minstekrav, ybottom=1, xright=maal, ytop=max(ypos)-1.6, col = fargerMaalNiva[2], border = NA)
    rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}
  if (maal < minstekrav & !is.na(maal) & !is.na(minstekrav)) {
    rect(xleft=maal, ybottom=1, xright=minstekrav, ytop=max(ypos)-1.6, col = fargerMaalNiva[2], border = NA)
    rect(xleft=0, ybottom=1, xright=maal, ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}
  if (!is.na(maal) & is.na(minstekrav) & maalretn=='lav') {
    rect(xleft=0, ybottom=1, xright=maal, ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}
  if (!is.na(maal) & is.na(minstekrav) & maalretn=='hoy') {
    rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}

  barplot( t(andeler[, 1]), beside=T, las=1,
           names.arg=rep('',dim(andeler)[1]),
           horiz=T, axes=F, space=c(0,0.3),
           col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)

  title(main = tittel, cex.main=skriftStr*1.1)
  ypos <- as.numeric(ypos) #as.vector(ypos)
  yposOver <- max(ypos)-2 + 0.5*diff(ypos)[1]
  if (!is.na(minstekrav)) {
    lines(x=rep(minstekrav, 2), y=c(-1, yposOver), col=fargerMaalNiva[2], lwd=2)
    par(xpd=TRUE)
    text(x=minstekrav, y=yposOver, labels = minstekravTxt,
         pos = 4, cex=cexgr*0.65, srt = 90)
    par(xpd=FALSE)
  }
  if (!is.na(maal)) {
    lines(x=rep(maal, 2), y=c(-1, yposOver), col=fargerMaalNiva[1], lwd=2)
    barplot( t(andeler[, 1]), beside=T, las=1,
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)
    par(xpd=TRUE)
    text(x=maal, y=yposOver, labels = maalTxt, pos = 4, cex=cexgr*0.65, srt = 90) #paste0(maalTxt,maal,'%')
    par(xpd=FALSE)
  }
  arrows(x0 = KI[1,], y0 = ypos, x1 = KI[2,], y1 = ypos,
         length=0.5/max(ypos), code=3, angle=90, lwd=1.8, col='gray') #, col=farger[1])
  legend('bottom', cex=0.9*cexgr, bty='n',
         lwd=1.8, lty = 1, pt.cex=1.8, col='gray',
         legend=paste0('Konfidensintervall ', names(N)[1]))

  axis(1,cex.axis=0.9)
  mtext( hisreg::wrap.it(rownames(andeler), 21), side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  antAar <- dim(andeler)[2]

  if (dim(andeler)[2]==2) {
    par(xpd=TRUE)
    points(y=ypos, x=andeler[, dim(andeler)[2]],cex=pktStr, pch= 19)
    par(xpd=FALSE)
    if (legPlass=='nede'){
      legend('bottomright', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',
             lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
             legend=rev(names(N)), ncol = 1)}
    if (legPlass=='top'){
      # legend('top', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
      #        lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
      #        legend=names(N), ncol = dim(andeler)[2])
      legend('top', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
             lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
             legend=rev(paste0(names(N), " (", as.character(round(as.numeric(andeler[substr(rownames(andeler), 1, 9) == "Nasjonalt", ]), 1)),
                           "%, N = ", N[rownames(N) == "Nasjonalt", ], ")")),
             ncol = 1)}
    if (legPlass=='topleft'){
      legend('topleft', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
             lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
             legend=rev(names(N)), ncol = dim(andeler)[2])}
    if (legPlass=='topright'){
      legend('topright', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
             lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
             legend=rev(names(N)), ncol = dim(andeler)[2])}

  } else {

    par(xpd=TRUE)
    points(y=ypos, x=andeler[,1],cex=pktStr) #'#4D4D4D'
    points(y=ypos, x=andeler[,2],cex=pktStr,pch= 19)
    par(xpd=FALSE)
    if (legPlass=='nede'){
      legend(x=82, y=ypos[2]+1 ,xjust=0, cex=cexgr, bty='n', #bg='white', box.col='white',
             lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
             legend=names(N) )}
    if (legPlass=='top'){
      legend('top', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
             lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
             legend=names(N), ncol = dim(andeler)[2])
    }
  }
  text(x=0, y=ypos, labels = pst_txt, cex=skriftStr, pos=4)#

  par('mar'= oldpar_mar)
  par('fig'= oldpar_fig)
  par('oma'= oldpar_oma)

  if ( outfile != '') {dev.off()}
}
