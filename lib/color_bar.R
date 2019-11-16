# + plot a color bar
color_bar <- function(col, 
                      breaks,
                      nticks=NA, 
                      breaks_as_strings=NA, 
                      title='',
                      cutTails=T,
                      legtxt="",
                      legdig=0,
                      xlim=NA,
                      ylim=NA,
                      cex=1) {
#------------------------------------------------------------------------------
# col. n-vector with colors (n=number of colors)
# breaks. (n+1)-vector with breaks
# nthicks. number of equidistant breaks to plot
# breaks_as_strings.
#------------------------------------------------------------------------------
#    scale = (length(lut)-1)/(max-min)
  x_range<-xlim[2]-xlim[1]
  y_range<-ylim[2]-ylim[1]
  xu<-x_range/50
  yu<-y_range/50
  x1<- xlim[1]+ .5 * xu
  x2<-      x1+  2 * xu
  y1<- ylim[1]+ 25 * yu 
  y2<- ylim[2]- 1.5 * yu 
  dx<- 2.4 * xu
  if (any(!is.na(breaks_as_strings))) {
    nbr<-length(breaks_as_strings)
    ticks<-1:nbr
    breaks<-breaks_as_strings
  } else {
    nbr<-length(breaks)
    if (cutTails) {
      min<-min(breaks[2:(nbr-1)])
      max<-max(breaks[2:(nbr-1)])
      ticks<-round(seq(2, (nbr-1), len=nticks),0)
    } else {
      min<-min(breaks)
      max<-max(breaks)
      ticks<-round(seq(1, nbr, len=nticks),0)
    }
  }
  dy<-(y2-y1)/length(col)
  rect(x1+dx,y1,
       x2+1.5*dx,y2+dx,
       col="white", border=NA)
  text((x2+1.5*dx/2),
       (y1+dy/2+(ticks-1)*dy),
       round(breaks[ticks],legdig),
       cex=cex)
  text((x1+x2)/2,
        y2+dx/2,
        legtxt,
        cex=cex)
  for (i in 1:(length(col))) {
   y = (i-1)*dy + y1 
   rect(x1,y,x2,y+dy, col=col[i], border=NA)
  }
}

