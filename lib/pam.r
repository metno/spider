# Function to plot color bar
color.bar1<-function(col, 
                     breaks,
                     breaks_as_strings=NA,
                     nticks=11, 
                     title='', cutTails=T,
                     legtxt="",legdig=0,
                     x1=1000000,
                     y1=6450000,
                     x2=1050000,
                     y2=7530000,
                     dx=50000,
                     cex=2.5) {
#    scale = (length(lut)-1)/(max-min)
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
    breaks<-round(breaks[ticks],legdig)
  }
  dy<-(y2-y1)/length(col)
  rect(x1+dx,y1,
       x2+1.5*dx,y2+dx,
       col="white", border=NA)
#  text((x2+1.5*dx/2),
#       (y1+dy/2+(ticks-1)*dy),
#       breaks,
#       cex=cex)
  text((x1+x2)/2,
        y2+dx/2,
        legtxt,
        cex=cex)
  for (i in 1:(length(col))) {
   y = (i-1)*dy + y1 
   border<-ifelse(i==1,"black","black") 
   rect(x1,y,x2,y+dy, col=col[i], border=border)
   if (i!=length(col)) lines(c(x1,x2+0.1*abs(x2-x1)),c(y+dy,y+dy))
   if (i!=length(col)) text((x2+1.5*dx/2),(y+dy), breaks[i],cex=cex)
  }
}

pam<-function(ffout=list(name=NA,width=800,height=800),
              borders_par=list(name=NA,layer=NA,proj4=NA,proj4to=NA,lwd=2),
              fig_par=list(mar=c(.5,.5,.5,.5),
                           colors=NA,
                           breaks=NA,
                           fool_coltab=NA,
                           fool_path=NA,
                           fool_breaks=NA),
              leg_par=list(type=NA,
                           aux=NA),
              raster_to_plot
              ) {
  if (!is.na(borders_par$name)) {
    borders<-readOGR(borders_par$name,borders_par$layer)
  }
  if (!is.na(fig_par$fool_coltab)) {
    require(fool)
    coltab<-load_color_table(path=fig_par$fool_path,abbrv=fig_par$fool_coltab)
    fig_par$breaks<-seq(fig_par$fool_breaks[1],
                        fig_par$fool_breaks[2],
                        length=(length(coltab)+1))
    fig_par$colors<-c(coltab)
    print(fig_par$breaks)
    print(raster_to_plot)
  }
  if (!is.na(ffout$name)) 
    png(file=ffout$name,width=ffout$width,height=ffout$height)
  par(mar=fig_par$mar)
  if (!any(is.na(fig_par$breaks))) {
    image(raster_to_plot,col=fig_par$colors,breaks=fig_par$breaks,xlab="",ylab="",axes=F)
  } else {
    image(raster_to_plot,xlab="",ylab="",axes=F)
  }
  if (!is.na(borders_par$name)) plot(borders,add=T,lwd=borders_par$lwd) 
  if (!is.na(leg_par$type)) {
    tstr<-vector()
    for (i in 1:length(fig_par$colors)) {
      if (i==length(fig_par$colors)) {
    #    tstr[i]<-paste0(">",br[i],"mm")
      } else {
        tstr[i]<-paste0(round(fig_par$breaks[i+1],1))
      }
    }

    if (leg_par$type=="color_bar") {
#      color.bar1(col=fig_par$colors,breaks_as_strings=tstr,x1=4,x2=5.5,y1=62.8,y2=71.4,dx=1.3,cex=1.3)
      fool::color.bar(col=fig_par$colors,breaks=fig_par$breaks,x1=4,x2=5.5,y1=62.8,y2=71.4,dx=1.3,cex=1.3,nticks=11,legdig=2)
    }
  }
  box()
  if (!is.na(ffout$name)) dev.off()
}
