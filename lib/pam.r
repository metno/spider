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
              borders_par=list(name=NA,
                               layer=NA,
                               proj4=NA,
                               proj4to=NA,
                               lwd=6),
              fig_par=list(mar=c(.5,.5,.5,.5),
                           disagg_fact=NA,
                           xlim=NA,
                           ylim=NA,
                           colors=NA,
                           breaks=NA,
                           fool_coltab=NA,
                           fool_path=NA,
                           fool_breaks=NA),
              leg_par=list(type=NA,
                           nticks=11,
                           height=NA,
                           dig=0),
              raster_to_plot,
              verbose=T
              ) {
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
  if (verbose) {
    print(paste("distributions of values to plot min,10-th perc,20-th perc,...,max",
      toString(round(quantile(getValues(raster_to_plot),probs=seq(0,1,by=0.1),na.rm=T),5))))
  }
  # borders
  if (!is.na(borders_par$name)) {
    borders<-readOGR(borders_par$name,borders_par$layer,verbose=F)
  }
  # set xlim and ylim
  if (any(is.na(fig_par$xlim))) 
    fig_par$xlim<-as.numeric(extent(raster_to_plot)[1:2])
  if (any(is.na(fig_par$ylim))) 
    fig_par$ylim<-as.numeric(extent(raster_to_plot)[3:4])
  # colors and breaks
  if (!is.na(fig_par$fool_coltab)) {
    suppressPackageStartupMessages(require(fool))
    coltab<-load_color_table(path=fig_par$fool_path,abbrv=fig_par$fool_coltab)
    if (fig_par$fool_coltab_rev) coltab<-rev(coltab)
    if (any(is.na(fig_par$breaks))) {
      fig_par$breaks<-seq(fig_par$fool_breaks[1],
                          fig_par$fool_breaks[2],
                          length=(length(coltab)+1))
    }
    fig_par$colors<-c(coltab)
  }
  if (length(fig_par$breaks)!=(length(fig_par$colors)+1)) {
    print(paste("must have one more break than colors (#breaks,#colors)",
                length(fig_par$breaks),length(fig_par$colors)))
    return(NA)
  }
  # ensure no blank regions in the map
  dat<-getValues(raster_to_plot)
  if (any(ix<-(dat<min(fig_par$breaks) & !is.na(dat)))) 
    raster_to_plot[which(ix)]<-min(fig_par$breaks)
  if (any(ix<-(dat>max(fig_par$breaks) & !is.na(dat)))) 
    raster_to_plot[which(ix)]<-max(fig_par$breaks)
  if (!is.na(fig_par$disagg_fact)) 
    raster_to_plot<-disaggregate(raster_to_plot,fact=fig_par$disagg_fact,method="bilinear")
  # plot!
  if (!is.na(ffout$name)) 
    png(file=ffout$name,width=ffout$width,height=ffout$height)
  par(mar=fig_par$mar)
  if (!any(is.na(fig_par$breaks))) {
    image(raster_to_plot,
          col=fig_par$colors,
          breaks=fig_par$breaks,
          xlim=fig_par$xlim,
          ylim=fig_par$ylim,
          xlab="",
          ylab="",axes=F)
  } else {
    image(raster_to_plot,
          xlim=fig_par$xlim,
          ylim=fig_par$ylim,
          xlab="",
          ylab="",axes=F)
  }
  # plot borders
  if (!is.na(borders_par$name)) plot(borders,add=T,lwd=borders_par$lwd) 
  # plot legend
  if (!is.na(leg_par$type)) {
    if (leg_par$type=="color_bar") {
      color_bar(col=fig_par$colors,
                breaks=fig_par$breaks,
                xlim=fig_par$xlim,
                ylim=fig_par$ylim,
                height=leg_par$height,
                cex=2.3,
                cutTails=F,
                nticks=11,
                legdig=leg_par$dig)
    } else if (leg_par$type=="color_bar_mybreaks") {
      tstr<-vector()
      for (i in 1:length(fig_par$colors)) {
        if (i==length(fig_par$colors)) {
      #    tstr[i]<-paste0(">",br[i],"mm")
        } else {
          tstr[i]<-paste0(round(fig_par$breaks[i+1],1))
        }
      }
      color_bar(col=fig_par$colors,
                breaks_as_strings=tstr,
                xlim=fig_par$xlim,
                ylim=fig_par$ylim,
                height=leg_par$height,
                cex=2.3,
                cutTails=F,
                legdig=leg_par$dig)

    }
  }
  box()
  # end of plot
  if (!is.na(ffout$name)) dev.off()
  if (verbose) print(paste("written file",ffout$name))
}
