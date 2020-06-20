#------------------------------------------------------------------------------
# optimal interpolation 
oi_var_gridpoint_by_gridpoint<-function(i,
                                        dh=10000, #m
                                        box_o_nearest_halfwidth=100000, #m
                                        dz=NA,
                                        lafmin=NA,
                                        dh_adaptive=F,
                                        corr="soar",
                                        pmax,
                                        fg=NA,
                                        fg_gamma=NA,
                                        fg_min=NA,
                                        fg_max=NA,
                                        return_fg_only=F,
                                        succ_corr=F,
                                        y_elab=F,
                                        loocv=F,
                                        o_errvar_min=0.001,
                                        o_errvar_max=4,
                                        xa_errvar_min=0.001,
                                        xa_errvar_max=4) {
# global variables: xgrid_spint, ygrid_spint, zgrid_spint, lafgrid_spint,
#                   xobs_spint, yobs_spint, zobs_spint, lafobs_spint,
#                   yo_spint, yb_spint, xb_spint, eps2_spint
#                   nobs
# Description:
# OI analysis at the i-th point (at xgrid_spint[i],ygrid_spint[i],...)
# given the set of observations yo_spint (at xobs_spint,yobs_spint,...)
# with or without a backgorund
# 
# Input arguments
# i: gridpoint index (refers to vectors Xgrid_spint,...,xb_spint
# dh: horizontal de-correlation length (m)
# box_o_nearest_halfwidth: half-width of the square box used to select the 
#                          nearest observations
# dz: vertical de-correlation length (m, NA if z is not considered)
# lafmin: land-area fraction minimum value for the de-correlation factor 
#         (0-1, NA if laf is not considered)
# dh_adaptive: logical. F if dh is the actual horizontal de-correlation length
#                       T if the actual horizontal de-correlation length is 
#                         obtained as the 10-th percentile of the distances
#                         between the i-th gridpoint and the nearest observations
#                         (up to the nearest pmax observations). In this case dh
#                         is used as a lower limit.
# corr: model for the correlation functions. 
#       "soar" second order auto-regressive (only horizontal distance is used).
#       "gaussian" gaussian.
# pmax: mximum number of nearest observations to consider
# fg: method used to compute the background (first-guess). 
#  NA (default), background available in yb_spint, xb_spint
#  "linear", background as a linear function of the vertical coordinate  
#  "Frei", background as a non-linear function of the vertical coordinate  
#  "mean", background set to the mean value of the nearest observations
# fg_gamma: vertical lapse rate used for fg="linear" and optimized for fg="Frei"
# fg_min: minimum allowed value for the background
# fg_max: maximum allowed value for the background
# succ_corr: logical.
#  F (default). the background is used as it is.
#  T. three steps of successive corrections are applied to the background.
# y_elab: logical.
#  F (default). grid.. vectors and obs.. vectors refer to the same locations
#  T. grid.. and obs.. vectors refer to different locations
# loocv. logical. Leave one out cross-validation
#  F (default). standard run, use all the observations
#  T. run in loocv mode, without considering the observations at the i-th gridpoint
# o_errvar_min. minimum allowed value for the observation error variance.
# o_errvar_max. maximum allowed value for the observation error variance.
# xa_errvar_min. minimum allowed value for the analysis error variance.
# xa_errvar_max. maximum allowed value for the analysis error variance.
#
# return(c(xa,xa_errvar,o_errvar,xidi,idiv,av,dh))
# NOTE: av is the leave-one-out CV. However, its errvar is not returned.
#       todo: figure out how to compute the leave-ione-out errvar.
#------------------------------------------------------------------------------
# select the p_i observations nearest to the i-th gridpoint
  if (i%%1000==0) print(paste(i,Sys.time()-t0))
  deltax <- abs( xgrid_spint[i] - xobs_spint)
  av     <- NA
  if ( y_elab) {
    res  <- ifelse( exists( "yb_spint"), yb_spint[i], NA)
    idiv <- 0
    xidi <- 1/(1+eps2_spint[i])
    if ( loocv) deltax[i] <- box_o_nearest_halfwidth + 1
    if ( length( which( flagx <- ( deltax < box_o_nearest_halfwidth)))==1) { 
      if ( return_fg_only) { return( NA)} 
      else { return( c( res, NA, NA, xidi, idiv, av, dh))}
    }
    deltay <- abs( ygrid_spint[i]-yobs_spint[flagx])
    if ( length( which( flagy <- ( deltay < box_o_nearest_halfwidth)))==1) {
        if (return_fg_only) {return(NA)} 
        else { return(c(res,NA,NA,xidi,idiv,av,dh))}
    }
  } else {
    res  <- ifelse( exists( "xb_spint"), xb_spint[i], NA)
    idiv <- NA
    xidi <- 0
    if ( !any( flagx <- ( deltax < box_o_nearest_halfwidth))) {
      if ( return_fg_only) { return( NA)} 
      else { return( c( res, NA, NA, xidi, idiv, av, dh))}
    }
    deltay <- abs( ygrid_spint[i]-yobs_spint[flagx])
    if (!any( flagy <- ( deltay < box_o_nearest_halfwidth))) {
      if ( return_fg_only) { return( NA)} 
      else { return( c( res, NA, NA, xidi, idiv, av, dh))}
    }
  }
  ixa <- which( flagx)[flagy]
  if (length(ixa)==0) 
    if (return_fg_only) {return(NA)} 
    else { return(c(res,NA,NA,xidi,idiv,av,dh))}
  #
  if (length(ixa)>pmax) {
    disth2<-deltax[ixa]*deltax[ixa]+deltay[flagy]*deltay[flagy]
    ixb<-order(disth2, decreasing=F)[1:pmax]
    ixa<-ixa[ixb]
    disth2<-disth2[ixb]
    rm(ixb)
  }
  p_i<-length(ixa)
  # first-guess from nearest observations
  if (!is.na(fg)) {
    yo_mean<-mean(yo_spint[ixa])
    # Frei profile may provide unrealistic values if 
    #  it is required to extrapole a value for elevations 
    #  far from the ones used to optimize the parameters
    #  OR if the elevations used are within a narrow layer
    if ( fg == "Frei") {
      zmin <- sort( zobs_spint[ixa])[ min( c( length(ixa), 2      ))]
      zmax <- sort( zobs_spint[ixa])[ max( c(           1, (p_i-1)))]
      if ( (zmin-zgrid_spint[i])>10 |
           (zgrid_spint[i]-zmax)>10 |
           (zmax-zmin)<25 ) fg <- "linear"
    }
    if ( fg == "linear") {
      par <- c(yo_mean)
      opt <- optimize( f          = opt_vertprof_basic,
                       interval   = c(fg_min,fg_max),
                       vert_coord = zobs_spint[ixa],
                       gamma      = fg_gamma,
                       obs        = yo_spint[ixa])
      if ( !return_fg_only)
        yb_spint_i <- tvertprof_basic( zobs_spint[ixa],
                                       t0    = opt$minimum,
                                       gamma = fg_gamma)
      xb_i <- tvertprof_basic( zgrid_spint[i],
                               t0    = opt$minimum,
                               gamma = fg_gamma)
    } else if (fg=="Frei") {
      par <- c( yo_mean,
                fg_gamma,
                5,
                zmin,
                zmax)
      opt <- optim( par, 
                    opt_vertprof_Frei_2014,
                    vert_coord = zobs_spint[ixa],
                    obs        = yo_spint[ixa])
      if (!return_fg_only)
        yb_spint_i <- tvertprof_Frei_2014( z     = zobs_spint[ixa],
                                 t0    = opt$par[1],
                                 gamma = opt$par[2],
                                 a     = opt$par[3],
                                 h0    = opt$par[4],
                                 h1i   = opt$par[5])
      xb_i <- tvertprof_Frei_2014( z     = zgrid_spint[i],
                         t0    = opt$par[1],
                         gamma = opt$par[2],
                         a     = opt$par[3],
                         h0    = opt$par[4],
                         h1i   = opt$par[5])
    } else if (fg=="mean") {
      if ( !return_fg_only)
        yb_spint_i <- rep(yo_mean,length=p_i)
      xb_i <- yo_mean
    }
  } else {
    if (!return_fg_only) yb_spint_i<-yb_spint[ixa]
    xb_i<-xb_spint[i]
  } # end compute first-guess
  if (!is.null(fg_min)) {
    if (!is.na(fg_min) & !is.nan(fg_min)) {
      if (!return_fg_only)
        yb_spint_i[which(yb_spint_i<fg_min)]<-fg_min
      xb_i<-max(xb_i,fg_min)
    }
  }
  if (!is.null(fg_max)) {
    if (!is.na(fg_max) & !is.nan(fg_max)) {
      if (!return_fg_only)
        yb_spint_i[which(yb_spint_i>fg_max)]<-fg_max
      xb_i<-min(xb_i,fg_max)
    }
  }
  # probability of error in the background estimation
  if (fg == "Frei") {
    sd <- ifelse( length(ixa)>1, max( 0.5, sd(yo_spint[ixa])), 0.5)
    if ( ( abs( xb_i -mean(yo_spint[ixa])) /sd) > 5 ) xb_i <- NA
  }
  # return_fg_only
# plot for debug
#xr<-range(c(yo_spint[ixa],yb_spint_i,xb_i))
#yr<-range(c(zobs_spint[ixa],zgrid_spint[i]))
#if ( (i %%1) ==0) {
#png(file=paste0(fg,"/fig_",formatC(i,width=7,flag="0",format="d"),".png"),width=600,height=800)
#par(mar=c(4,5,1,1),cex.axis=1.5)
#plot(yo_spint[ixa], zobs_spint[ixa],cex=2,xlim=xr,ylim=c(0,1550),
#     xlab="Temperature (degC)",ylab="Elevation (m amsl)",main="",pch=21,bg="gray")
#points(yb_spint_i, zobs_spint[ixa],pch=21,bg="gold",cex=2)
#points(xb_i, zgrid_spint[i],pch=21,bg="red",cex=3 )
#abline(h=seq(0,3000,by=50),lwd=1,lty=2,col="gray")
#abline(h=seq(0,3000,by=100),lwd=1,lty=2,col="gray")
#abline(h=c(0,1000,2000),lwd=3,col="gray")
#ts<--50:50
#for (t0 in seq(-50,50,by=1)) {
#  lines(ts,1/fg_gamma*(ts-t0),lty=2,lwd=2,col="gray")
#}
#dev.off()
#png(file=paste0("map/fig_",formatC(i,width=7,flag="0",format="d"),".png"),width=600,height=800)
#par(mar=c(1,1,1,1))
#plot(xgrid_spint, ygrid_spint,cex=.1,
#     xlab="",ylab="",main="",col="gray",axes=F)
#points(xobs_spint, yobs_spint,cex=.1,col="lightgray")
#rect( min(xobs_spint[ixa]), min(yobs_spint[ixa]),
#      max(xobs_spint[ixa]), max(yobs_spint[ixa]),lwd=2 )
#points(xgrid_spint[i], ygrid_spint[i],pch=21,bg="red",cex=.5 )
#box()
#dev.off()
#print(i)
#}
  if (return_fg_only) return(xb_i)
  #
  if (!exists("disth2")) disth2<-deltax[ixa]*deltax[ixa]+deltay[flagy]*deltay[flagy]
  # correlation matrices
  if (dh_adaptive) {
    dh<-max(dh,as.numeric(quantile(sqrt(disth2),probs=0.1)))
  }
  dh2<-dh*dh
  if (corr=="gaussian") {
    rloc<-exp( -0.5* disth2 / dh2 )
  } else if (corr=="soar")  {
    distnorm_loc<-sqrt(disth2) / dh
    rloc<-(1+distnorm_loc)*exp(-distnorm_loc)
    if (!succ_corr) rm(distnorm_loc)
  }
  if (corr=="gaussian") {
    S<-exp(-0.5*(outer(yobs_spint[ixa],yobs_spint[ixa],FUN="-")**2. + 
                 outer(xobs_spint[ixa],xobs_spint[ixa],FUN="-")**2)/dh2)
  } else if (corr=="soar")  {
    distnorm<-sqrt(outer(yobs_spint[ixa],yobs_spint[ixa],FUN="-")**2. + 
                   outer(xobs_spint[ixa],xobs_spint[ixa],FUN="-")**2) / dh 
    S<-(1+distnorm)*exp(-distnorm)
    if (!succ_corr) rm(distnorm)
  }
  # successive corrections (Barnes scheme) step
  if (succ_corr) {
    for (sc in 3:1) {
      if (corr=="gaussian") {
        S1<-S**(1/sc**2)
        rloc1<-rloc**(1/sc**2)
      } else if (corr=="soar")  {
        distnorm1<-distnorm/sc
        S1<-(1+distnorm1)*exp(-distnorm1)
        distnorm1_loc<-distnorm_loc/sc
        rloc1<-(1+distnorm1_loc)*exp(-distnorm1_loc)
      }
      yb_spint_i<-yb_spint_i+crossprod(S1,(yo_spint[ixa]-yb_spint_i)) / 
                  (rowSums(S1)+eps2_spint[ixa])
      xb_i<-xb_i+sum(rloc1*(yo_spint[ixa]-yb_spint_i))/sum(rloc1+eps2_spint[ixa])
    }
    rm(S1,rloc1)
    if (corr=="soar") rm(distnorm1,distnorm,distnorm_loc,distnorm1_loc)
  }
  # adjust gaussian correlations by taking into account more geo-parameters
  if (corr=="gaussian") {
    if (!is.na(dz)) {
      S<-S*exp(-0.5*abs(outer(zobs_spint[ixa],zobs_spint[ixa],FUN="-"))/dz2) 
      rloc<-rloc*exp(-0.5*deltaz[ixa]/dz2)
    }
    if (!is.na(lafmin)) {
      S<-S*(1-(1-lafmin)*
         abs(outer(lafobs_spint[ixa],lafobs_spint[ixa],FUN="-")))
      rloc<-rloc*(1-(1-lafmin)*deltalaf[ixa])
    }
  }
  # innovation
  di<-yo_spint[ixa]-yb_spint_i
  # OI analysis
  SRinv<-chol2inv(chol( (S+diag(x=eps2_spint[ixa],length(ixa))) ))
  xidi<-sum(rloc*as.vector(rowSums(SRinv)))
  SRinv_di<-crossprod(SRinv,di)       
  o_errvar<-min(c(o_errvar_max,
                  max(c(o_errvar_min,
                        mean(di*(di-crossprod(S,SRinv_di)))))))
  rm(S)
  xa_errvar<-min(c(xa_errvar_max,
                   max(c(xa_errvar_min,
                         (o_errvar/ mean(eps2_spint[ixa])) * 
                         (1-sum(as.vector(crossprod(rloc,SRinv))*rloc))))))
  xa<-xb_i+sum(rloc*as.vector(SRinv_di))
  if (y_elab & !loocv) {
    ii<-which(ixa==i)
    Wii<-sum(rloc*SRinv[ii,])
    idiv<-(xidi-Wii)/(1-Wii)
    av<-(xa-Wii*yo_spint[i])/(1-Wii)
  }
  # debug
  #if (yo_to_check[i]>20) {
  #png(file=paste0("png/tvert_",i,".png"),width=800,height=800)
  #plot(aaa,1:2000,ylim=range(c(zobs_spint[ixa],zgrid_spint[i])),xlim=range(c(yo_spint[ixa],yb_spint_i,xb_i,yo_to_check[i])),
  #main=paste(round(xa,1),round(xa_errvar,3),round(o_errvar,3),round(xidi,3),round(dh,1),round((yo_to_check[i]-xa)**2/(xa_errvar+o_errvar),2)))
  #points(yo_spint[ixa],zobs_spint[ixa],pch=21,bg="blue")
  #points(yb_spint_i,zobs_spint[ixa],pch=21,bg="cyan")
  #points(xb_i,zgrid_spint[i],pch=21,bg="red")
  #points(yo_to_check[i],zgrid_spint[i],pch=21,bg="red")
  #dev.off()
  #}
  return(c(xa,xa_errvar,o_errvar,xidi,idiv,av,dh))
}

