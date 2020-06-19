#+ vertical profile of temperature (Frei, 2014)
tvertprof <- function( z, 
                       t0, 
                       gamma, 
                       a, 
                       h0,
                       h1i) {
# ref:
# Frei, C. (2014). Interpolation of temperature in a mountainous region 
#  using nonlinear profiles and non‐Euclidean distances.
#  International Journal of Climatology, 34(5), 1585-1605.
# --< Input >--
#  z     = array/numeric. elevations [m amsl]
#  t0    = array/numeric. temperature at z=0 [K or degC]
#  gamma = array/numeric. temperature lapse rate [K/m]
#  a     = array/numeric. inversion strength  [K]
#  h0    = array/numeric. z where inversion starts [m]
#  h1i   = array/numeric. h0+h1i is z where inversion stops [m]
#       (Frei uses h1 directly, I use an increment to h0 so to avoid ending
#        up with h1<=h0 during the optimization)
# --< Output >--
#  res   = array/numeric. temperature [K or degC]
#------------------------------------------------------------------------------
  res   <- z
  res[] <- NA
  if (    length(t0) != length(z)) 
    { aux<-z; aux[]<-NA; aux[]<-t0[1]; t0<-aux}
  if ( length(gamma) != length(z)) 
    { aux<-z; aux[]<-NA; aux[]<-gamma[1]; gamma<-aux}
  if (     length(a) != length(z)) 
    { aux<-z; aux[]<-NA; aux[]<-a[1]; a<-aux}
  if (    length(h0) != length(z)) 
    { aux<-z; aux[]<-NA; aux[]<-h0[1]; h0<-aux}
  if (   length(h1i) != length(z)) 
    { aux<-z; aux[]<-NA; aux[]<-h1i[1]; h1i<-aux}
  if ( any(h1i==0 & !is.na(h1i))) h1i[h1i==0 & !is.na(h1i)] <- 0.1
  h1   <- h0 + abs( h1i)
  flag <- !is.na(z) & !is.na(t0) & !is.na(gamma) & !is.na(a) & !is.na(h0) & !is.na(h1i)
  z.le.h0 <- which( z <= h0 & flag )
  z.ge.h1 <- which( z >= h1 & flag )
  z.in    <- which( z >  h0 & z < h1 & flag)
  if ( length(z.le.h0) > 0) 
    res[z.le.h0] <- t0[z.le.h0] + gamma[z.le.h0] * z[z.le.h0] - a[z.le.h0] 
  if ( length(z.ge.h1) > 0) 
    res[z.ge.h1] <- t0[z.ge.h1] + gamma[z.ge.h1] * z[z.ge.h1] 
  if ( length(z.in) > 0)
   res[z.in] <- t0[z.in] + gamma[z.in] * z[z.in] - 
                a[z.in]/2 * ( 1 + cos( pi * ( z[z.in] -h0[z.in]) / (h1[z.in]-h0[z.in])))
  return(res)
}

#+ vertical profile of temperature (linear)
tvertprof_basic <- function( z, 
                             t0,
                             gamma) {
# input
#  z= array. elevations [m amsl]
#  t0= numeric. temperature at z=0 [K or degC]
#  gamma=numeric. temperature lapse rate [K/m]
# Output
#  t= array. temperature [K or degC]
#------------------------------------------------------------------------------
  return( t0 + gamma * z)
}

#+ cost function used for optimization of vertprof parameter
vertprofbasic2opt<-function(par,vert_coord,gamma,obs) {
  pred<-tvertprof_basic(z=vert_coord,t0=par[1],gamma=gamma)
  return(log((mean((pred-obs)**2))**0.5))
}

#+ cost function used for optimization of vertprof parameter
vertprof2opt<-function(par,vert_coord,obs,sd) {
  pred <- tvertprof( z     = vert_coord,
                     t0    = par[1],
                     gamma = par[2],
                     a     = par[3],
                     h0    = par[4],
                     h1i   = par[5])
  return( log( (mean( (pred-obs)**2 / sd**2))**0.5))
}

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
      opt <- optimize( f          = vertprofbasic2opt,
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
                    vertprof2opt,
                    vert_coord = zobs_spint[ixa],
                    obs        = yo_spint[ixa])
      if (!return_fg_only)
        yb_spint_i <- tvertprof( z     = zobs_spint[ixa],
                                 t0    = opt$par[1],
                                 gamma = opt$par[2],
                                 a     = opt$par[3],
                                 h0    = opt$par[4],
                                 h1i   = opt$par[5])
      xb_i <- tvertprof( z     = zgrid_spint[i],
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


# 
#------------------------------------------------------------------------------
latte_express_step1 <- function( i,
                                 dh=10000, #m
                                 box_o_nearest_halfwidth=100000, #m
                                 dz=NA,
                                 pmax,
                                 fg="Frei",
                                 fg_gamma=NA,
                                 xgrid_spint,
                                 ygrid_spint,
                                 zgrid_spint,
                                 xobs_spint,
                                 yobs_spint,
                                 zobs_spint,
                                 yo_spint,
                                 fg_min=NA,
                                 fg_max=NA) {
#------------------------------------------------------------------------------
# select the p_i observations nearest to the i-th gridpoint
  if (i%%1000==0) print(paste(i,Sys.time()-t0))
  deltax <- abs( xgrid_spint[i] - xobs_spint)
  if ( !any( flagx <- ( deltax < box_o_nearest_halfwidth))) return( rep( NA, 10)) 
  deltay <- abs( ygrid_spint[i]-yobs_spint[flagx])
  if ( !any( flagy <- ( deltay < box_o_nearest_halfwidth))) return( rep( NA, 10)) 
  ixa <- which( flagx)[flagy]
  if ( length( ixa) == 0) return( rep( NA, 10)) 
  #
  if ( length(ixa) > pmax) {
    disth2 <- deltax[ixa]*deltax[ixa]+deltay[flagy]*deltay[flagy]
    ixb    <- order(disth2, decreasing=F)[1:pmax]
    ixa    <- ixa[ixb]
    disth2 <- disth2[ixb]
    rm( ixb)
  }
  p_i <- length( ixa)
  # first-guess from nearest observations
  # Frei profile may provide unrealistic values if 
  #  it is required to extrapole a value for elevations 
  #  far from the ones used to optimize the parameters
  #  OR if the elevations used are within a narrow layer
#  if ( fg == "Frei") {
#    zmin <- sort( zobs_spint[ixa])[ min( c( length(ixa), 2      ))]
#    zmax <- sort( zobs_spint[ixa])[ max( c(           1, (p_i-1)))]
#    if ( (zmax-zmin) < 25) fg <- "linear"
#  }
#  if ( fg == "linear") {
    lopt <- optimize( f          = vertprofbasic2opt,
                      interval   = c(fg_min,fg_max),
                      vert_coord = zobs_spint[ixa],
                      gamma      = fg_gamma,
                      obs        = yo_spint[ixa])
    yb_lin <- tvertprof_basic( z     = zobs_spint[ixa], 
                           t0    = lopt$minimum,
                           gamma = fg_gamma)
    
#    res <- c( opt$minimum, NA, NA, NA, NA, range(zobs_spint[ixa]), range(yo_spint[ixa]), 0)
#  } else if ( fg == "Frei") {
#    opt <- optim( c( mean( yo_spint[ixa]), fg_gamma, 5, min( zobs_spint[ixa]), max( zobs_spint[ixa])), 
#                  vertprof2opt,
#                  vert_coord = zobs_spint[ixa],
#                  obs        = yo_spint[ixa])
    zq <- as.numeric( quantile( zobs_spint[ixa], probs = c( 0.1, 0.25, 0.75, 0.9)))
    sd <- zobs_spint[ixa]; sd[] <- NA
    for (j in seq(100,3000,by=100)) {
      if (j==100) { ix <- which( zobs_spint[ixa] < 100) }
      else if (j==3000) { ix <- which( zobs_spint[ixa] >= 3000) }
      else { ix <- which( zobs_spint[ixa] >= j & zobs_spint[ixa] < (j+100) ) }
      if (length(ix)<10) next 
      sd[ix] <- max( c( .1, sd( yo_spint[ixa][ix] - yb_lin[ix])))
    }
    sd[is.na(sd)] <- max( sd, na.rm=T)
print("---------------------------------------------------")
print(zq)
print(  round(c( lopt$minimum, fg_gamma, sd( (yo_spint[ixa]-yb_lin)), zq[2], max(c(10,zq[3]-zq[2]))),4))
    opt <- constrOptim( theta = c( lopt$minimum, fg_gamma, sd( (yo_spint[ixa]-yb_lin)), zq[2], max(c(10,zq[3]-zq[2]))),
                        grad  = NULL,
                        f     = vertprof2opt,
                        ui    = rbind( c( 1, 0, 0, 0, 0), c( -1,  0,  0,  0,  0), 
                                       c( 0, 1, 0, 0, 0), c(  0, -1,  0,  0,  0),
                                       c( 0, 0, 1, 0, 0), c(  0,  0, -1,  0,  0),
                                       c( 0, 0, 0, 1, 0), c(  0,  0,  0, -1,  0),
                                       c( 0, 0, 0, 0, 1), c(  0,  0,  0,  0, -1)),
                        ci= c(  -60, -60,
                               -0.1, 0.001,
                                -50, -50,
                               -100, -3000,
                               -100, -3000),
#                               -100, -max(zq[3],100),
#                               -100, -max(zq[4],200)),
                        sd = sd,
                        vert_coord = zobs_spint[ixa],
                        obs        = yo_spint[ixa])
print(round(opt$par,4))
    res <- c( lopt$minimum, opt$par, range(zobs_spint[ixa]), range(yo_spint[ixa]))
#  }
yb_frei <- tvertprof( z    = zobs_spint[ixa],
                     t0    = opt$par[1],
                     gamma = opt$par[2],
                     a     = opt$par[3],
                     h0    = opt$par[4],
                     h1i   = opt$par[5])

# plot for debug
xr<-range(c(yo_spint[ixa],yb_frei,yb_lin))
yr<-range(zobs_spint[ixa])
if ( (i %%1) ==0) {
png(file=paste0("bkg/fig_",formatC(i,width=7,flag="0",format="d"),".png"),width=600,height=800)
par(mar=c(4,5,3,1),cex.axis=1.5)
plot(yo_spint[ixa], zobs_spint[ixa],cex=2,xlim=xr,ylim=c(0,1550),
     xlab="Temperature (degC)",ylab="Elevation (m amsl)",main="",pch=21,bg="gray")
points(yb_lin,  zobs_spint[ixa],pch=21,bg="gold",cex=2)
points(yb_frei, zobs_spint[ixa],pch=21,bg="red",cex=3 )
abline(h=seq(0,3000,by=50),lwd=1,lty=2,col="gray")
abline(h=seq(0,3000,by=100),lwd=1,lty=2,col="gray")
abline(h=c(0,1000,2000),lwd=3,col="gray")
ts<--50:50
for (t0 in seq(-50,50,by=1)) {
  lines(ts,1/fg_gamma*(ts-t0),lty=2,lwd=2,col="gray")
}
par(new=T)
plot(sd,zobs_spint[ixa],xlim=c(0,max(sd)),ylim=c(0,1550),axes=F)
axis(3)
dev.off()
png(file=paste0("map/fig_",formatC(i,width=7,flag="0",format="d"),".png"),width=600,height=800)
par(mar=c(1,1,1,1))
plot(xgrid_spint, ygrid_spint,cex=.1,
     xlab="",ylab="",main="",col="gray",axes=F)
points(xobs_spint, yobs_spint,cex=.1,col="lightgray")
rect( min(xobs_spint[ixa]), min(yobs_spint[ixa]),
      max(xobs_spint[ixa]), max(yobs_spint[ixa]),lwd=2 )
points(xgrid_spint[i], ygrid_spint[i],pch=21,bg="red",cex=.5 )
box()
dev.off()
print(i)
}


  return( res)
}

# 
#------------------------------------------------------------------------------
latte_express_step2 <- function( i, # <- index over tar
                                 x_tar,
                                 y_tar,
                                 z_tar,
                                 x_par,
                                 y_par,
                                 par,
                                 z_bil,
                                 val_bil,
                                 mah) {
#------------------------------------------------------------------------------
  if (i%%10000==0) print(i)
  dist2 <- (x_tar[i]-x_par)**2 + (y_tar[i]-y_par)**2
  w <- exp(-0.5*dist2/25000**2)
  ix <- which( (w/sum(w)) > 0.00001)
  n  <- length(ix)
  vals <-1/sum(w[ix]) * sum( w[ix] * (
          tvertprof( z=rep( z_tar[i], n), t0=par[ix,1], gamma=par[ix,2], 
                     a=par[ix,3], h0=par[ix,4], h1i=par[ix,5]) - 
          tvertprof( z=rep( z_bil[i], n), t0=par[ix,1], gamma=par[ix,2], 
                     a=par[ix,3], h0=par[ix,4], h1i=par[ix,5]) + val_bil[i]))
#  print(paste(round(val_bil[i],2),round(vals,2),round(z_tar[i]-z_bil[i])))
#  print(cbind(sqrt(dist2[ix])/1000,w[ix]/sum(w[ix])))
#  print(sum(w[ix]/sum(w[ix])))
  vals
}


# 
#------------------------------------------------------------------------------
latte_express <- function( box_o_nearest_halfwidth=100000, #m
                           pmax,
                           fg_gamma=NA) {
#------------------------------------------------------------------------------
jump <- F
if (!jump) {
  if ( !any( !is.na( values_ma <- getValues(rmaster)))) {
    print(paste("warning: all NAs for master grid file",argv$ffmaster))
    return( NULL)
  }
  if ( !any( !is.na( values_ma_dem <- getValues(rmaster_dem)))) {
    print(paste("warning: all NAs for master dem file",argv$ffmasterdem))
    return( NULL)
  }
  if ( !any( !is.na( values_dem <- getValues(r_dem)))) {
    print(paste("warning: all NAs for dem file",argv$ffindem))
    return( NULL)
  }
  # ix_ma, indexes to points that unmasked and not NAs
  if ( length( ix_ma <- which( !is.na( values_ma) & 
                               !is.na( values_ma_dem)))==0) 
    boom("all NAs for intersection of master & master_dem")
  xy_ma   <- xyFromCell( rmaster, ix_ma) #dim nmaster 2
  nmaster <- length(ix_ma)
  xgrid_spint <- xy_ma[,1]
  ygrid_spint <- xy_ma[,2]
  zgrid_spint <- values_ma_dem[ix_ma]
  #
  s <- aggregate( r, fact=50)
  if ( length( ix_agg <- which( !is.na( getValues(s))))==0) 
    boom("all NAs for intersection of agg")
  xy_agg  <- xyFromCell( s, ix_agg)
  ix_agg1 <- which( !is.na(extract( rmaster, xy_agg)))
  ix_agg <- ix_agg[ix_agg1]
  nagg   <- length( ix_agg)
  xagg_spint <- xy_agg[ix_agg1,1]
  yagg_spint <- xy_agg[ix_agg1,2]
  #
  values <- getValues(r)
  if ( length( ix_in <- which( !is.na(values) & !is.na(values_dem)))==0) {
      print("all NAs for intersection of data & data_dem")
      next
  }
  if (argv$ffin_proj4==argv$ffmaster_proj4) {
    coord.new<-xyFromCell(r,ix_in) #nobs 2
  } else {
    cat("coordinate conversion...")
    coord.new<-spTransform( 
                SpatialPoints(xyFromCell(r,ix_in),
                               proj4string=CRS(argv$ffin_proj4)) 
                                          ,CRS(argv$ffmaster_proj4))
    coord.new<-attr(coord.new,"coords") #nobs 2
    cat("ok!\n")
  }
  # 
  nobs       <- length(ix_in)
  xobs_spint <- coord.new[,1]
  yobs_spint <- coord.new[,2]
  zobs_spint <- values_dem[ix_in]
  yo_spint   <- values[ix_in]
  fg_min     <- min( yo_spint) - as.numeric( diff( range( yo_spint)))
  fg_max     <- max( yo_spint) + as.numeric( diff( range( yo_spint)))
  #
  cat( "(LATTE-xpress) interpoLATion using verTical profilEs - the fast way, start ...")
  cat( paste( "#obs/ #grid/ #agg:", nobs, "/", nmaster, "/", nagg))
  #

  #
  if ( !is.na( argv$cores)) {
    arr <- mcmapply( latte_express_step1,
                     1:nagg,
                     mc.cores                = argv$cores,
                     SIMPLIFY                = T,
                     MoreArgs = list(
                       box_o_nearest_halfwidth = argv$latte_halfbox,
                       pmax                    = argv$latte_pmax,
                       xgrid_spint             = xagg_spint,
                       ygrid_spint             = yagg_spint,
                       yobs_spint              = yobs_spint,
                       xobs_spint              = xobs_spint,
                       zobs_spint              = zobs_spint,
                       yo_spint                = yo_spint,
                       fg_gamma                = argv$latte_gamma,
                       fg_min                  = fg_min,
                       fg_max                  = fg_max))
  # no-multicores
  } else {
    arr <- mapply(   latte_express_step1,
                     1:nagg,
                     SIMPLIFY                = T,
                     MoreArgs = list(
                       box_o_nearest_halfwidth = argv$latte_halfbox,
                       pmax                    = argv$latte_pmax,
                       xgrid_spint             = xagg_spint,
                       ygrid_spint             = yagg_spint,
                       yobs_spint              = yobs_spint,
                       xobs_spint              = xobs_spint,
                       zobs_spint              = zobs_spint,
                       yo_spint                = yo_spint,
                       fg_gamma                = argv$latte_gamma,
                       fg_min                  = fg_min,
                       fg_max                  = fg_max))
  }
  cat(paste("done!",round(Sys.time()-t0,1), attr(Sys.time()-t0,"unit"),"\n"))
save( file="tmp.rdata", arr,r,rmaster,r_dem,ix_ma,nagg,argv,nmaster,xgrid_spint,ygrid_spint,zgrid_spint,xagg_spint,yagg_spint)
}
  print("here")
  load("tmp.rdata")
  print("here")
  if ( any( is.na( arr[1,]))) 
    print(paste0("@@ warning: problems in regridding over ",
                 length( which( is.na( arr[1,]))), " points"))
  par <- array( data=NA, dim=c( nagg, 5))
  # arr 1=lin gamma;2=Fr t0;3=Fr gamma;4=Fr a;5= Fr hmn;6=Fr hmx;7=zmn;8=zmx;9=tmn;10=tmx
  par[,1]<- arr[2,]
  par[,2]<- arr[3,]
  par[,3]<- arr[4,]
  par[,4]<- arr[5,]
  par[,5]<- arr[6,]
  #
  rsam <- resample( r, rmaster)
  rsam_dem <- resample( r_dem, rmaster)
  #
  #
  cat( "(LATTE-xpress) interpoLATion using verTical profilEs - the fast way, start ...")
  if ( !is.na( argv$cores)) {
    arr <- mcmapply( latte_express_step2,
                     1:nmaster,
                     mc.cores                = argv$cores,
                     SIMPLIFY                = T,
                     MoreArgs = list(
                       x_tar   = xgrid_spint,
                       y_tar   = ygrid_spint,
                       z_tar   = zgrid_spint,
                       x_par   = xagg_spint,
                       y_par   = yagg_spint,
                       par     = par,
                       z_bil   = getValues(rsam_dem)[ix_ma],
                       val_bil = getValues(rsam)[ix_ma]))
  # no-multicores
  } else {
     arr <- mapply( latte_express_step2,
                     1:nmaster,
                     SIMPLIFY                = T,
                     MoreArgs = list(
                       x_tar   = xgrid_spint,
                       y_tar   = ygrid_spint,
                       z_tar   = zgrid_spint,
                       x_par   = xagg_spint,
                       y_par   = yagg_spint,
                       par     = par,
                       z_bil   = getValues(rsam_dem)[ix_ma],
                       val_bil = getValues(rsam)[ix_ma]))
 
  }
save( file="tmp1.rdata", arr,r,rmaster,r_dem,rsam,ix_ma,nagg,argv,nmaster,xgrid_spint,ygrid_spint,zgrid_spint,xagg_spint,yagg_spint,par)
for (j in 1:nagg) {
  print(j)
  da<- sqrt( (xagg_spint[j]-xgrid_spint)**2 + (yagg_spint[j]-ygrid_spint)**2)
  ix<- which( da<50000 )
xr<-range(c(getValues(rsam)[ix_ma][ix],arr[ix]),na.rm=T)
yf<-tvertprof( z=zgrid_spint[ix], t0=par[j,1], gamma=par[j,2], 
               a=par[j,3], h0=par[j,4], h1i=par[j,5]) 
png(file=paste0("compare/fig_",formatC(j,width=7,flag="0",format="d"),".png"),width=600,height=800)
par(mar=c(4,5,3,1),cex.axis=1.5)
plot( getValues(rsam)[ix_ma][ix], zgrid_spint[ix],cex=1, ylim=c(0,2000),
      xlim=xr,
      xlab="Temperature (degC)",ylab="Elevation (m amsl)",
      main="",pch=21,bg="gray",axes=F)
points( arr[ix], zgrid_spint[ix],cex=1,pch=21,bg="cornflowerblue")
points( yf, zgrid_spint[ix],cex=1,pch=21,bg="gold")
abline(h=seq(0,3000,by=50),lwd=1,lty=2,col="gray")
abline(h=seq(0,3000,by=100),lwd=1,lty=2,col="gray")
abline(h=c(0,1000,2000),lwd=3,col="gray")
ts<--50:50
for (t0 in seq(-50,50,by=1)) {
  lines(ts,1/fg_gamma*(ts-t0),lty=2,lwd=2,col="gray")
}


box()
dev.off()
}
save(file="tmp1.rdata",arr,r,rmaster,ix_ma)
q()
#,r_dem,
#     xgrid_spint,ygrid_spint,zgrid_spint,
#     yo_spint,xobs_spint,yobs_spint,zobs_spint,
#     res,par,ixok,ixno, resl)
  return( res)
}
