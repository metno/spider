#+
return_level_fun <- function( i, 
                              lab=NULL,
                              iter_reg=1,
                              year4retlev=5,
                              randomseed=1,
                              iter_bay=50000,
                              burn=47000,
                              sample_radius=NA,
                              verbose=T) {
#------------------------------------------------------------------------------
  options(warn=2)
  if (verbose) t0 <- Sys.time()
  if (lab == "fitGEV_bayesian") {

    # no regionalization and no upscaling, just use the time series of the i-th grid point 
    if (sample_radius < 0 | is.na(sample_radius)) {
      retlev_j <- array( data=NA, dim=c(iter_reg, (nyears*3)))
      location_j  <- log_scale_j <- shape_j <- vector(mode="numeric",length=iter_reg)
      j <- 1
      data <- as.vector(mat[i,])
      proposalParams_mean <- c(0,0,0)
      proposalParams_sd   <- c(0.5,0.5,0.1)
      if ((!any(data!=0)) | (max(abs(diff(data)))==0)) {
        location_j[j]  <- NA
        log_scale_j[j] <- NA
        shape_j[j]     <- NA
        retlev_j[j,1:nyears] <- rep(NA,nyears)
        retlev_j[j,(nyears+1):(2*nyears)] <- rep(NA,nyears) 
        retlev_j[j,(2*nyears+1):(3*nyears)] <- rep(NA,nyears)
      } else {
        if (any(is.na(proposalParams_mean))) proposalParams_mean <- c(0,0,0)
        # use this for reproducible results
        if (!is.na(randomseed)) set.seed(j)
#        t0 <- Sys.time()
        par_bay <- fevd( data,
                         type="GEV", 
                         method="Bayesian", 
                         priorFun="fevdPriorShape",
                         iter=iter_bay,
                         proposalParams=list(mean=proposalParams_mean,
                                             sd=proposalParams_sd))
        t1 <- Sys.time()
  #          print( paste( "time", round(t1-t0,1), attr(t1-t0,"unit")))
        location_j[j]  <-  mean(par_bay$results[(burn+1):iter_bay,1])
        log_scale_j[j] <-  mean(par_bay$results[(burn+1):iter_bay,2])
        shape_j[j]     <-  mean(par_bay$results[(burn+1):iter_bay,3])
        # Return levels
        retlev_bay <- return.level.fevd.bayesian_anyfunction( par_bay, 
                                                              return.period = year4retlev,
                                                              do.ci = TRUE,
                                                              FUN="median",
                                                              burn=burn)
        if (length(year4retlev) == 1) {
          retlev_j[j,1] <- as.numeric(retlev_bay[1])
          retlev_j[j,2] <- as.numeric(retlev_bay[2])
          retlev_j[j,3] <- as.numeric(retlev_bay[3])
        } else {
          retlev_j[j,1:nyears] <- as.numeric(retlev_bay[,1])
          retlev_j[j,(nyears+1):(2*nyears)] <- as.numeric(retlev_bay[,2])
          retlev_j[j,(2*nyears+1):(3*nyears)] <- as.numeric(retlev_bay[,3])
        }
        # safe check
        if ( (any( !is.na(data) & data<0) & any( !is.na(retlev_j) & retlev_j<=0)) | any( is.na(retlev_j))) {
          location_j[j]  <- NA
          log_scale_j[j] <- NA
          shape_j[j]     <- NA
          retlev_j[j,1:nyears] <- rep(NA,nyears)
          retlev_j[j,(nyears+1):(2*nyears)] <- rep(NA,nyears) 
          retlev_j[j,(2*nyears+1):(3*nyears)] <- rep(NA,nyears)
        }
      }

    # regionalization or upscaling
    } else {
      if ( (p <- length( ix <- (which(nn2$nn.idx[i,]!=0)))) == 0) {
        return(c(NA,NA,NA,rep(NA,(nyears*3))))
      } else {
        if (is.na(sample_radius)) {
          # use this for reproducible results
          if (!is.na(randomseed)) set.seed(i)
          rand <- array( data=sample(x=p,size=(iter_reg*ntime),replace=T),
                         dim=c(iter_reg,ntime))
        } else {
          probs <- exp(-0.5*(nn2$nn.dists[i,ix])**2/sample_radius**2) / sum(exp(-0.5*(nn2$nn.dists[i,ix])**2/sample_radius**2))
          # use this for reproducible results
          if (!is.na(randomseed)) set.seed(i)
          rand <- array( data=sample(x=p,size=(iter_reg*ntime),replace=T,prob=probs),
                         dim=c(iter_reg,ntime))
        }
        #
        retlev_j <- array( data=NA, dim=c(iter_reg, (nyears*3)))
        location_j  <- log_scale_j <- shape_j <- vector(mode="numeric",length=iter_reg)
        # regionalization
        for (j in 1:iter_reg) {
          # 1st iteration, use the timeseries of the closest input point
          if (j == 1) {
            data <- as.vector(mat[nn2$nn.idx[i,1],])
            proposalParams_mean <- c(0,0,0)
            proposalParams_sd   <- c(0.5,0.5,0.1)
          # from the 2nd iteration, use resampled timeseries
          } else {
            for (k in 1:ntime) data[k] <- mat[nn2$nn.idx[i,ix[rand[j,k]]],k]
  #          proposalParams_mean <- c(location_j[j-1],log_scale_j[j-1],shape_j[j-1])
            proposalParams_mean <- c(0,0,0)
            proposalParams_sd   <- c(0.5,0.5,0.1)
          }
          if ((!any(data!=0)) | (max(abs(diff(data)))==0)) {
            location_j[j]  <- NA
            log_scale_j[j] <- NA
            shape_j[j]     <- NA
            retlev_j[j,1:nyears] <- rep(NA,nyears)
            retlev_j[j,(nyears+1):(2*nyears)] <- rep(NA,nyears) 
            retlev_j[j,(2*nyears+1):(3*nyears)] <- rep(NA,nyears)
          } else {
            if (any(is.na(proposalParams_mean))) proposalParams_mean <- c(0,0,0)
            # use this for reproducible results
            if (!is.na(randomseed)) set.seed(j)
  #          t0 <- Sys.time()
            par_bay <- fevd( data,
                             type="GEV", 
                             method="Bayesian", 
                             priorFun="fevdPriorShape",
                             iter=iter_bay,
                             proposalParams=list(mean=proposalParams_mean,
                                                 sd=proposalParams_sd))
            t1 <- Sys.time()
  #          print( paste( "time", round(t1-t0,1), attr(t1-t0,"unit")))
            location_j[j]  <-  mean(par_bay$results[(burn+1):iter_bay,1])
            log_scale_j[j] <-  mean(par_bay$results[(burn+1):iter_bay,2])
            shape_j[j]     <-  mean(par_bay$results[(burn+1):iter_bay,3])
            # Return levels
            retlev_bay <- return.level.fevd.bayesian_anyfunction( par_bay, 
                                                                  return.period = year4retlev,
                                                                  do.ci = TRUE,
                                                                  FUN="median",
                                                                  burn=burn)
            if (length(year4retlev) == 1) {
              retlev_j[j,1] <- as.numeric(retlev_bay[1])
              retlev_j[j,2] <- as.numeric(retlev_bay[2])
              retlev_j[j,3] <- as.numeric(retlev_bay[3])
            } else {
              retlev_j[j,1:nyears] <- as.numeric(retlev_bay[,1])
              retlev_j[j,(nyears+1):(2*nyears)] <- as.numeric(retlev_bay[,2])
              retlev_j[j,(2*nyears+1):(3*nyears)] <- as.numeric(retlev_bay[,3])
            }
            # safe check
            if ( (any( !is.na(data) & data<0) & any( !is.na(retlev_j[j,]) & retlev_j[j,]<=0)) | any( is.na(retlev_j[j,]))) {
              location_j[j]  <- NA
              log_scale_j[j] <- NA
              shape_j[j]     <- NA
              retlev_j[j,1:nyears] <- rep(NA,nyears)
              retlev_j[j,(nyears+1):(2*nyears)] <- rep(NA,nyears) 
              retlev_j[j,(2*nyears+1):(3*nyears)] <- rep(NA,nyears)
            }
          }
        } # end of regionalization loop
      } # end of if all data do have different values
    } # end of regionalization yes/no
    if (verbose) {
      t1 <- Sys.time()
      cat( paste( "i=", i, ":time", round(t1-t0,1), attr(t1-t0,"unit"), "\n"))
    }
    # first 3: GEV parameters;
    # next nyears: average values of 2.5-percentiles
    # next nyears: average values of best estimate of return levels 
    # next nyears: average values of 97.5-percentiles of return levels 
    # next nyears: standard deviations of 2.5-percentiles
    # next nyears: standard deviations of best estimate of return levels 
    # next nyears: standard deviations of 97.5-percentiles of return levels 
    return( c( median(location_j,na.rm=T),
               median(log_scale_j,na.rm=T),
               median(shape_j,na.rm=T),
               apply(retlev_j,MAR=2,FUN=median,na.rm=T),
               apply(retlev_j,MAR=2,FUN=function(x){diff(quantile(x,probs=c(0.25,0.75),na.rm=T))})))
  } # End of fitGEV_bayesian
}
