#+
spider_gridclimind_prepare <- function( argv   = NULL, 
                                        r      = NULL,
                                        r_ref  = NULL,
                                        dat_aggr = NULL,
                                        dat_cont = NULL) {
#------------------------------------------------------------------------------
  if ( is.null(argv))
    if ( "argv" %in% ls(envir = .GlobalEnv)) 
      argv <- get( "argv", envir = .GlobalEnv)
  if ( is.null(r))
    if ( "r" %in% ls(envir = .GlobalEnv)) 
      r <- get( "r", envir = .GlobalEnv)
  if ( is.null(r_ref))
    if ( "r_ref" %in% ls(envir = .GlobalEnv)) 
      r_ref <- get( "r_ref", envir = .GlobalEnv)
  if ( is.null(dat_aggr))
    if ( "dat_aggr" %in% ls(envir = .GlobalEnv)) {
      dat_aggr <- get( "dat_aggr", envir = .GlobalEnv)
    } else {
      dat_aggr   <- vector( length=ncell(r), mode="numeric")
      dat_aggr[] <- NA
    }
  if ( is.null(dat_cont))
    if ( "dat_cont" %in% ls(envir = .GlobalEnv)) {
      dat_cont <- get( "dat_cont", envir = .GlobalEnv)
    } else {
      dat_cont   <- vector( length=ncell(r), mode="numeric")
      dat_cont[] <- NA
    }
  #
  vr   <- getValues(r)
  nvr   <- length(vr)
  init <- vector( mode="numeric", length=nvr); init[] <- NA
  flag <- !is.na( vr) & !is.nan( vr) & is.finite( vr)
  vref <- NULL
  if ( !is.na(argv$ffin_ref_template)) {
    vref <- getValues(r_ref)
    flag <- flag & !is.na( vref) & !is.nan( vref) & is.finite( vref)
  }
  if ( length( ix <- which( flag)) == 0) return(NULL)
  vr   <- vr[ix]
  if ( !is.na(argv$ffin_ref_template)) vref <- vref[ix]
  # scores that require to store the whole dataset in memory
  if ( argv$gridclimind_index %in% c("quantile")) {
    return( list( online=F, ix=ix, n=length(ix),
                  mat_col=vr, mat_ref_col=vref ))
  # scores that are computed online
  } else {
    if ( argv$gridclimind_index == "degree_days_sum" ) {
      if ( is.na(argv$degday_r)) {
        ixb <- 1:length(vr)
      } else {
        if ( argv$degday_b == "below") {       ixb <- which( vr <  argv$degday_r) }
        else if ( argv$degday_b == "below=") { ixb <- which( vr <= argv$degday_r) }
        else if ( argv$degday_b == "above")  { ixb <- which( vr >  argv$degday_r) }
        else if ( argv$degday_b == "above=") { ixb <- which( vr >= argv$degday_r) }
      }
      vr_aux <- vr; vr_aux[] <- 0
      if ( length(ixb) > 0) vr_aux[ixb] <- vr[ixb] - argv$degday_r 
      vr <- vr_aux
    } else if ( argv$gridclimind_index == "degree_days" ) {
      if ( is.na(argv$degday_r)) {
        ixb <- 1:length(vr)
      } else {
        if ( argv$degday_b == "below") {       ixb <- which( vr <  argv$degday_r) }
        else if ( argv$degday_b == "below=") { ixb <- which( vr <= argv$degday_r) }
        else if ( argv$degday_b == "above")  { ixb <- which( vr >  argv$degday_r) }
        else if ( argv$degday_b == "above=") { ixb <- which( vr >= argv$degday_r) }
      }
      vr_aux <- vr; vr_aux[] <- 0
      if ( length(ixb) > 0) vr_aux[ixb] <- 1 
      vr <- vr_aux
    } else if ( argv$gridclimind_index == "prcptot" ) {
      if ( is.na(argv$prcptot_r)) {
        ixb <- 1:length(vr)
      } else {
        if ( argv$prcptot_b == "below") {       ixb <- which( vr <  argv$prcptot_r) }
        else if ( argv$prcptot_b == "below=") { ixb <- which( vr <= argv$prcptot_r) }
        else if ( argv$prcptot_b == "above")  { ixb <- which( vr >  argv$prcptot_r) }
        else if ( argv$prcptot_b == "above=") { ixb <- which( vr >= argv$prcptot_r) }
      }
      vr_aux <- vr; vr_aux[] <- 0
      if ( length(ixb) > 0) vr_aux[ixb] <- vr[ixb]
      vr <- vr_aux
    }
    if ( length( ix1 <- which( !is.na( vr) & !is.nan( vr) & is.finite( vr) )) == 0) return(NULL)   
    dat <- vr[ix1]
    ix  <- ix[ix1]
    dat_cont[ix] <- dat_cont[ix] + 1
    if ( any( is.na( dat_cont[ix]))) {
      ix1 <- is.na( dat_cont[ix])
      dat_cont[ix][ix1] <- 1
      dat_aggr[ix][ix1] <- dat[ix1]
    }
    if ( argv$gridclimind_index %in% c( "degree_days_sum", "degree_days",
                                        "prcptot") ) {
      dat_aggr[ix] <- dat_aggr[ix] + dat
    } else {
      dat_aggr[ix] <- dat_aggr[ix] + ( dat - dat_aggr[ix]) / dat_cont[ix]
    }
    return( list( online=T, ix=ix, n=length(ix),
                  dat_aggr_up=dat_aggr, 
                  dat_cont_up=dat_cont ))
  }
}

