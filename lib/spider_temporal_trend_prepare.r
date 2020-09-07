#+ prepare data for calculation of trend through time
spider_temporal_trend_prepare <- function( argv   = NULL, 
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
  n <- ncell(r)
  # vr: values of r (n-vector)
  vr   <- getValues(r)
  init <- vector( mode="numeric", length=n); init[] <- NA
  flag <- !is.na( vr) & !is.nan( vr) & is.finite( vr)
  vref <- NULL
  if ( !is.na(argv$ffin_ref_template)) {
    vref <- getValues(r_ref)
    flag <- flag & !is.na( vref) & !is.nan( vref) & is.finite( vref)
  }
  # ix: pointer to elements of r that are not NAs and finite (m-vector)
  if ( length( ix <- which( flag)) == 0) return(NULL)
  m  <- length( ix)
  vr <- vr[ix]
  if ( !is.na(argv$ffin_ref_template)) vref <- vref[ix]
  # NOTE: vr and vref are now m-vectors
  #
  # scores that require to store the whole dataset in memory
  if ( argv$temporal_trend_elab %in% c("Theil_Sen_regression")) {
    return( list( online=F, ix=ix, n=length(ix),
                  mat_col=vr, mat_ref_col=vref ))
  #
  # scores that are computed online
  } else {
    # dat / dat_cont / dat_aggr: n-vectors
    dat_cont[ix] <- dat_cont[ix] + 1
    dat <- vector( mode="numeric", length=n); dat[] <- NA
    dat[ix] <- 0
    dat_aggr <- dat
    # update online score: end
    return( list( online=T, ix=ix, n=length(ix),
                  dat_aggr_up=dat_aggr, 
                  dat_cont_up=dat_cont ))
  }
}

