#+
spider_verif_grid_prepare <- function( argv   = NULL, 
                                       r      = NULL,
                                       r_ref  = NULL,
                                       dat_mean = NULL,
                                       dat_cont = NULL) {
#------------------------------------------------------------------------------
  if ( is.null(argv))
    if ( "argv" %in% ls(envir = .GlobalEnv)) 
      get( "argv", envir = .GlobalEnv)
  if ( is.null(r))
    if ( "r" %in% ls(envir = .GlobalEnv)) 
      get( "r", envir = .GlobalEnv)
  if ( is.null(r_ref))
    if ( "r_ref" %in% ls(envir = .GlobalEnv)) 
      get( "r_ref", envir = .GlobalEnv)
  if ( is.null(dat_mean))
    if ( "dat_mean" %in% ls(envir = .GlobalEnv)) 
      get( "dat_mean", envir = .GlobalEnv)
  if ( is.null(dat_cont))
    if ( "dat_cont" %in% ls(envir = .GlobalEnv)) 
      get( "dat_cont", envir = .GlobalEnv)
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
  if ( argv$verif_metric %in% c("corr","msess","ets","a","b","c","d",
                                "seeps","roblinreg","quantile")) {
    return( list( online=F, ix=ix, n=length(ix),
                  mat_col=vr, mat_ref_col=vref ))
  # scores that are computed online
  } else {
    if ( argv$verif_metric %in% c("mbias","rmsf") )
      vr <- vr / vref
    if ( argv$verif_metric %in% c("bias","mae","rmse") )
      vr <- vr - vref
    if ( length( ix1 <- which( !is.na( vr) & !is.nan( vr) & is.finite( vr) )) == 0) return(NULL)   
    dat <- vr[ix1]
    ix  <- ix[ix1]
    if ( argv$verif_metric == "mae" ) 
      dat <- abs( dat)
    if ( argv$verif_metric %in% c( "rmse", "rmsf") ) 
      dat <- dat**2
    dat_cont[ix] <- dat_cont[ix] + 1
    if ( any( is.na( dat_cont[ix]))) {
      ix1 <- is.na( dat_cont[ix])
      dat_cont[ix][ix1] <- 1
      dat_mean[ix][ix1] <- dat[ix1]
    }
    dat_mean[ix] <- dat_mean[ix] + ( dat - dat_mean[ix]) / dat_cont[ix]
    return( list( online=T, ix=ix, n=length(ix),
                  dat_mean_up=dat_mean, dat_cont_up=dat_cont ))
  }
}

