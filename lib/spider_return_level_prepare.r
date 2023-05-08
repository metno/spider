#+ prepare data for calculation of trend through time
spider_return_level_prepare <- function( argv = NULL, r = NULL) {
#------------------------------------------------------------------------------
  if ( is.null(argv))
    if ( "argv" %in% ls(envir = .GlobalEnv)) 
      argv <- get( "argv", envir = .GlobalEnv)
  if ( is.null(r))
    if ( "r" %in% ls(envir = .GlobalEnv)) 
      r <- get( "r", envir = .GlobalEnv)
  #
  n <- ncell(r)
  # vr: values of r (n-vector)
  vr   <- getValues(r)
  init <- vector( mode="numeric", length=n); init[] <- NA
  flag <- !is.na( vr) & !is.nan( vr) & is.finite( vr)
  # ix: pointer to elements of r that are not NAs and finite (m-vector)
  if ( length( ix <- which( flag)) == 0) return(NULL)
  m  <- length( ix)
  vr <- vr[ix]
  #
  # scores that require to store the whole dataset in memory
  if ( argv$return_level_elab %in% c("fitGEV_bayesian")) {
    return( list( online=F, ix=ix, n=length(ix), mat_col=vr))
  } else {
    return(NULL)
  } 
}

