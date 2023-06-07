#+ prepare data for calculation of zrq
spider_zrq_prepare <- function( argv = NULL, r = NULL) {
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
  ix <- ix[1:700000]
  m  <- length( ix)
  vr <- vr[ix]
  #
  return( list( online=F, ix=ix, n=length(ix), mat_col=vr))
}

