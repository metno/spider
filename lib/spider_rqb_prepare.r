#+ prepare data for calculation of rqb
spider_rqb_prepare <- function( argv = NULL, r = NULL) {
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
  flag <- !is.na( vr) & !is.nan( vr) & is.finite( vr) & abs(vr)<1000000
  # ix: pointer to elements of r that are not NAs and finite (m-vector)
  if ( length( ix <- which( flag)) == 0) return(NULL)
  if (!is.na(argv$rqb_m1) & !is.na(argv$rqb_m2)) 
    ix <- ix[argv$rqb_m1:argv$rqb_m2]
  m  <- length( ix)
  vr <- vr[ix]
  if (!is.na(argv$rqb_r)) {
    if (argv$rqb_b == "below") {
      ixna <- which( vr >= argv$rqb_r)
    } else if (argv$rqb_b == "below=") {
      ixna <- which( vr > argv$rqb_r)
    } else if (argv$rqb_b == "above") {
      ixna <- which( vr <= argv$rqb_r)
    } else if (argv$rqb_b == "above=") {
      ixna <- which( vr < argv$rqb_r)
    }
    if (length(ixna)>0) vr[ixna] <- NA
  }
  #
  return( list( online=F, ix=ix, n=length(ix), mat_col=vr))
}

