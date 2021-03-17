#+
spider_griddqc_outliers <- function( argv=NULL, r=NULL) {
#------------------------------------------------------------------------------
  if ( is.null(argv))
    if ("argv" %in% ls(envir = .GlobalEnv)) 
      argv <- get("argv", envir = .GlobalEnv)
  if ( is.null(r))
    if ("r" %in% ls(envir = .GlobalEnv)) 
      r <- get("r", envir = .GlobalEnv)

  # c. remove outliers. Check for outliers in square boxes
  t0   <- Sys.time()

  rval <- getValues(r)
  raux <- r

  values      <- rval
  values_mina <- pmin( pmax( values - 10, 0),
                       pmax( values - 1 * values, 0))
  values_maxa <- pmax( values + 10, values + 1 * values)
  values_minv <- pmin( pmax( values - 1, 0),
                       pmax( values - 0.1 * values, 0))
  values_maxv <- pmax( values + 1, values + 0.1 * values)

  # no data transformation for now
  argv$boxcox.lambda<-0.3
#  tvalues_mina <- boxcox( x=values_mina, lambda=argv$boxcox.lambda)
#  tvalues_maxa <- boxcox( x=values_maxa, lambda=argv$boxcox.lambda)
#  tvalues_minv <- boxcox( x=values_minv, lambda=argv$boxcox.lambda)
#  tvalues_maxv <- boxcox( x=values_maxv, lambda=argv$boxcox.lambda)
  tvalues  <- boxcox( x=values,  lambda=argv$boxcox.lambda)

  # compute mean and sd on a coarser grid

  raux[] <- tvalues

  raux_agg <- aggregate( raux,
                         fact  = argv$gridded_dqc.outlier_aggfact,
                         fun   = mean,
                         na.rm = T)
  rtmu <- disaggregate( raux_agg,
                        fact   = argv$gridded_dqc.outlier_aggfact,
                        method = "bilinear",
                        na.rm  = T)
  if ( ncell(rtmu) > ncell(r)) {
    rtmu <- crop( rtmu, r)
  } else if ( ncell( rtmu) < ncell(r)) {
    rtmu <- extend( rtmu, r)
  }
  tmu <- getValues( rtmu)
  
  raux_agg <- aggregate( raux,
                         fact  = argv$gridded_dqc.outlier_aggfact,
                         fun   = sd,
                         na.rm = T)
  rtsig <- disaggregate( raux_agg,
                         fact   = argv$gridded_dqc.outlier_aggfact,
                         method = "bilinear",
                         na.rm  = T)
  if ( ncell(rtsig) > ncell(r)) {
    rtsig <- crop( rtsig, r)
  } else if ( ncell( rtsig) < ncell(r)) {
    rtsig <- extend( rtsig, r)
  }
  tsig <- getValues( rtsig)

  raux[] <- values

  raux_agg <- aggregate( raux,
                         fact  = argv$gridded_dqc.outlier_aggfact,
                         fun   = mean,
                         na.rm = T)
  rmu <- disaggregate( raux_agg,
                       fact   = argv$gridded_dqc.outlier_aggfact,
                       method = "bilinear",
                       na.rm  = T)
  if ( ncell(rmu) > ncell(r)) {
    rmu <- crop( rmu, r)
  } else if ( ncell( rmu) < ncell(r)) {
    rmu <- extend( rmu, r)
  }
  mu <- getValues( rmu)

  # flag bad data
  flag1 <- is.na(values) | is.na(mu) | is.na(tsig)
  # outliers are defined as in Lanzante,1997: abs(value-mean)/st.dev > 5
  flag2 <- ( mu < values_minv | mu > values_maxv ) & 
           (( abs( tvalues - tmu) / tsig) > 5)
  flag3 <- mu < values_mina | mu > values_maxa
  suspect <- which( flag1 | flag2 | flag3)
  if ( length( suspect) > 0)
    rval[suspect] <- argv$gridded_dqc.outlier_pad
  r[] <- rval

  t1<-Sys.time()

  # verbose
  cat( paste( "   fraction of NAs=", round( length( which( flag1)) / length(values), 4),"\n"))
  cat( paste( " fraction outliers=", round( length( which( flag2 | flag3)) / length(values), 4),"\n"))
  cat( paste( "remove outliers - time", round( t1-t0,1),
                                         attr( t1-t0,"unit"), "\n"))
  r
}
