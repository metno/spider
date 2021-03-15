#+
spider_griddqc_outliers <- function( argv=NULL, r=NULL) {
#------------------------------------------------------------------------------
  if ( is.null(argv))
    if ("argv" %in% ls(envir = .GlobalEnv)) 
      get("argv", envir = .GlobalEnv)
  if ( is.null(r))
    if ("r" %in% ls(envir = .GlobalEnv)) 
      get("r", envir = .GlobalEnv)
  # c. remove outliers. Check for outliers in square boxes
  t0   <- Sys.time()
  raux <- r
  daux <- boxcox( x=rval, lambda=0.5)
  raux[] <- daux
  # compute mean and sd on a coarser grid
  raux_agg <- aggregate( raux,
                         fact  = argv$gridded_dqc.outlier_aggfact,
                         fun   = mean,
                         na.rm = T)
  daux_agg <- getValues(raux_agg)
  ix_aux   <- which(!is.na(daux_agg))
  xyaux    <- xyFromCell(raux_agg,ix_aux)
  xrad_aux <- xyaux[,1]
  yrad_aux <- xyaux[,2]
  vrad_aux <- daux_agg[ix_aux]
  if ( !is.na( argv$cores)) {
    arr <- mcmapply( get_rad_stat,
                     1:length(ix_aux),
                     mc.cores = argv$cores,
                     SIMPLIFY = T,
                     dh_ref   = argv$gridded_dqc.outlier_reflen,
                     x        = xrad_aux,
                     y        = yrad_aux,
                     val      = vrad_aux)
  # no-multicores
  } else {
    arr <- mapply(   get_rad_stat,
                     1:length(ix_aux),
                     SIMPLIFY = T,
                     dh_ref   = argv$gridded_dqc.outlier_reflen,
                     x        = xrad_aux,
                     y        = yrad_aux,
                     val      = vrad_aux)
  }
  # disaggregate mean and sd on the original grid
  raux_agg[]       <- NA
  raux_agg[ix_aux] <- arr[1,] # mean
  raux <- disaggregate( raux_agg,
                        fact   = argv$gridded_dqc.outlier_aggfact,
                        method = "bilinear",
                        na.rm  = T)
  if ( ncell(raux) > ncell(r)) {
    raux <- crop( raux, r)
  } else if ( ncell( raux) < ncell(r)) {
    raux <- extend( raux, r)
  }
  avg <- getValues(raux)
  raux_agg[] <- NA
  raux_agg[ix_aux] <- arr[2,] # sd
  raux <- disaggregate( raux_agg,
                        fact   = argv$gridded_dqc.outlier_aggfact,
                        method = "bilinear",
                        na.rm  = T)
  if ( ncell(raux) > ncell(r)) {
    raux <- crop( raux, r)
  } else if ( ncell(raux) < ncell(r)) {
    raux <- extend( raux, r)
  }
  stdev <- getValues( raux)
  # check for outliers
  ix <- which( stdev>0 & !is.na(daux) & !is.na(avg) & !is.na(stdev))
  # outliers are defined as in Lanzante,1997: abs(value-mean)/st.dev > 5
  suspect <- which( ( abs(daux[ix]-avg[ix]) / stdev[ix] ) > 5 ) 
  if ( length( suspect) > 0) 
    rval[ix[suspect]] <- argv$gridded_dqc.outlier_pad
  r[] <- rval
  t1<-Sys.time()
  print( paste( " remove outliers - time", round(t1-t0,1),
                                           attr( t1-t0,"unit")))
  r
}

