#+ check for holes in the field
spider_griddqc_cool <- function( argv=NULL, r=NULL) {
#------------------------------------------------------------------------------
  t00 <- Sys.time()
  if ( is.null( argv))
    if ("argv" %in% ls( envir = .GlobalEnv)) 
      argv <- get( "argv", envir = .GlobalEnv)
  if ( is.null(r))
    if ( "r" %in% ls( envir = .GlobalEnv)) 
      r <- get("r", envir = .GlobalEnv)
  # 
  rval <- getValues(r)
  suppressPackageStartupMessages( library( "igraph"))
  for (i in 1:length(argv$gridded_dqc.clump_r)) {
    raux <- r
    if ( any( rval <= argv$gridded_dqc.clump_r[i])) 
      raux[ which( rval <= argv$gridded_dqc.clump_r[i])] <- NA
    rclump <- clump(raux)
    fr     <- freq(rclump)
    ix     <- which( !is.na(fr[,2]) & 
                     fr[,2]<=argv$gridded_dqc.clump_n[i] )
    if ( length(ix) > 0) {
      rval[ getValues(rclump) %in% fr[ix,1]] <- argv$gridded_dqc.clump_pad[i]
      r[] <- rval
    }
  }
  t11 <- Sys.time()
  print( paste( "spider_griddqc_cool",
                "/ time", round(t11-t00,1), attr(t11-t00,"unit")))
  r
}
