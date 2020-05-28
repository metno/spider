#+ check for holes in the field
spider_griddqc_cool <- function( argv=NULL, r=NULL) {
#------------------------------------------------------------------------------
  if ( is.null(argv))
    if ("argv" %in% ls(envir = .GlobalEnv)) 
      get("argv", envir = .GlobalEnv)
  if ( is.null(r))
    if ("r" %in% ls(envir = .GlobalEnv)) 
      get("r", envir = .GlobalEnv)
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
  r
}
