#+
spider_downscale <- function() {
#------------------------------------------------------------------------------
  if ( !( argv$space_fun %in% c( "ngb", "bilinear"))) 
    boom( "--fun must be either \"ngb\" or \"bilinear\"")
  if ( compareCRS( crs(r), crs(rmaster))) {
    if ( rasters_match(r, rmaster)) {
      r1 <- r
    } else {
      r1 <- resample( r, rmaster, method=argv$space_fun)
    }
  } else {
    r1 <- projectRaster( r, rmaster, method=argv$space_fun)
  }
  if ( argv$master_mask) r1 <- mask( r1, mask=rmaster)
  r <- r1
  if ( !any( !is.na( values <- getValues(r)))) {
    print(paste("warning: all NAs after downscale"))
    return(NULL)
  }
  r
}
