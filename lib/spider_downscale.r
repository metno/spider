#+
spider_downscale <- function() {
#------------------------------------------------------------------------------
  if ( !( argv$space_fun %in% c( "ngb", "bilinear"))) 
    boom( "spider_downscale: --fun must be either \"ngb\" or \"bilinear\"")
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


#  coord.master<-xyFromCell(rmaster,1:ncell(rmaster))
#  values<-getValues(r)
#  ix<-which(!is.na(values))
#  if ( compareCRS( crs(r), crs(rmaster))) {
#    coord.new<-xyFromCell(r,ix)
#  } else {
#    coord.new<-spTransform(
#                SpatialPoints(xyFromCell(r,ix),
#                               proj4string=CRS(argv$ffin_proj4))
#                                          ,CRS(argv$ffmaster_proj4))
#  }
#
#  if (!is.na(argv$cores)) {
#    res <- mcmapply( precise_fun,
#                     1:ndat,
#                     mc.cores = argv$cores,
#                     SIMPLIFY = T,
#                     fun = "mean")
#  # no-multicores
#  } else {
#    res <- mapply( precise_fun,
#                   1:ndat,
#                   SIMPLIFY = T,
#                   fun = "mean")
#  }

