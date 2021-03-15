#+ 
spider_mask <- function() {
#------------------------------------------------------------------------------
  if ( argv$ffin_proj4 == argv$ffmaster_proj4) {
    r <- mask( r, mask = rmaster)
  } else {
    coord.new<-spTransform( 
                SpatialPoints( rbind( c(argv$crop[1], argv$crop[3]),
                                      c(argv$crop[1], argv$crop[4]),
                                      c(argv$crop[2], argv$crop[3]),
                                      c(argv$crop[2], argv$crop[4])),
                               proj4string = CRS( argv$crop_proj4)) 
                                            ,CRS( argv$ffin_proj4))
    bbox <- attr( coord.new, "bbox")
    r<-crop(r,
            extent( bbox[1,1], bbox[1,2], bbox[2,1], bbox[2,2]))
  }
  if ( !any( !is.na( values<-getValues(r)))) {
    print( paste( "warning: all NAs after crop"))
    return(NULL)
  }
  r
}
