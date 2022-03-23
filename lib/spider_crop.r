#+ 
spider_crop <- function() {
#------------------------------------------------------------------------------
  if ( argv$ffin_proj4 == argv$crop_proj4) {
    r <- crop( r,
               extent( argv$crop[1], argv$crop[2], 
                       argv$crop[3], argv$crop[4]))
    rmaster <- crop( rmaster,
                     extent( argv$crop[1], argv$crop[2], 
                             argv$crop[3], argv$crop[4]))
    if (!is.na(r_ref)) {
      r_ref <- crop( r_ref,
                     extent( argv$crop[1], argv$crop[2], 
                             argv$crop[3], argv$crop[4]))
    }
    if ( class( r_dem) != "logical") { 
      r_dem <- crop( r_dem,
                     extent( argv$crop[1], argv$crop[2], 
                             argv$crop[3], argv$crop[4]))
      rmaster_dem <- crop( rmaster_dem,
                           extent( argv$crop[1], argv$crop[2], 
                                   argv$crop[3], argv$crop[4]))
    }
  } else {
    coord.new<-spTransform( 
                SpatialPoints( rbind( c(argv$crop[1], argv$crop[3]),
                                      c(argv$crop[1], argv$crop[4]),
                                      c(argv$crop[2], argv$crop[3]),
                                      c(argv$crop[2], argv$crop[4])),
                               proj4string = CRS( argv$crop_proj4)) 
                                            ,CRS( argv$ffin_proj4))
    bbox <- attr( coord.new, "bbox")
    r <- crop( r,
               extent( bbox[1,1], bbox[1,2],
                       bbox[2,1], bbox[2,2]))
    rmaster <- crop( rmaster,
                     extent( bbox[1,1], bbox[1,2],
                             bbox[2,1], bbox[2,2]))
    if (!is.na(r_ref)) {
      r_ref <- crop( r_ref,
                     extent( bbox[1,1], bbox[1,2],
                             bbox[2,1], bbox[2,2])) 
    }

    if ( class( r_dem) != "logical") { 
      r_dem <- crop( r_dem,
                     extent( bbox[1,1], bbox[1,2],
                             bbox[2,1], bbox[2,2]))
      rmaster_dem <- crop( rmaster_dem,
                           extent( bbox[1,1], bbox[1,2],
                                   bbox[2,1], bbox[2,2]))
    }
  }
  if ( !any( !is.na( values<-getValues(r)))) {
    print( paste( "warning: all NAs after crop"))
    return(NULL)
  }
  return(list( r=r, r_dem=r_dem, r_ref=r_ref, rmaster=rmaster, rmaster_dem=rmaster_dem))
}
