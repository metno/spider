#+ 
spider_point_mask <- function() {
#------------------------------------------------------------------------------
  if ( any( is.na( argv$point_mask_x)) | any( is.na( argv$point_mask_y)) |
       length( argv$point_mask_x) != length( argv$point_mask_y) |
       length( argv$point_mask_x) != length( argv$point_mask_labels) ) {
    print( "warning: something is wrong with the list of points")
    print( argv$point_mask_x)
    print( argv$point_mask_y)
    print( argv$point_mask_labels)
    print( any( is.na( argv$point_mask_x)))
    print( any( is.na( argv$point_mask_y)))
    print( length( argv$point_mask_x))
    print( length( argv$point_mask_y))
    print( length( argv$point_mask_labels))
    return( NULL)
  }
  if (argv$point_mask_proj4!=as.character(crs(r))) { 
    coord.new <- attr( spTransform( SpatialPoints(
      cbind( argv$point_mask_x,
             argv$point_mask_y),
             proj4string=CRS(argv$point_mask_proj4)),
             crs(r)), "coords")
    point_x <- coord.new[,1]
    point_y <- coord.new[,2]
  } else {
    point_x <- argv$point_mask_x
    point_y <- argv$point_mask_y
  }
  if ( !any( !is.na( values <- extract( r, cbind( point_x, point_y),
                                        method=argv$point_mask_method)))) {
    print(paste("warning: all NAs after point mask"))
    return( NULL)
  }
  return(values)
}
