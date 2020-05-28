#+
spider_polygon_mask <- function() {
#------------------------------------------------------------------------------
  if ( !file.exists( argv$ffin_polygon_shp)) {
    print( paste( "warning: file not found", argv$ffin_polygon_shp))
    return( NULL)
  }
  poly <- suppressWarnings( suppressMessages( 
           readOGR( argv$ffin_polygon_shp, argv$polygon_layer,
                    stringsAsFactors=F, verbose=F)))
  if ( as.character(poly@proj4string) != as.character(crs(r))) 
    poly <- spTransform( poly, crs(r))
  if ( any( !is.na( argv$polygon_ids))) {
    pix <- which( names(poly@data) == argv$polygon_data_field)
    if ( length(pix) == 0) 
      boom( paste("Error while reading shapefile",argv$ffin_polygon_shp,
                  "layer",argv$polygon_layer,
                  "data field",argv$polygon_data_field,
                  ", please use one of",toString(names(poly@data))))
    if ( length( ix <- which( poly@data[,pix] %in% 
                              as.character(argv$polygon_ids)))>0) {
      poly<-poly[ix,]
    } else {
      print("warning: the shapefile doe not contain the dpscified IDs. Skip this timestep")
      return( NULL)
    }
  } 
  # mask out considering the polygons
  r <- mask( r, poly)
  #
  if ( !any( !is.na( values <- getValues(r)))) {
    print(paste("warning: all NAs after polygon mask"))
    return( NULL)
  }
  #
  r
}
