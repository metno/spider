#+ check if two raster grids coincide
rasters_match <- function( r1, r2) {
#------------------------------------------------------------------------------
 return( compareCRS( crs(r1), crs(r2)) &
         extent(r1) == extent(r1)      &
         ncell(r1)  == ncell(r2)       &
         res(r1)    == res(r2))

}
