#+ check if two raster grids coincide
rasters_match <- function( r1, r2) {
#------------------------------------------------------------------------------
ex1 <- as.numeric(as.vector(extent(r1)))
ex2 <- as.numeric(as.vector(extent(r2)))
 return( compareCRS( crs(r1), crs(r2)) &
         ex1[1] == ex2[1]      &
         ex1[2] == ex2[2]      &
         ex1[3] == ex2[3]      &
         ex1[4] == ex2[4]      &
         ncell(r1)  == ncell(r2)       &
         res(r1)[1] == res(r2)[1]      &
         res(r1)[2] == res(r2)[2] )

}
