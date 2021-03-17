#+
get_rad_stat <- function( i, x, y, val, dh_ref=25000) {
#------------------------------------------------------------------------------
#  require( RANN)
#
#  nn2 <- nn2( cbind( x, y),
#              query = cbind( x[i], y[i]),
#              k = 25,
#              searchtype = "radius", 
#              radius = dh_ref)
#
#  nnix <- nn2[[1]]

  if ( length( ix <- which( abs( x[i] - x) < dh_ref)) == 0) 
    return( c(NA, NA))
  if ( length( iy <- which( abs( y[i] - y[ix]) < dh_ref)) == 0) 
    return( c(NA, NA))

  val1 <- val[ix[iy]]

  return( c( mean( val1), sd( val1)))
}
