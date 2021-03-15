#+
get_rad_stat <- function( i, x, y, val, dh=25000) {
#------------------------------------------------------------------------------
  if ( length( ix <- which( abs( x[i] - x) < dh)) == 0) 
    return( c(NA, NA))
  if ( length( iy <- which( abs( y[i] - y[ix]) < dh)) == 0) 
    return( c(NA, NA))
  val1 <- val[ix[iy]]
  return( c( mean( val1),
               sd( val1)))
}
