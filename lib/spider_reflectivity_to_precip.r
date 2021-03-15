#+
spider_reflectivity_to_precip <- function() {
#------------------------------------------------------------------------------
  r <- (10**(r/10) / 200)**(5/8)
  if ( !any( !is.na( values <- getValues(r)))) {
    print( paste( "warning: all NAs after transforming reflectivity to precip"))
    return( NULL)
  }
  r
}

