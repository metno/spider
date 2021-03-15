#+ vertical profile of temperature (linear)
tvertprof_basic <- function( z, 
                             t0,
                             gamma) {
# input
#  z= array. elevations [m amsl]
#  t0= numeric. temperature at z=0 [K or degC]
#  gamma=numeric. temperature lapse rate [K/m]
# Output
#  t= array. temperature [K or degC]
#------------------------------------------------------------------------------
  return( t0 + gamma * z)
}

