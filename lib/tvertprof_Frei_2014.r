#+ vertical profile of temperature (Frei, 2014)
tvertprof_Frei_2014 <- function( z, 
                                 t0, 
                                 gamma, 
                                 a, 
                                 h0,
                                 h1i) {
# ref:
# Frei, C. (2014). Interpolation of temperature in a mountainous region 
#  using nonlinear profiles and non‐Euclidean distances.
#  International Journal of Climatology, 34(5), 1585-1605.
# --< Input >--
#  z     = array/numeric. elevations [m amsl]
#  t0    = array/numeric. temperature at z=0 [K or degC]
#  gamma = array/numeric. temperature lapse rate [K/m]
#  a     = array/numeric. inversion strength  [K]
#  h0    = array/numeric. z where inversion starts [m]
#  h1i   = array/numeric. h0+h1i is z where inversion stops [m]
#       (Frei uses h1 directly, I use an increment to h0 so to avoid ending
#        up with h1<=h0 during the optimization)
# --< Output >--
#  res   = array/numeric. temperature [K or degC]
#------------------------------------------------------------------------------
  res   <- z
  res[] <- NA
  if (    length(t0) != length(z)) 
    { aux<-z; aux[]<-NA; aux[]<-t0[1]; t0<-aux}
  if ( length(gamma) != length(z)) 
    { aux<-z; aux[]<-NA; aux[]<-gamma[1]; gamma<-aux}
  if (     length(a) != length(z)) 
    { aux<-z; aux[]<-NA; aux[]<-a[1]; a<-aux}
  if (    length(h0) != length(z)) 
    { aux<-z; aux[]<-NA; aux[]<-h0[1]; h0<-aux}
  if (   length(h1i) != length(z)) 
    { aux<-z; aux[]<-NA; aux[]<-h1i[1]; h1i<-aux}
  if ( any(h1i==0 & !is.na(h1i))) h1i[h1i==0 & !is.na(h1i)] <- 0.1
  h1   <- h0 + abs( h1i)
  flag <- !is.na(z) & !is.na(t0) & !is.na(gamma) & !is.na(a) & !is.na(h0) & !is.na(h1i)
  z.le.h0 <- which( z <= h0 & flag )
  z.ge.h1 <- which( z >= h1 & flag )
  z.in    <- which( z >  h0 & z < h1 & flag)
  if ( length(z.le.h0) > 0) 
    res[z.le.h0] <- t0[z.le.h0] + gamma[z.le.h0] * z[z.le.h0] - a[z.le.h0] 
  if ( length(z.ge.h1) > 0) 
    res[z.ge.h1] <- t0[z.ge.h1] + gamma[z.ge.h1] * z[z.ge.h1] 
  if ( length(z.in) > 0)
   res[z.in] <- t0[z.in] + gamma[z.in] * z[z.in] - 
                a[z.in]/2 * ( 1 + cos( pi * ( z[z.in] -h0[z.in]) / (h1[z.in]-h0[z.in])))
  return(res)
}

