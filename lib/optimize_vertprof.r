#+ cost function used for optimization of vertprof parameter
opt_vertprof_basic <- function( par, 
                                vert_coord, 
                                gamma, 
                                obs,
                                sd = NA) {
#------------------------------------------------------------------------------
  if ( any( is.na(sd))) { sd<-vert_coord; sd[]<-1}
  pred <- tvertprof_basic( z     = vert_coord, 
                           t0    = par[1],
                           gamma = gamma)
  return( log( ( mean( (pred-obs)**2 / sd**2))**0.5))
}

#+ cost function used for optimization of vertprof parameter
opt_vertprof_Frei_2014 <- function( par, 
                                    vert_coord, 
                                    obs,
                                    sd = NA) {
#------------------------------------------------------------------------------
  if ( any( is.na(sd))) { sd<-vert_coord; sd[]<-1}
  pred <- tvertprof_Frei_2014( z     = vert_coord,
                               t0    = par[1],
                               gamma = par[2],
                               a     = par[3],
                               h0    = par[4],
                               h1i   = par[5])
  return( log( ( mean( (pred-obs)**2 / sd**2))**0.5))
}

