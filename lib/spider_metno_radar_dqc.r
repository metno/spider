#+
spider_metno_radar_dqc <- function( ffin, t_to_read) {
#------------------------------------------------------------------------------
  var_dqcrad <- c( "is_nodata",
                   "is_blocked",
                   "is_seaclutter",
                   "is_groundclutter",
                   "is_otherclutter")
  nv_dqcrad <- length(var_dqcrad)
  for (v in 1:nv_dqcrad) {   
    u <- read_griddeddata( mode="data", var=var_dqcrad[v], 
                           ffin=ffin, t_to_read=t_to_read)
    if (is.null(u)) {
      print(paste("warning: problem reading radar dqc var=",var_dqcrad[v]))
      return( NULL)
    }
    if ( !any( !is.na( values_u <- getValues(u)))) {
      print(paste("warning: all NAs for  radar dqc var=",var_dqcrad[v]))
      return( NULL)
    }
    r[ which( getValues(u)==1)]<-NA
  } # end for v
  if ( !any( !is.na( values <- getValues(r)))) {
    print(paste("warning: all NAs after radar dqc"))
    return( NULL)
  }
  r
}
