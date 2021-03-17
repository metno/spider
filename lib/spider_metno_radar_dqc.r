#+
spider_metno_radar_dqc <- function( ffin, t_to_read) {
#------------------------------------------------------------------------------
  var_dqcrad <- c( "block_percent",
                   "is_seaclutter",
                   "is_groundclutter",
                   "is_otherclutter",
                   "is_lowele",
                   "is_highele")
  thr  <- c(    20,    1,    1,    1,    0,    1)
  cond <- c( "geq", "eq", "eq", "eq", "eq", "eq") 
  nv_dqcrad<-length(var_dqcrad)
  for (v in 1:nv_dqcrad) {   
    u<-read_griddeddata( "data",
                         var=var_dqcrad[v],
                         ffin=ffin,
                         t_to_read=t_to_read)
    if (is.null(u)) {
      print(paste("warning: problem reading radar dqc var=",var_dqcrad[v]))
      next
    }
    if (!any(!is.na(values_u<-getValues(u)))) {
      print(paste("warning: all NAs for  radar dqc var=",var_dqcrad[v]))
      next
    }
    if ( cond[v] == "eq") {
      r[which(getValues(u) == thr[v])] <- NA
    } else if ( cond[v] == "geq") {
      r[which(getValues(u) >= thr[v])] <- NA
    } else if ( cond[v] == "leq") {
      r[which(getValues(u) <= thr[v])] <- NA
    } else if ( cond[v] == "gt") {
      r[which(getValues(u) > thr[v])] <- NA
    } else if ( cond[v] == "lt") {
      r[which(getValues(u) < thr[v])] <- NA
    }
  } # end for v
  if (!any(!is.na(values<-getValues(r)))) {
    print(paste("warning: all NAs after radar dqc"))
    next
  }
  r
}
