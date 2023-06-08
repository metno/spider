#+
zrq_inbase_fun <- function(i) {
  if ((i%%100000)==0) {
    print(paste(i,"/",nix))
    t22 <- Sys.time()
    print( paste( "time", round(t22-t00,1), attr(t22-t00,"unit")))
  }
  vec <- array( data=NA, dim=c(nbaseyears-1,n_qtiles))
#  tmp <- array( data=NA, dim=c(nbaseyears-1,nbaseyears*(2*argv$zrq_ndays+1)))
  for (j in 1:(nbaseyears-1)) {
#    tmp[j,] <- c(mat[i,ix_t],mat[i,ix_t[which(ix_t_years==years_loop[j])]])
    vec[j,] <- as.numeric( quantile8_speed( c(mat[i,ix_t],mat[i,ix_t[which(ix_t_years==years_loop[j])]]), probs=zrq_qtiles, na.rm=T))
  }
#  as.vector( t(apply( tmp, MARGIN=1, FUN=function(x){quantile8_speed(x,probs=zrq_qtiles, na.rm=T)})))
  as.vector(vec)
}
