#+
rqb_fun <- function( i, nboot=100) {
#  if ((i%%100000)==0) print(paste(i,"/",nix))
  if ((i%%100000)==0) { t11 <- Sys.time(); print(paste(i, "/", nix, "time", round(t11-t00,1), attr(t11-t00,"unit")))}
  dat <- as.numeric(mat[i,])
  res <- boot( data=dat, statistic=quantile8_speed4boot, na.rm=T, probs=rqb_qtiles, R=nboot)
  mean <- as.numeric( apply( res$t, MAR=2, FUN=mean, na.rm=T))
  sd   <- as.numeric( apply( res$t, MAR=2, FUN=sd, na.rm=T))
  c(mean,sd)
}
