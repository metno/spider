#+
rqb_fun <- function(i) {
#  if ((i%%100000)==0) print(paste(i,"/",nix))
  if ((i%%100000)==0) { t11 <- Sys.time(); print(paste(i, "/", nix, "time", round(t11-t00,1), attr(t11-t00,"unit")))}
  dat <- as.numeric(mat[i,])
  res <- boot( data=dat, statistic=quantile8_speed4boot, na.rm=T, probs=rqb_qtiles, R=1000)
  as.numeric(res$t0)
#  as.numeric( boot( data=as.numeric(mat[i,]), statistic=quantile8_speed4boot, na.rm=T, probs=rqb_qtiles, R=1000)$t0)
}
