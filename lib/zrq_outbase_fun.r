#+
zrq_outbase_fun <- function(i) {
  if ((i%%100000)==0) print(paste(i,"/",nix))
  as.numeric( quantile8_speed( mat[i,], probs=zrq_qtiles, na.rm=T))
}
