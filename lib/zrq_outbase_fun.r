#+
zrq_outbase_fun <- function(i) {
  if ((i%%100000)==0) print(paste(i,"/",nix))
  as.numeric( quantile( mat[i,ix_t], probs=zrq_qtiles, type=8, na.rm=T))
}
