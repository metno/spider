#+
zrq_outbase_fun <- function(i) {
  as.numeric( quantile( mat[i,ix_t], probs=zrq_qtiles, type=8, na.rm=T))
}
