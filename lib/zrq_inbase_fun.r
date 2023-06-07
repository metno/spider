#+
zrq_inbase_fun <- function(i) {
  vec <- array( data=NA, dim=c(nbaseyears-1,n_qtiles))
  for (i in 1:(nbaseyears-1)) {
    vec[i,] <- as.numeric( quantile( c(mat[i,ix_t],mat[i,ix_t[which(ix_t_years==years_loop[i])]]), probs=zrq_qtiles, type=8, na.rm=T))
  }
  as.vector(vec)
}
