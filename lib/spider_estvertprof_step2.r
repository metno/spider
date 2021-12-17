# 
#------------------------------------------------------------------------------
spider_estvertprof_step2 <- function( i, # <- index over tar
                                 xma_tar,
                                 yma_tar,
                                 zma_tar,
                                 xag_tar,
                                 yag_tar,
                                 par,
                                 zbilma_tar,
                                 vbilma_tar,
                                 dh_ref=25000,
                                 w_min=0.00001) {
#------------------------------------------------------------------------------
#  if (i%%10000==0) print(i)
  w   <- exp(-0.5* ((xma_tar[i]-xag_tar)**2+(yma_tar[i]-yag_tar)**2) / dh_ref**2)
  ix  <- which( (w/sum(w)) > w_min)
  aux <- 1/sum( w[ix]) 
  return( aux * c( sum( w[ix] * par[ix,1]), sum( w[ix] * par[ix,2])))
}



