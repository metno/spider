# 
#------------------------------------------------------------------------------
latte_express_step2 <- function( i, # <- index over tar
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
  w <- exp(-0.5* ((xma_tar[i]-xag_tar)**2+(yma_tar[i]-yag_tar)**2) / dh_ref**2)
  ix <- which( (w/sum(w)) > w_min)
  n  <- length(ix)
  vals <-1/sum(w[ix]) * sum( w[ix] * (
          tvertprof_Frei_2014( z=rep( zma_tar[i], n), t0=par[ix,1], gamma=par[ix,2], 
                     a=par[ix,3], h0=par[ix,4], h1i=par[ix,5]) - 
          tvertprof_Frei_2014( z=rep( zbilma_tar[i], n), t0=par[ix,1], gamma=par[ix,2], 
                     a=par[ix,3], h0=par[ix,4], h1i=par[ix,5]) + vbilma_tar[i]))
  vals
}



