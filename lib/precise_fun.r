#+ 
precise_fun<-function( i,
                       nmin  = 2,
                       n_x_x = 50,
                       eps2  = 0.01,
                       fun   = "mean",
                       range_val = c(0,1000)) {
#------------------------------------------------------------------------------
  if ( (n_y_x <- length( ix<- which( !is.na( dat[i,])))) < nmin ) {
    res <- rep( NA, n_tseq_out)
  } else {
    yo  <- dat[i,ix]
    if ( !any( yo!=0)) {
      res <- rep( 0, n_tseq_out)
    } else {
      yb  <- rep( mean(yo), n_y_x)
      xb  <- rep( mean(yo), n_x_x)
      y_x <- tprec_in[ix]
      x_x <- seq( range( y_x, na.rm=T)[1], range( y_x, na.rm=T)[2], length=n_x_x)
      dx  <- mean( diff(y_x), na.rm=T)/2
      S <- exp( -0.5 * outer( y_x, y_x, FUN="-")**2 / dx**2)
      G <- exp( -0.5 * outer( x_x, y_x, FUN="-")**2 / dx**2)
      SRinv <- try( chol2inv( chol( (S + diag( eps2, nrow=n_y_x, ncol=n_y_x)))))
      # slower alternative
      if ( !is.null( attr( SRinv, "class"))) 
            SRinv <- solve( S + diag( eps2, nrow=n_y_x, ncol=n_y_x))
      xa <- xb + tcrossprod( tcrossprod( G, SRinv), t(yo-yb))
      if ( any( !is.na( range_val))) {
        xa[ xa < range_val[1]] <- range_val[1]
        xa[ xa > range_val[2]] <- range_val[2]
      }
      res <- vector( mode="numeric", length= n_tseq_out)
      if (fun == "mean") {
        for (t in 1:n_tseq_out) 
          res[t] <- mean( xa[ x_x >= tprec_out[t] & 
                              x_x <= tprec_out[t+1]], na.rm=T)
      }
    }
  }
  res
}
