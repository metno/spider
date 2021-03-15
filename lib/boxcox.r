#+ Box-Cox transformation
boxcox<-function(x,lambda) {
  if (lambda==0) {
    return(log(x))
  } else {
    res <- x; res[] <- NA
    ix <- which( !is.na(x) & is.finite(x))
    res[ix] <- (x[ix]**lambda - 1) / lambda
    return( res)
  }
}

