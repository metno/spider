quantile8_speed4boot <- function(x, indices, probs = c(0.1, 0.9), na.rm = F) {

  # required by boot function
  x <- x[indices]

  if ( length( ix <- which(!is.na(x))) == 0) return( rep(NA,length(probs)))

  # remove NAs if requested
  if (na.rm) x <- x[ix]

  n <- length(x)
  index <- 1/3 + (n + 1/3) * probs

  lo    <- floor(index)
  hi    <- ceiling(index)
  if ( length( ix <- which(lo<1)) > 0) lo[ix] <- 1
  if ( length( ix <- which(hi>n)) > 0) hi[ix] <- n

  x  <- sort(x, partial = unique(c(lo, hi)))
  qs <- x[lo]

  i     <- 1:length(probs)
  h     <- index - lo
  qs    <- (1 - h) * qs + h * x[hi]
  qs

}
