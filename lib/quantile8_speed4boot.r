quantile8_speed4boot <- function(x, indices, probs = c(0.1, 0.9), na.rm = F) {

  # required by boot function
  x <- x[indices]

  # remove NAs if requested
  if (na.rm) x <- x[!is.na(x)]

  n <- length(x)
  index <- 1/3 + (n + 1/3) * probs

  lo    <- max( c( 1, floor(index)))
  hi    <- min( c( n, ceiling(index)))

  x  <- sort(x, partial = unique(c(lo, hi)))
  qs <- x[lo]

  i     <- 1:length(probs)
  h     <- index - lo
  qs    <- (1 - h) * qs + h * x[hi]
  qs

}
