return.level.fevd.bayesian_anyfunction <- function (x, 
                                                    return.period = c(2, 20, 100), ..., 
                                                    do.ci = FALSE, 
                                                    burn.in = 499, 
                                                    FUN = "median", 
                                                    qcov = NULL, qcov.base = NULL) 
{
# Author: Julia Lutz (MET Norway)
  if (x$type == "PP") {
    mod2 <- "GEV"
  } else {
    mod2 <- x$type
  }
  f <- match.fun(FUN)
  p <- x$results
  np <- dim(p)[2] - 1
  p <- p[, 1:np]
  pnames <- colnames(p)
  if (FUN == "mean") { 
    p <- colMeans(p)
  } else {
    p <- apply(p, 2, f)
  }
  if (is.fixedfevd(x)) {
    if (is.element("log.scale", pnames)) {
      p["log.scale"] <- exp(p["log.scale"])
      pnames[pnames == "log.scale"] <- "scale"
      names(p) <- pnames
    }
    if (!do.ci) {
      if (all(is.element(c("location", "shape"), pnames))) {
        res <- rlevd(return.period, loc = p["location"], 
                     scale = p["scale"], shape = p["shape"], threshold = x$threshold, 
                     type = mod2, npy = x$npy, rate = x$rate)
      } else if (is.element("shape", pnames)) {
        res <- rlevd(return.period, scale = p["scale"], 
                     shape = p["shape"], threshold = x$threshold, 
                     type = mod2, npy = x$npy, rate = x$rate)
      } else if (is.element("location", pnames)) {
        res <- rlevd(return.period, loc = p["location"], 
                     scale = p["scale"], threshold = x$threshold, 
                     type = mod2, npy = x$npy, rate = x$rate)
      } else {
        res <- rlevd(return.period, scale = p["scale"], 
                     threshold = x$threshold, type = mod2, npy = x$npy, 
                      rate = x$rate)
      }
      attr(res, "return.period") <- return.period
      attr(res, "data.name") <- x$data.name
      attr(res, "fit.call") <- x$call
      attr(res, "call") <- match.call()
      attr(res, "fit.type") <- x$type
      attr(res, "data.assumption") <- "stationary"
      attr(res, "period") <- x$period.basis
      attr(res, "units") <- x$units
      attr(res, "class") <- "return.level"
    } else if (do.ci) { 
      res <- ci(x, return.period = return.period, FUN = FUN, ...)
    }
    if (!is.null(dim(res))) colnames(res)[2] <- paste("Posterior",FUN)
  } else {
    if (missing(return.period)) return.period <- 100
    res <- return.level.ns.fevd.bayesian(x = x, return.period = return.period, 
                                         ..., do.ci = do.ci, 
                                         qcov = qcov, qcov.base = qcov.base)
    if (do.ci) return(res)
    if (length(return.period) == 1) res <- matrix(res, ncol = 1)
    colnames(res) <- paste(return.period, "-", x$period.basis, 
                           " level", sep = "")
    attr(res, "return.period") <- return.period
    attr(res, "data.name") <- x$data.name
    attr(res, "fit.call") <- x$call
    attr(res, "call") <- match.call()
    attr(res, "fit.type") <- x$type
    attr(res, "data.assumption") <- "non-stationary"
    attr(res, "period") <- x$period.basis
    attr(res, "units") <- x$units
    if (is.null(qcov)) 
      attr(res, "qcov") <- x$data.name[2]
    else attr(res, "qcov") <- deparse(substitute(qcov))
    attr(res, "class") <- "return.level"
  }
  return(res)
}
