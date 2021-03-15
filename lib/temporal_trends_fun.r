#+ verification scores and summary statistics 
temporal_trends_fun <- function( i     = NA,
                                 x     = NA,
                                 x_ref = NA,
                                 lab   = "Theil_Sen_regression") {
#------------------------------------------------------------------------------
# Estimate temporal trends extracted from gridded datasets and assess the statistical significance of the trends.
# Trends are computed separately for each grid point. The method implemented is the Theil-Sen (median-slope) regression as described by Wilks (2019) p. 283.
# Are the trends significant with respect to the hypothesis of no trend in the data? We face the problem of multiplicity for independent tests (Wilks, 2019). Answer to this question is obtained by applying Mann-Kendall trend test (Wilks, 2019, p. 178) to the timeseries at each grid point. Then, the problem of multiplicity for independent tests is solved as described by Wilks (2019) p. 195. A Benjamini-Hochberg meta-test is performed with a predefined global test level (or critical false discovery rate).
#------------------------------------------------------------------------------
  # transform x and x_ref into 1D mat and mat_ref
  if (is.na(i)) {
    i<-1
    if (exists("x")) mat<-array(data=x,dim=c(1,length(x)))
    if (!any(!is.na(mat))) return(NA)
    if (exists("xref")) {
      mat_ref<-array(data=x_ref,dim=c(1,length(x_ref)))
      if (!any(!is.na(mat_ref))) return(NA)
    }
  }
  # 
  res <- c( NA, NA, NA)
  t <- which( !is.na( mat[i,]) & is.finite( mat[i,]))
  if ( (nt <- length(t)) < 2) return( NA, NA, nt)
  data <- mat[i,t]
  if ( lab == "Theil_Sen_regression") {
    # REF: Wilks (2019) p. 283
    bnum <- outer( data, data, FUN="-")
    bden <- outer(    t,    t, FUN="-")
    bset <- bnum / bden
    b <- median( bset[row(bset)>col(bset) & is.finite(bset)])
    if ( !(!is.finite(b) | is.na(b) | is.null(b))) {
      residuals <- data - b * t
      a <- median( residuals)
      res <- c( a, b)
    }
  } else if ( lab == "Mann_Kendall_trend_test") {
    # REF: Wilks (2019) p. 178
    # S, test statistic
    aux <- sign( outer( data, data, FUN="-"))
    S <- sum( aux[row(aux)>col(aux) & is.finite(aux)])
    # varS, variance of the sampling sidtribution of S
    ng <- length( uni <- unique( data))
    varS <- nt * ( nt - 1) * ( 2 * nt + 5) / 18
    if ( ng != nt) {
      minus <- function( m, x, y) {
         if ( (l <- length( which( x == y[m]))) == 1) {
           return( 0)
         } else {
           return( l * ( l - 1) * ( 2 * l + 5) / 18)
         }
      }
      val <- mapply( minus,
                     1:ng,
                     SIMPLIFY = T, 
                     MoreArgs = list( x = data, y = uni))
      varS <- varS - sum( val)
    }
    # standard Gaussian value
    z <- ( S - sign(S)) / sqrt( varS)
    # two-sided p-value (not adjusted for multiplicity testing)
    p <- 2 * pnorm( -abs(z), mean=0, sd=1)
    #
    res <- c( z, p)
  }
  c( res[1], res[2], nt)
}

