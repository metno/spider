#+
zrq_datesel_fun <- function( t, inbase=FALSE, ndays=2) {
  year_t <- format( tseq[t], format="%Y")
  month_t <- format( tseq[t], format="%m")
  day_t <- format( tseq[t], format="%d")
  year1 <- as.integer(format( zrq_inbase_begin, format="%Y"))
  year2 <- as.integer(format( zrq_inbase_end, format="%Y"))
  ix <- integer(0)
  ix_years <- integer(0)
  for (y in year1:year2) {
    if (inbase & y == as.integer(year_t)) next
    dateaux <- as.POSIXlt( str2Rdate( paste0(y,"-",month_t,"-",day_t), "%Y-%m-%d"), tz="GMT")
    diff <- difftime( tseq, dateaux, tz="GMT", units="days")
    if ( length(ix_y <- which( abs(as.integer(diff)) <= ndays))>0) {
      ix <- c( ix, ix_y)
      ix_years <- c( ix_years, rep(y,length(ix_y)))
    }
  }
  return(list(ix=ix,ix_years=ix_years))
}
