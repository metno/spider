#+
zrq_datesel_fun <- function( tseq, inbase=FALSE) {

  zrq_inbase_begin <- as.POSIXlt( str2Rdate(argv$zrq_inbase_begin, format="%Y-%m-%d"), tz="UTC")
  zrq_inbase_end   <- as.POSIXlt( str2Rdate(argv$zrq_inbase_end,   format="%Y-%m-%d"), tz="UTC")
  zrq_date <- as.POSIXlt( str2Rdate( argv$zrq_date, "%Y-%m-%d"), tz="UTC")
  t <- which( tseq == zrq_date)
  year_t <- format( tseq[t], format="%Y", tz="UTC")
  month_t <- format( tseq[t], format="%m", tz="UTC")
  day_t <- format( tseq[t], format="%d", tz="UTC")
  year1 <- as.integer(format( zrq_inbase_begin, format="%Y", tz="UTC"))
  year2 <- as.integer(format( zrq_inbase_end, format="%Y", tz="UTC"))
  ix <- integer(0)
  ix_years <- integer(0)
  for (y in year1:year2) {
    if (inbase & y == as.integer(year_t)) next
    dateaux <- as.POSIXlt( str2Rdate( paste0(y,"-",month_t,"-",day_t), "%Y-%m-%d"), tz="UTC")
    diff <- difftime( tseq, dateaux, tz="UTC", units="days")
    if ( length(ix_y <- which( abs(as.integer(diff)) <= argv$zrq_ndays))>0) {
      ix <- c( ix, ix_y)
      ix_years <- c( ix_years, rep(y,length(ix_y)))
    }
  }
  return(list(ix=ix,ix_years=ix_years))
}
