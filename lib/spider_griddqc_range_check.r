#+
spider_griddqc_range_check <- function() {
#------------------------------------------------------------------------------
  rval <- getValues(r)
  r[ which( rval<argv$gridded_dqc.min)] <- argv$gridded_dqc.min_pad
  r[ which( rval>argv$gridded_dqc.max)] <- argv$gridded_dqc.max_pad
  r
}

