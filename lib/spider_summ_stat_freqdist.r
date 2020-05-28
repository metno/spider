#+
spider_summ_stat_freqdist <- function( argv  = NULL, 
                                       r     = NULL,
                                       first = F) {
#------------------------------------------------------------------------------
  if ( is.null(argv))
    if ( "argv" %in% ls(envir = .GlobalEnv)) 
      get( "argv", envir = .GlobalEnv)
  if ( is.null(r))
    if ( "r" %in% ls(envir = .GlobalEnv)) 
      get( "r", envir = .GlobalEnv)
# first time in, define variables
# NOTE: case of "within", num is a vector with dimension nr-1
#       otherwise, num is a vector with dimension nr
  nr <- length(argv$summ_stat_r)
  if (argv$summ_stat_b=="=within" | argv$summ_stat_b=="within" |
      argv$summ_stat_b=="within=" | argv$summ_stat_b=="=within=") {
    num <- vector( mode="numeric", length=(nr-1))
  } else {
    num <- vector( mode="numeric", length=nr)
  }
  #   if required, write the file header
  if ( !file.exists(argv$ffout_summ_stat) | 
       (!argv$ffout_summ_stat_append & first)) {
    header_string <- integer(0)
    for (i in 1:nr) {
      if (argv$summ_stat_b=="=within" | argv$summ_stat_b=="within" |
          argv$summ_stat_b=="within=" | argv$summ_stat_b=="=within=") {
        if (i<nr) header_string <- paste0(header_string,
                                          paste0(";num_",argv$summ_stat_b,"_",
                                                 argv$summ_stat_r[i],"_",
                                                 argv$summ_stat_r[(i+1)]))
      } else {
        header_string <- paste0(header_string,
                                paste0(";num_",argv$summ_stat_b,"_",
                                       argv$summ_stat_r[i]))
      }
    }
    header_string <- paste0(header_string,";numtot")
    cat(file=argv$ffout_summ_stat,append=F,
        paste0("time",header_string,"\n"))
  }
  # count the number of cases 
  val    <- getValues(r)
  numtot <- length(which(!is.na(val)))
  val    <- val[which(!is.na(val))]
  num[]  <- NA
  for (i in 1:nr) {
    if ( argv$summ_stat_b %in% c("within","=within","within=","=within=") ) {
      if (i<nr) num[i] <- score_fun( x          = val,
                                     lab        = "count_x",
                                     threshold  = argv$summ_stat_r[i],
                                     threshold1 = argv$summ_stat_r[(i+1)],
                                     type       = argv$summ_stat_b)
    } else { 
      num[i] <- score_fun( x         = val,
                           lab       = "count_x",
                           threshold = argv$summ_stat_r[i],
                           type      = argv$summ_stat_b)
    }
  }
  if ( exists("ix")) rm( ix)
  rm( val)
  # output
  data_string <- integer(0)
  for (i in 1:nr) {
    if ( argv$summ_stat_b=="=within" | argv$summ_stat_b=="within" |
         argv$summ_stat_b=="within=" | argv$summ_stat_b=="=within=") {
      if (i<nr) data_string <- paste0( data_string, ";", num[i])
    } else {
      data_string <- paste0( data_string, ";", num[i])
    }
  }
  data_string <- paste0( data_string, ";", numtot)
  cat( file=argv$ffout_summ_stat, append=T,
      paste0( t_to_read, data_string, "\n"))
}
