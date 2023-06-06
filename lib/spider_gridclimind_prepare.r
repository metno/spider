#+
spider_gridclimind_prepare <- function( argv   = NULL, 
                                        r      = NULL,
                                        r_ref  = NULL,
                                        dat_aggr = NULL,
                                        dat_flag = NULL,
                                        dat_cont = NULL) {
#------------------------------------------------------------------------------
  if ( is.null(argv))
    if ( "argv" %in% ls(envir = .GlobalEnv)) 
      argv <- get( "argv", envir = .GlobalEnv)
  if ( is.null(r))
    if ( "r" %in% ls(envir = .GlobalEnv)) 
      r <- get( "r", envir = .GlobalEnv)
  if ( is.null(r_ref))
    if ( "r_ref" %in% ls(envir = .GlobalEnv)) 
      r_ref <- get( "r_ref", envir = .GlobalEnv)
  if ( is.null(dat_aggr))
    if ( "dat_aggr" %in% ls(envir = .GlobalEnv)) {
      dat_aggr <- get( "dat_aggr", envir = .GlobalEnv)
    } else {
      dat_aggr   <- vector( length=ncell(r), mode="numeric")
      dat_aggr[] <- NA
    }
  if ( is.null(dat_cont))
    if ( "dat_cont" %in% ls(envir = .GlobalEnv)) {
      dat_cont <- get( "dat_cont", envir = .GlobalEnv)
    } else {
      dat_cont   <- vector( length=ncell(r), mode="numeric")
      dat_cont[] <- NA
    }
  if ( is.null(dat_flag))
    if ( "dat_flag" %in% ls(envir = .GlobalEnv)) {
      dat_flag <- get( "dat_flag", envir = .GlobalEnv)
    } else {
      dat_flag   <- vector( length=ncell(r), mode="integer")
      dat_flag[] <- NA
    }
  #
  n <- ncell(r)
  # vr: values of r (n-vector)
  vr   <- getValues(r)
  init <- vector( mode="numeric", length=n); init[] <- NA
  flag <- !is.na( vr) & !is.nan( vr) & is.finite( vr)
  vref <- NULL
  if ( !is.na(argv$ffin_ref_template)) {
    vref <- getValues(r_ref)
    flag <- flag & !is.na( vref) & !is.nan( vref) & is.finite( vref)
  }
  # ix: pointer to elements of r that are not NAs and finite (m-vector)
  # (except if one wants to count the number of NAs (freq and freq_r is NA)
  if ( argv$gridclimind_index == "freq" & is.na(argv$freq_r)) {
    ix <- 1:length(vr) 
  } else {
    if ( length( ix <- which( flag)) == 0) return(NULL)
  }
  m  <- length( ix)
  vr <- vr[ix]
  if ( !is.na(argv$ffin_ref_template)) vref <- vref[ix]
  # NOTE: vr and vref are now m-vectors
  #
  # scores that require to store the whole dataset in memory
  if ( argv$gridclimind_index %in% c( "quantile", "metnoheatwave", "rx5day", "rx3day")) {
    return( list( online=F, ix=ix, n=length(ix),
                  mat_col=vr, mat_ref_col=vref ))
  } else if ( argv$gridclimind_index %in% c( "metnoheatwave2023")) {
    return( list( online=F, ix=ix, n=length(ix),
                  mat_col=vr, mat_ref_col=NULL))
  #
  # scores that are computed online
  } else {
    # dat / dat_cont / dat_aggr/ dat_flag: n-vectors
    dat_cont[ix] <- dat_cont[ix] + 1
    if (length(ixaux <- which(is.na(dat_flag[ix]))) > 0) dat_flag[ix][ixaux] <- 0
    dat <- vector( mode="numeric", length=n); dat[] <- NA
    dat[ix] <- 0
    # compute score for one timestep: begin
    # ixb: pointer to elements of r (m-vec) that satisfy the specified condition
    # %%%%%%%%%% degree_days_sum %%%%%%%%%%%%%%%%%%%%%%%
    if ( argv$gridclimind_index == "degree_days_sum" ) {
      if ( is.na(argv$degday_r)) {
        ixb <- 1:length(vr)
      } else {
        if ( argv$degday_b == "below") {       ixb <- which( vr <  argv$degday_r) }
        else if ( argv$degday_b == "below=") { ixb <- which( vr <= argv$degday_r) }
        else if ( argv$degday_b == "above")  { ixb <- which( vr >  argv$degday_r) }
        else if ( argv$degday_b == "above=") { ixb <- which( vr >= argv$degday_r) }
      }
      if ( length(ixb) > 0) dat[ix][ixb] <- vr[ixb] - argv$degday_r 
    # %%%%%%%%%% ETCCDI Heating degree days %%%%%%%%%%%%%%%
    } else if ( argv$gridclimind_index == "HD17" ) {
      dat[ix] <- 17 - vr
    # %%%%%%%%%% ETCCDI Simple daily intensity index %%%%%%
    } else if ( argv$gridclimind_index == "sdii" ) {
      dat_cont[ix] <- dat_cont[ix] - 1
      if (length(ixb <- which(vr>=1)) > 0) {
        dat_cont[ix][ixb] <- dat_cont[ix][ixb] + 1
        dat[ix][ixb] <- vr[ixb]
      }
    # %%%%%%%%%% degree_days %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    } else if ( argv$gridclimind_index == "degree_days" ) {
      if ( is.na(argv$degday_r)) {
        ixb <- 1:length(vr)
      } else {
        if ( argv$degday_b == "below") {       ixb <- which( vr <  argv$degday_r) }
        else if ( argv$degday_b == "below=") { ixb <- which( vr <= argv$degday_r) }
        else if ( argv$degday_b == "above")  { ixb <- which( vr >  argv$degday_r) }
        else if ( argv$degday_b == "above=") { ixb <- which( vr >= argv$degday_r) }
      }
      if ( length(ixb) > 0) dat[ix][ixb] <- rep( 1, length(ixb))
    # %%%%%%%%%% prcptot %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    } else if ( argv$gridclimind_index == "prcptot" ) {
      if ( is.na(argv$prcptot_r)) {
        ixb <- 1:length(vr)
      } else {
        if ( argv$prcptot_b == "below") {       ixb <- which( vr <  argv$prcptot_r) }
        else if ( argv$prcptot_b == "below=") { ixb <- which( vr <= argv$prcptot_r) }
        else if ( argv$prcptot_b == "above")  { ixb <- which( vr >  argv$prcptot_r) }
        else if ( argv$prcptot_b == "above=") { ixb <- which( vr >= argv$prcptot_r) }
      }
      if ( length(ixb) > 0) dat[ix][ixb] <- vr[ixb]
    # %%%%%%%%%% freq %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    } else if ( argv$gridclimind_index == "freq" ) {
      if ( is.na(argv$freq_r)) {
        ixb <- which( is.na(vr) | is.nan(vr) | !is.finite( vr))
      } else {
        if ( argv$freq_b == "below")         { ixb <- which( vr <  argv$freq_r) }
        else if ( argv$freq_b == "below=")   { ixb <- which( vr <= argv$freq_r) }
        else if ( argv$freq_b == "above")    { ixb <- which( vr >  argv$freq_r) }
        else if ( argv$freq_b == "above=")   { ixb <- which( vr >= argv$freq_r) }
        else if ( argv$freq_b == "within")   { ixb <- which( vr >  argv$freq_r[1] & vr <  argv$freq_r[2]) }
        else if ( argv$freq_b == "=within")  { ixb <- which( vr >= argv$freq_r[1] & vr <  argv$freq_r[2]) }
        else if ( argv$freq_b == "=within=") { ixb <- which( vr >= argv$freq_r[1] & vr <= argv$freq_r[2]) }
        else if ( argv$freq_b == "within=")  { ixb <- which( vr >  argv$freq_r[1] & vr <= argv$freq_r[2]) }
      }
      if ( length(ixb) > 0) dat[ix][ixb] <- rep( 1, length(ixb))
    # %%%%%%%%%% maximum number of consecutive cases %%%%%%%%%%%%%%%%
    } else if ( argv$gridclimind_index == "maxcons" ) {
      if ( is.na(argv$maxcons_r)) {
        ixb <- which( is.na(vr) | is.nan(vr) | !is.finite( vr))
      } else {
        if ( argv$maxcons_b == "below")         { ixb <- which( !(vr <  argv$maxcons_r)) }
        else if ( argv$maxcons_b == "below=")   { ixb <- which( !(vr <= argv$maxcons_r)) }
        else if ( argv$maxcons_b == "above")    { ixb <- which( !(vr >  argv$maxcons_r)) }
        else if ( argv$maxcons_b == "above=")   { ixb <- which( !(vr >= argv$maxcons_r)) }
        else if ( argv$maxcons_b == "within")   { ixb <- which( !(vr >  argv$maxcons_r[1] & vr <  argv$maxcons_r[2])) }
        else if ( argv$maxcons_b == "=within")  { ixb <- which( !(vr >= argv$maxcons_r[1] & vr <  argv$maxcons_r[2])) }
        else if ( argv$maxcons_b == "=within=") { ixb <- which( !(vr >= argv$maxcons_r[1] & vr <= argv$maxcons_r[2])) }
        else if ( argv$maxcons_b == "within=")  { ixb <- which( !(vr >  argv$maxcons_r[1] & vr <= argv$maxcons_r[2])) }
      }
      if ( length(ixb) > 0) dat_cont[ix][ixb] <- 0
    }
    # compute score for one timestep: end
    # update online score: begin
    # -- online sum
    if ( argv$gridclimind_index %in% c( "degree_days_sum", "degree_days",
                                        "prcptot",
                                        "HD17",
                                        "freq") ) {
      if ( length( iy <- !is.na( dat_cont[ix])) > 0) 
        dat_aggr[ix][iy] <- dat_aggr[ix][iy] + dat[ix][iy]
      if ( length( iy <-  is.na( dat_cont[ix])) > 0) {
        dat_cont[ix][iy] <- 1 
        dat_aggr[ix][iy] <- dat[ix][iy]
      }
    # -- online max number of consecutive cases
    } else if (argv$gridclimind_index %in% c( "maxcons") ) {
      if ( length( iy <- !is.na( dat_cont[ix])) > 0) { 
        dat_aggr[ix][iy] <- pmax( dat_aggr[ix][iy], dat_cont[ix][iy], na.rm=T)
      }
      if ( length( iy <-  is.na( dat_cont[ix])) > 0) {
        dat_cont[ix][iy] <- 1 
        dat_aggr[ix][iy] <- dat_cont[ix][iy]
      }
    # -- online mean
    } else if (argv$gridclimind_index %in% c( "mean") ) {
      if ( length( iy <- is.na( dat_cont[ix])) > 0) {
        dat_cont[ix][iy] <- 1
        dat_aggr[ix][iy] <- dat[ix][iy] 
      }
      dat_aggr[ix] <- dat_aggr[ix] + ( dat[ix] - dat_aggr[ix]) / dat_cont[ix]
    # -- sdii, kind of online mean
    } else if (argv$gridclimind_index %in% c( "sdii") ) {
      if ( length(ixb) > 0) {
        if ( length( iy <- is.na( dat_cont[ix][ixb])) > 0) {
          dat_cont[ix][ixb][iy] <- 1
          dat_aggr[ix][ixb][iy] <- dat[ix][ixb][iy] 
        }
        dat_aggr[ix][ixb] <- dat_aggr[ix][ixb] + ( dat[ix][ixb] - dat_aggr[ix][ixb]) / dat_cont[ix][ixb]
      }
    # -- gsl, Growing season length (days)
    # Let tg(ij) be the daily mean temperature at day i of period j. Then counted is the no of days between the first occurrence of at least 6 consecutive days with: tg(ij) > 5 degC and the first occurrence after 1 July of at least 6 consecutive days with: tg(ij) < 5 degC
    # Assumed we have 1 year of continous daily mean temperature values
    # dat_flag = 0 GS not yet started; = 1 we are in GS; = 2 GS ended
    # dat_cont counts the number of consecutive days where a condition is met (tg(ij)>5 if dat_flag = 0 or tg(ij)<5 if dat_flag = 1 and after 1st July)
    # dat_aggr counts the number of days in the GS (1st day is the day with the 5 days before it -i.e. 6 days with the 1st GS day- having tg(ij)>5 degC; the last day is the day with the 5 days before it having tg(ij)<5 degC)

    } else if (argv$gridclimind_index %in% c( "gsl") ) {
      if ( length( iy <- which(dat_flag[ix] == 2)) > 0) dat_cont[ix][iy] <- 0
      dat_flag0 <- dat_flag[ix] == 0
      if ( length( iy <- which(dat_flag0 & vr <= argv$gsl_tg_threshold)) > 0) dat_cont[ix][iy] <- 0
      if ( length( iy <- which(dat_flag0 & dat_cont[ix] >= argv$gsl_ndays_threshold)) > 0) { dat_cont[ix][iy] <- 0 ; dat_flag[ix][iy] <- 1}
      dat_flag1 <- dat_flag[ix] == 1
      if ( format(tseq[t],format="%m",tz="UTC") %in% c("07","08","09","10","11","12")) {
        if ( length( iy <- which(dat_flag1 & vr >= argv$gsl_tg_threshold)) > 0) dat_cont[ix][iy] <- 0
        if ( length( iy <- which(dat_flag1 & dat_cont[ix] >= argv$gsl_ndays_threshold)) > 0) { dat_cont[ix][iy] <- 0 ; dat_flag[ix][iy] <- 2}
      } else {
        if ( length( iy <- which(dat_flag1)) > 0) dat_cont[ix][iy] <- 0
      }
      dat_flag1 <- dat_flag[ix] == 1
      if ( length( iy <- which(dat_flag1 &  is.na(dat_aggr[ix]))) > 0) dat_aggr[ix][iy] <- 0
      if ( length( iy <- which(dat_flag1 & !is.na(dat_aggr[ix]))) > 0) dat_aggr[ix][iy] <- dat_aggr[ix][iy] + 1
    } else {
      return( NULL)
    }
    # update online score: end
    return( list( online=T, ix=ix, n=length(ix),
                  dat_aggr_up=dat_aggr, 
                  dat_cont_up=dat_cont,
                  dat_flag_up=dat_flag ))
  }
}

