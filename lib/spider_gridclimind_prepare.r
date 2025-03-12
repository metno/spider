#+
spider_gridclimind_prepare <- function( argv   = NULL, 
                                        r      = NULL,
                                        r_ref  = NULL,
                                        dat_aggr    = NULL,
                                        dat_aggrAlt = NULL,
                                        dat_flag    = NULL,
                                        dat_cont    = NULL) {
#------------------------------------------------------------------------------
  # initializations
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
  if ( is.null(dat_aggrAlt))
    if ( "dat_aggrAlt" %in% ls(envir = .GlobalEnv)) {
      dat_aggrAlt <- get( "dat_aggrAlt", envir = .GlobalEnv)
    } else {
      dat_aggrAlt   <- vector( length=ncell(r), mode="numeric")
      dat_aggrAlt[] <- NA
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
  # read file with "flexible" thresholds (i.e. one value for each grid point)
  if (!is.na(argv$ffin_rflexy_rdata)) {
    ffin_rflexy<-replaceDate(string   = argv$ffin_rflexy_rdata,
                             date.str = format(tseq[t],
                                        format=argv$ffin_date.format,tz="GMT"),
                             year_string  = argv$year_string,
                             month_string = argv$month_string,
                             day_string   = argv$day_string,
                             hour_string  = argv$hour_string,
                             sec_string   = argv$sec_string,
                             format       = argv$ffin_date.format)
    if (!file.exists(ffin_rflexy)) {
      cat( paste( "file not found", ffin_rflexy, "\n"))
      q()
    } else {
      rflexy_env <- new.env( parent = emptyenv())
      load( ffin_rflexy, envir=rflexy_env)
      if (!rasters_match( rmaster, rflexy_env$rmaster)) {
        cat( paste( "Warning reading file", ffin_rflexy, "the two rmasters are different\n"))
#        print("rmaster")
#        print(rmaster)
#        print("rflexy_env$rmaster")
#        print(rflexy_env$rmaster)
      }
      rflexy <- r 
      rflexy[] <- NA
      rflexy[rflexy_env$ix_dat] <- rflexy_env$qres
      rm(rflexy_env)
    }
  }

  # begin elaborations
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
  if (!is.na(argv$ffin_rflexy_rdata)) {
    vrflexy <- getValues(rflexy)
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
  if ( !is.na(argv$ffin_rflexy_rdata)) {
    vrflexy <- vrflexy[ix]
    flag_vrflexy <- !is.na( vrflexy) & !is.nan( vrflexy) & is.finite( vrflexy)
    ix_vrflexy <- which( is.na(vrflexy) | is.nan(vrflexy) | !is.finite(vrflexy))
  }
  # NOTE: vr and vref are now m-vectors
  #
  # scores that require to store the whole dataset in memory
  if ( argv$gridclimind_index %in% c( "quantile", "metnoheatwave", "rx5day", "rx3day", "rx2day", "rx4day")) {
    return( list( online=F, ix=ix, n=length(ix),
                  mat_col=vr, mat_ref_col=vref ))
  } else if ( argv$gridclimind_index %in% c( "metnoheatwave2023")) {
    return( list( online=F, ix=ix, n=length(ix),
                  mat_col=vr, mat_ref_col=NULL))
  #
  # scores that are computed online
  } else {
    # dat / dat_cont / dat_aggr/ dat_aggAlt/ dat_flag: n-vectors
    dat_cont[ix] <- dat_cont[ix] + 1
    if (length(ixaux <- which( is.na(dat_flag[ix]))) > 0) dat_flag[ix][ixaux] <- 0
    dat <- vector( mode="numeric", length=n); dat[] <- NA
    dat[ix] <- 0
    dat_alt <- vector( mode="numeric", length=n); dat_alt[] <- NA
    dat_alt[ix] <- 0
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
      if ( length( ixb <- which( vr < 17)) > 0 ) dat[ix][ixb] <- 17 - vr[ixb]
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
      dat_alt[ix] <- vr
    # %%%%%%%%%% prcptot %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    } else if ( argv$gridclimind_index == "prcptot_rflexy" ) {
      if ( argv$prcptot_b == "below") {       ixb <- which( vr <  vrflexy & flag_vrflexy) }
      else if ( argv$prcptot_b == "below=") { ixb <- which( vr <= vrflexy & flag_vrflexy) }
      else if ( argv$prcptot_b == "above")  { ixb <- which( vr >  vrflexy & flag_vrflexy) }
      else if ( argv$prcptot_b == "above=") { ixb <- which( vr >= vrflexy & flag_vrflexy) }
      if ( length(ixb) > 0) dat[ix][ixb] <- vr[ixb]
      dat_alt[ix] <- vr
    # %%%%%%%%%% prcptot as snow %%%%%%%%%%%%%%%%%%%%%%
    } else if ( argv$gridclimind_index == "prcptot_as_snow" ) {
      # vref is supposed to be mean daily temperature in Celsius degrees
      ixb <- which( (vr > 0.1) & (vref < 0.5))
      if ( length(ixb) > 0) dat[ix][ixb] <- vr[ixb]
      dat_alt[ix] <- vr
    # %%%%%%%%%% prcptot as rain %%%%%%%%%%%%%%%%%%%%%%
    } else if ( argv$gridclimind_index == "prcptot_as_rain" ) {
      # vref is supposed to be mean daily temperature in Celsius degrees
      ixb <- which( (vr > 0.1) & (vref >= 0.5))
      if ( length(ixb) > 0) dat[ix][ixb] <- vr[ixb]
      dat_alt[ix] <- vr
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
    # %%%%%%%%%% freq %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    } else if ( argv$gridclimind_index == "freq_rflexy" ) {
      if ( argv$freq_b == "below")         { ixb <- which( vr <  vrflexy & flag_vrflexy) }
      else if ( argv$freq_b == "below=")   { ixb <- which( vr <= vrflexy & flag_vrflexy) }
      else if ( argv$freq_b == "above")    { ixb <- which( vr >  vrflexy & flag_vrflexy) }
      else if ( argv$freq_b == "above=")   { ixb <- which( vr >= vrflexy & flag_vrflexy) }
      if ( length(ixb) > 0) dat[ix][ixb] <- rep( 1, length(ixb))
      if ( length(ix_vrflexy) > 0) dat[ix][ix_vrflexy] <- rep( NA, length(ix_vrflexy))
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
    # %%%%%%%%%% maximum number of consecutive cases alternative version (used for altCDD and altCWD) %%%%%%%%%%%%%%%%
    } else if ( argv$gridclimind_index == "maxcons_alt" ) {
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
      if ( tseq[t] >= as.POSIXlt( str2Rdate(argv$spell_date_begin, format="%Y-%m-%d"), tz="UTC") &
           tseq[t] <= as.POSIXlt( str2Rdate(argv$spell_date_end, format="%Y-%m-%d"), tz="UTC"))
        dat_flag[ix] <- 1
      if ( length(ixb) > 0) {
        dat_cont[ix][ixb] <- 0
        dat_flag[ix][ixb] <- 0
      }
    # %%%%%%%%%% maximum number of consecutive cases gridpoint-by-gridpoint threshold %%%%%%%%%%
    } else if ( argv$gridclimind_index == "maxcons_rflexy" ) {
      if ( argv$maxcons_b == "below")         { ixb <- which( vr >=  vrflexy & flag_vrflexy) }
      else if ( argv$maxcons_b == "below=")   { ixb <- which( vr >   vrflexy & flag_vrflexy) }
      else if ( argv$maxcons_b == "above")    { ixb <- which( vr <=  vrflexy & flag_vrflexy) }
      else if ( argv$maxcons_b == "above=")   { ixb <- which( vr <   vrflexy & flag_vrflexy) }
      if ( length(ixb) > 0) dat_cont[ix][ixb] <- 0
    # %%%%%%%%%% cold/warm spell duration index (gridpoint-by-gridpoint threshold) alternative version (used for altCSDI and altWSDI)%%%%%%%%%%
    } else if ( argv$gridclimind_index %in% c( "sdi_rflexy", "sdi_rflexy_alt" )) {
# dat_aggr is the vector of climate indices
# dat_cont is the vector that counts the consecutive occurrence for a prescribed condition (i.e. min temperature less than the 10-th percentile)
# dat_flag is the vector that is equal to 1 if the timestep is whithin the aggregation period AND the prescribed condition is TRUE; otherwise it is set to 0
# dat_aggrAlt is the vector that is equal to 1 if the spell has been added to dat_aggr before; otherwise, it is equal to 0 and it is the first time we add the spell to dat_aggr. If it is the first time we consider the spell, then we need to add a number that could be greater than 1.
      # time differences in days (tseq[t]-spell_date_begin and tseq[t]-spell_date_end
      diff_from_b <- as.numeric( difftime( tseq[t], as.POSIXlt( str2Rdate(argv$spell_date_begin, format="%Y-%m-%d"), tz="UTC"), tz="UTC", units="days"))
      diff_from_e <- as.numeric( difftime( tseq[t], as.POSIXlt( str2Rdate(argv$spell_date_end, format="%Y-%m-%d"), tz="UTC"), tz="UTC", units="days"))
      # initializations
      if ( length( ixaux <- which( is.na( dat_aggrAlt[ix]))) > 0) dat_aggrAlt[ix][ixaux] <- 0
      if ( length( ixaux <- which( is.na( dat_aggr[ix])))    > 0) dat_aggr[ix][ixaux]    <- 0
      # test the condition gridpoint-by-gridpoint
      if ( argv$sdi_b == "below")         { ixb <- which( vr >=  vrflexy & flag_vrflexy) }
      else if ( argv$sdi_b == "below=")   { ixb <- which( vr >   vrflexy & flag_vrflexy) }
      else if ( argv$sdi_b == "above")    { ixb <- which( vr <=  vrflexy & flag_vrflexy) }
      else if ( argv$sdi_b == "above=")   { ixb <- which( vr <   vrflexy & flag_vrflexy) }
      # if the timestep is within the aggregation period, then set dat_flaf to 1
      if ( diff_from_b >= 0 & diff_from_e <= 0)
        dat_flag[ix] <- 1
      # for all gridpoints where the condition is not valid, set dat_cont, dat_flag and dat_aggr to 0
      if ( length(ixb) > 0) {
        dat_cont[ix][ixb] <- 0
        dat_flag[ix][ixb] <- 0
        dat_aggrAlt[ix][ixb] <- 0
      }
    }
    # compute score for one timestep: end
    # update online score: begin
    # -- online sum
    if ( argv$gridclimind_index %in% c( "degree_days_sum", "degree_days",
                                        "HD17",
                                        "freq",
                                        "freq_rflexy")) {
      if ( length( iy <- which( !is.na( dat_cont[ix]))) > 0) 
        dat_aggr[ix][iy] <- dat_aggr[ix][iy] + dat[ix][iy]
      if ( length( iy <- which( is.na( dat_cont[ix]))) > 0) {
        dat_cont[ix][iy] <- 1 
        dat_aggr[ix][iy] <- dat[ix][iy]
      }
    # -- online prcptot flexible threshold 
    } else if (argv$gridclimind_index %in% c( "prcptot", 
                                              "prcptot_rflexy",
                                              "prcptot_as_snow",
                                              "prcptot_as_rain")) {
      if ( length( iy <- which( !is.na( dat_cont[ix]))) > 0) { 
        dat_aggr[ix][iy] <- dat_aggr[ix][iy] + dat[ix][iy]
        dat_aggrAlt[ix][iy] <- dat_aggrAlt[ix][iy] + dat_alt[ix][iy]
      }
      if ( length( iy <- which( is.na( dat_cont[ix]))) > 0) {
        dat_cont[ix][iy] <- 1 
        dat_aggr[ix][iy] <- dat[ix][iy]
        dat_aggrAlt[ix][iy] <- dat_alt[ix][iy]
      }
    # -- online max number of consecutive cases
    } else if (argv$gridclimind_index %in% c( "maxcons", "maxcons_rflexy") ) {
      if ( length( iy <- which( !is.na( dat_cont[ix]))) > 0) { 
        dat_aggr[ix][iy] <- pmax( dat_aggr[ix][iy], dat_cont[ix][iy], na.rm=T)
      }
      if ( length( iy <- which( is.na( dat_cont[ix]))) > 0) {
        dat_cont[ix][iy] <- 1 
        dat_aggr[ix][iy] <- dat_cont[ix][iy]
      }
    # -- online max number of consecutive cases
    } else if (argv$gridclimind_index %in% c( "maxcons_alt") ) {
      if ( length( iy <- which( !is.na( dat_cont[ix]) & dat_flag[ix] == 1)) > 0) { 
        dat_aggr[ix][iy] <- pmax( dat_aggr[ix][iy], dat_cont[ix][iy], na.rm=T)
      }
      if ( length( iy <- which( is.na( dat_cont[ix]))) > 0) {
        dat_cont[ix][iy] <- 1 
        dat_aggr[ix][iy] <- dat_cont[ix][iy]
      }
# -- cold/warm spell duration index (gridpoint-by-gridpoint threshold) alternative version (used for altCSDI and altWSDI)
    } else if (argv$gridclimind_index %in% c( "sdi_rflexy", "sdi_rflexy_alt") ) {
      # for all points where the condition is valid and with at least one timestep within the aggregation period ...
      if ( length( iy <- which(!is.na( dat_cont[ix]) & dat_flag[ix] == 1)) > 0) { 
        # for those spells where we need to update the index ...
        if ( length( iz <- which( dat_aggrAlt[ix][iy] == 1 & dat_cont[ix][iy] >= argv$spell_length)) > 0) {
          # in case of "sdi_rflexy", update only if the current timestep is within the aggregation period
          if ( argv$gridclimind_index == "sdi_rflexy" & diff_from_b >= 0  & diff_from_e <= 0) {
            dat_aggr[ix][iy][iz] <- dat_aggr[ix][iy][iz] + 1 
          # in case of "sdi_rflexy_alt", always update
          } else if ( argv$gridclimind_index == "sdi_rflexy_alt") {
            dat_aggr[ix][iy][iz] <- dat_aggr[ix][iy][iz] + 1 
          }
        }
        # for those spells that have not been yet considered in the calculations ... 
        if ( length( iz <- which( dat_aggrAlt[ix][iy] == 0 & dat_cont[ix][iy] >= argv$spell_length)) > 0) {
          # in case of "sdi_rflexy" and we are inside the aggregation period, use either dat_cont or 
          # the number of days within the period (useful if we are at the begininng of the period)
          if ( argv$gridclimind_index == "sdi_rflexy" & diff_from_b >= 0  & diff_from_e <= 0) { 
            dat_aggr[ix][iy][iz] <- dat_aggr[ix][iy][iz] +  pmin( dat_cont[ix][iy][iz], diff_from_b+1) 
          # in case of "sdi_rflexy" and we are after the end of the aggregation period, 
          # use only the days within the aggregation period 
          } else if ( argv$gridclimind_index == "sdi_rflexy" & diff_from_e >= 0) { 
            dat_aggr[ix][iy][iz] <- dat_aggr[ix][iy][iz] +  dat_cont[ix][iy][iz] - diff_from_e
          # in case of "sdi_rflexy_alt", use dat_cont 
          } else if ( argv$gridclimind_index == "sdi_rflexy_alt") {
            dat_aggr[ix][iy][iz] <- dat_aggr[ix][iy][iz] + dat_cont[ix][iy][iz]
          }
          dat_aggrAlt[ix][iy][iz] <- 1
        }
      }
      # the next two lines are probably never used, but they shouldn't be dangerous either
      if ( length( iy <- which( is.na( dat_cont[ix]))) > 0) dat_cont[ix][iy] <- 1 
      if ( length( iy <- which( is.na( dat_aggr[ix]))) > 0) dat_aggr[ix][iy] <- 0 
    # -- online mean
    } else if (argv$gridclimind_index %in% c( "mean") ) {
      if ( length( iy <- which( is.na( dat_cont[ix]))) > 0) {
        dat_cont[ix][iy] <- 1
        dat_aggr[ix][iy] <- dat[ix][iy] 
      }
      dat_aggr[ix] <- dat_aggr[ix] + ( dat[ix] - dat_aggr[ix]) / dat_cont[ix]
    # -- sdii, kind of online mean
    } else if (argv$gridclimind_index %in% c( "sdii") ) {
      if ( length(ixb) > 0) {
        if ( length( iy <- which( is.na( dat_cont[ix][ixb]))) > 0) {
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
      if ( length( iy <- which(is.na(dat_aggr[ix]))) > 0) dat_aggr[ix][iy] <- 0
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
                  dat_aggrAlt_up=dat_aggrAlt, 
                  dat_cont_up=dat_cont,
                  dat_flag_up=dat_flag ))
  }
}

