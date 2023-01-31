#+ create time sequences
spider_timeseq<-function( argv) {
#------------------------------------------------------------------------------

  # input - sequence of time steps 
print(argv$date1)
  # (a) get tseq from the date file
  if ( !is.na( argv$ffin_date.file)) {
    if ( !file.exists( argv$ffin_date.file))
       boom( paste0( "file not found", argv$ffin_date.file))
    tin  <- read.table( file=argv$ffin_date.file, header=F, stringsAsFactors=F, strip.white=T)
    tseq <- as.POSIXlt( str2Rdate(tin$V1,argv$ffin_date.format), tz="UTC")

  # (c) 
  } else if ( argv$date1 != "none" & argv$date2 != "none" & !is.na(argv$time_n_prev) & !is.na(argv$time_n_succ)) {
    date1 <- argv$date1
    date2 <- argv$date2
    first <- T
    while ( as.POSIXct( strptime( date1, format=argv$date.format)) <= as.POSIXct( strptime( date2, format=argv$date.format))) {
      if (argv$time_unit %in% c("sec","secs","second","seconds")) {
        aux1 <- rev( seq( strptime( date1, format=argv$date.format), length=argv$time_n_prev, by=(-argv$time_step)))
        aux2 <- rev( seq( strptime( date1, format=argv$date.format), length=argv$time_n_succ, by=argv$time_step))
      } else {
        aux1 <- rev( seq( strptime( date1, format=argv$date.format), length=argv$time_n_prev, by=paste((-argv$time_step),argv$time_unit)))
        aux2 <- rev( seq( strptime( date1, format=argv$date.format), length=argv$time_n_succ, by=paste(argv$time_step,argv$time_unit)))
      }
      date1_i <- format( aux1[1], format=argv$date.format)
      date2_i <- format( aux2[1], format=argv$date.format)

      tseq_i <- createTimeSeq( start_date     = date1_i,
                               stop_date      = date2_i,
                               format         = argv$date.format,
                               time_step      = argv$time_step,
                               unit           = argv$time_unit,
                               season         = NULL,
                               hourOFday.sel  = NULL,
                               dayOFmonth.sel = NULL,
                               N.prev         = NULL,
                               N.succ         = NULL,
                               RdateOnlyOut   = T,
                               verbose        = F)
      if (first) {
        tseq <- tseq_i
        first <- F
      } else {
        tseq <- c( tseq, tseq_i)
      }
      aux <- rev( seq( strptime( date1, format=argv$date.format), length=2, by="1 year"))
      date1 <- format( aux[1], format=argv$date.format)      
    }

    # tseq for the reference file (if exists)
    if ( !is.na( argv$ffin_ref_template)) {
      if (argv$tseq_ref_hour_offset==0) {
        tseq_ref <- tseq
      } else {
        tseq_ref <- as.POSIXlt( as.POSIXct(tseq,tz="GMT") + argv$tseq_ref_hour_offset*3600, tz="GMT")
      }
    }

  # (b) get tseq from other ways
  } else {
 
    # (b1) get tseq from the template file
    if ( argv$date1 == "none") {
      if ( !file.exists( argv$ffin_template))
         boom( paste0( "file not found", argv$ffin_template))
      tseq <- as.POSIXlt( str2Rdate(nc4.getTime(argv$ffin_template), format="%Y%m%d%H%M"), tz="UTC")

    # (b2) compute tseq
    } else {

      # (b2a) get tseq from date1 -> date2 
      date2 <- argv$date2
      if ( argv$date2 == "none") {
        if ( is.na(argv$time_n_prev) & is.na(argv$time_n_succ)) 
          boom( paste0( "error in date definition"))
        if (!is.na(argv$time_n_prev)) {
          if (argv$time_unit %in% c("sec","secs","second","seconds")) {
            aux<-rev(seq(strptime(argv$date1,format=argv$date.format),
                         length=argv$time_n_prev,
                         by=(-argv$time_step)))
          } else {
            aux<-rev(seq(strptime(argv$date1,format=argv$date.format),
                         length=argv$time_n_prev,
                         by=paste((-argv$time_step),argv$time_unit)))
          }
          date2<-argv$date1
          date1<-format(aux[1],format=argv$date.format)
          rm(aux)
        }
        if (!is.na(argv$time_n_succ)) {
          aux<-rev(seq(strptime(argv$date1,format=argv$date.format),
                                length=argv$time_n_succ,
                                by=paste(argv$time_step,argv$time_unit)))
          date1<-argv$date1
          date2<-format(aux[1],format=argv$date.format)
          rm(aux)
        }
      } else {
        date1 <- argv$date1
      }

      tseq <- createTimeSeq( start_date     = date1,
                             stop_date      = date2,
                             format         = argv$date.format,
                             time_step      = argv$time_step,
                             unit           = argv$time_unit,
                             season         = NULL,
                             hourOFday.sel  = NULL,
                             dayOFmonth.sel = NULL,
                             N.prev         = NULL,
                             N.succ         = NULL,
                             RdateOnlyOut   = T,
                             verbose        = F)
      # 
      if ( !is.na( argv$ffin_ref_template)) {
        if (argv$tseq_ref_hour_offset==0) {
          tseq_ref <- tseq
        } else {
          tseq_ref <- as.POSIXlt( as.POSIXct(tseq,tz="GMT") + argv$tseq_ref_hour_offset*3600, tz="GMT")
        }
      }
    } # end if (argv$date1=="none")
  } # end if (!is.na(ffin_date.file))

  # consider only some months
  if (any(!is.na(argv$date_filter_by_month))) {
    if (length(ix<-which( as.integer(format(tseq,format="%m",tz="GMT")) %in% 
                          argv$date_filter_by_month ))>0) {
      tseq<-tseq[ix]
      if (exists("tseq_ref")) tseq_ref<-tseq_ref[ix]
    } else {
      boom("date_filter_by_month is outside the time period chosen")
    }
  }
  n_tseq<-length(tseq)
  if (argv$debug) {
    print("input - time sequence")
    print(tseq)
    print(paste("number of time steps =",n_tseq))
  }
  # output - sequence of time steps 
  #
  n_tseq_out <- 1
  if (argv$date_out == "none") {
    date_out<-argv$date1
    date_out.format<-argv$date.format
  } else {
    date_out<-argv$date_out
    date_out.format<-argv$date_out.format
  }
  if ( !is.na( argv$date_out_time_step)) {
    if ( is.na(argv$date_out_time_n_prev) & is.na(argv$date_out_time_n_succ) ) 
      boom(paste0("error in date definition"))
    if (!is.na(argv$date_out_time_n_prev)) {
      if (argv$date_out_time_unit %in% c("sec","secs","second","seconds")) {
        aux<-rev(seq(strptime(date_out,format=argv$date_out.format),
                     length=argv$date_out_time_n_prev,
                     by=(-argv$date_out_time_step)))
      } else {
        aux<-rev(seq(strptime(date_out,format=argv$date_out.format),
                     length=argv$date_out_time_n_prev,
                     by=paste((-argv$date_out_time_step),argv$date_out_time_unit)))
      }
      date1 <- format( aux[1], format=argv$date_out.format)
      date2 <- date_out
      rm(aux)
    }
    if (!is.na(argv$date_out_time_n_succ)) {
      aux<-rev(seq(strptime(argv$date_out,format=argv$date_out.format),
                            length=argv$date_out_time_n_succ,
                            by=paste(argv$date_out_time_step,argv$date_out_time_unit)))
      date1 <- date_out
      date2 <- format(aux[1],format=argv$date_out.format)
      rm(aux)
    }
    tseq_out <- createTimeSeq( start_date     = date1,
                               stop_date      = date2,
                               format         = argv$date_out.format,
                               time_step      = argv$date_out_time_step,
                               unit           = argv$date_out_time_unit,
                               season         = NULL,
                               hourOFday.sel  = NULL,
                               dayOFmonth.sel = NULL,
                               N.prev         = NULL,
                               N.succ         = NULL,
                               RdateOnlyOut   = T,
                               verbose        = F)
    n_tseq_out <- length(tseq_out)
    date_out_ix <- vector( mode="numeric", length=n_tseq_out)
    for (t in 1:n_tseq_out)
      date_out_ix[t] <- as.numeric( difftime(  tseq_out[t], tseq[1], tz="UTC", units="secs")) / 
                        as.numeric( difftime( tseq[n_tseq], tseq[1], tz="UTC", units="secs"))
    as.numeric( difftime( tseq[n_tseq], tseq[1], tz="UTC", units="secs"))
    if ( argv$debug) {
      print( "output - time sequence")
      print( tseq_out)
      print( "index (0-1) with respect to input")
      print( round(date_out_ix,3))
      print( paste( "number of time steps =", n_tseq_out))
    }
    date_out <- format( tseq_out, format=date_out.format, tz="UTC")
  }
  #----------------------------------------------------------------------------
  if ( !exists( "tseq_ref")) tseq_ref<-NA
  if ( !exists( "tseq_out")) tseq_out<-NA
  if ( !exists( "date_out_ix")) date_out_ix<-NA
  return( list( n_tseq= n_tseq,
                tseq = tseq,
                tseq_ref = tseq_ref,
                date_out = date_out,
                date_out.format = date_out.format,
                n_tseq_out  = n_tseq_out,
                tseq_out    = tseq_out,
                date_out_ix = date_out_ix ))
}
