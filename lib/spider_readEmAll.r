#+ read all the gridded fields
spider_readEmAll <- function( argv=NA, 
                              time=NA,
                              time_ref=NA) {
#------------------------------------------------------------------------------
  if ( class(argv)=="logical")
    if ("argv" %in% ls(envir = .GlobalEnv)) 
      get("argv", envir = .GlobalEnv)
  #
  ffin <- replaceDate( string       = argv$ffin_template,
                       date.str     = format( time,
                                 format=argv$ffin_date.format,tz="GMT"),
                       year_string  = argv$year_string,
                       month_string = argv$month_string,
                       day_string   = argv$day_string,
                       hour_string  = argv$hour_string,
                       min_string   = argv$min_string,
                       sec_string   = argv$sec_string,
                       format       = argv$ffin_date.format)
  if (!file.exists(ffin)) {
    print(paste("file not found",ffin))
    return( NULL)
  }
  t_to_read <- format( as.POSIXct( as.numeric(
    as.POSIXct(time,format=argv$ffin_date.format,tz="GMT")) 
    + argv$ffin_hour_offset*3600, origin="1970-01-01",tz="GMT"),
              "%Y%m%d%H%M%S")
  if (argv$debug) print(paste("time_to_read time file",t_to_read,time,ffin))
  r <- read_griddeddata( mode="data", ffin=ffin, t_to_read=t_to_read)
  # case of problems while reading the input file (e.g., missing timestep)
  if ( is.null(r)) {
    # try an alternative input file, if provided
    if ( !is.na(argv$ffin_template_alternative)) {
      print("---> using alternative input file template")
      ffin <- replaceDate( string   = argv$ffin_template_alternative,
                           date.str = format(time,
                                    format=argv$ffin_date.format,tz="GMT"),
                           year_string  = argv$year_string,
                           month_string = argv$month_string,
                           day_string   = argv$day_string,
                           hour_string  = argv$hour_string,
                           sec_string   = argv$sec_string,
                           format       = argv$ffin_date.format)
      if (!file.exists(ffin)) {
        print(paste("file not found",ffin))
        return( NULL)
      }
      t_to_read<-format( as.POSIXct( as.numeric(
        as.POSIXct(time,format=argv$ffin_date.format,tz="GMT")) 
        + argv$ffin_hour_offset*3600, origin="1970-01-01",tz="GMT"),
                  "%Y%m%d%H%M%S")
      if (argv$debug) print(paste("time_to_read time file",t_to_read,time,ffin))
      r <- read_griddeddata( mode="data", ffin=ffin, t_to_read=t_to_read)
    }
    if (is.null(r)) {
      print(paste("warning: problem while reading time file",t_to_read,ffin))
      return( NULL)
    }
  } # end, try an alternative input file
  if ( !any( !is.na( values<-getValues(r)))) {
    print(paste("warning: all NAs for time file",t_to_read,ffin))
    return( NULL)
  }
  if (argv$verbose & first) {
    print(paste("extension of the raster file (xmin,xmax,ymin,ymax)",
                round(extent(r)[1],6),
                round(extent(r)[2],6),
                round(extent(r)[3],6),
                round(extent(r)[4],6)))
  }
  t_to_read_ffin <- t_to_read
  #
  # read reference file
  r_ref <- NA
  if (!is.na(argv$ffin_ref_template)) {
    ffin_ref<-replaceDate(string   = argv$ffin_ref_template,
                          date.str = format(time_ref,
                                     format=argv$ffin_date.format,tz="GMT"),
                          year_string  = argv$year_string,
                          month_string = argv$month_string,
                          day_string   = argv$day_string,
                          hour_string  = argv$hour_string,
                          sec_string   = argv$sec_string,
                          format       = argv$ffin_date.format)
    if (!file.exists(ffin_ref)) {
      print( paste( "file not found", ffin_ref))
      return( NULL)
    }
    t_to_read <- format(
                  as.POSIXct( time_ref, format=argv$ffin_date.format,tz="GMT"),
                  "%Y%m%d%H%M%S")
    if (argv$debug) print(paste("time_to_read time file", t_to_read,time_ref, ffin_ref))
    r_ref<-read_griddeddata( mode="ref", ffin_ref=ffin_ref, t_to_read=t_to_read)
    # case of problems while reading the input file (e.g., missing timestep)
    if ( is.null( r_ref)) {
      print(paste("warning: problem while reading time file",t_to_read,ffin_ref))
      return( NULL)
    }
    if ( !any( !is.na( values<-getValues(r_ref)))) {
      print(paste("warning: all NAs for time file",t_to_read,ffin_ref))
      return( NULL)
    }
  }
  #
  # Master grid
  rmaster <- NA
  if ( !( "rmaster" %in% ls(envir = .GlobalEnv))) {
    if ( argv$get_master_from_input_grid) {
      if ( argv$master_from_input_aggfact > 1) {
        rmaster <- aggregate( r, fact=argv$master_from_input_aggfact)
      } else {
        rmaster <- r
      }
      argv$ffmaster_proj4 <- argv$ffin_proj4
    } else if ( file.exists(argv$ffmaster)) {
      rmaster <- read_griddeddata( "master")
      if ( is.null( rmaster)) boom( "ERROR problem reading master grid")
    }
  }
  #
  # Master dem grid
  rmaster_dem <- NA
  if ( !( "rmaster_dem" %in% ls(envir = .GlobalEnv))) {
    if ( file.exists(argv$ffmaster) | 
         file.exists(argv$ffmasterdem)) {
      rmaster_dem <- read_griddeddata( "master_dem")
      if ( is.null( rmaster_dem)) boom( "ERROR problem reading master dem grid")
    }
  }
  #
  # data dem grid
  r_dem <- NA
  if ( !( "r_dem" %in% ls(envir = .GlobalEnv))) {
    if ( file.exists(argv$ffindem)) {
      r_dem <- read_griddeddata( "data_dem")
      if ( is.null( r_dem)) boom( "ERROR problem reading data dem grid")
    }
  }
  #
  # normal exit
  return( list( r              = r,
                ffin           = ffin,
                t_to_read_ffin = t_to_read_ffin,
                rmaster        = rmaster, 
                rmaster_dem    = rmaster_dem, 
                r_dem          = r_dem, 
                r_ref          = r_ref))
}
