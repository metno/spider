#+
read_ffin <- function() {
#-----------------------------------------------------------------------------
  ffin<-replaceDate(string=argv$ffin_template,
                    date.str=format(tseq[t],
                              format=argv$ffin_date.format,tz="GMT"),
                    year_string=argv$year_string,
                    month_string=argv$month_string,
                    day_string=argv$day_string,
                    hour_string=argv$hour_string,
                    min_string=argv$min_string,
                    sec_string=argv$sec_string,
                    format=argv$ffin_date.format)
  if (!file.exists(ffin)) {
    print(paste("file not found",ffin))
    next
  }
  t_to_read<-format(
              as.POSIXct(
               as.numeric(
    as.POSIXct(tseq[t],format=argv$ffin_date.format,tz="GMT")) 
    + argv$ffin_hour_offset*3600, origin="1970-01-01",tz="GMT"),
              "%Y%m%d%H%M%S")
  if (argv$debug) print(paste("time_to_read time file",t_to_read,tseq[t],ffin))
  r<-read_griddeddata()
  # case of problems while reading the input file (e.g., missing timestep)
  if (is.null(r)) {
    # try an alternative input file, if provided
    if (!is.na(argv$ffin_template_alternative)) {
      print("---> using alternative input file template")
      ffin<-replaceDate(string=argv$ffin_template_alternative,
                        date.str=format(tseq[t],
                                 format=argv$ffin_date.format,tz="GMT"),
                        year_string=argv$year_string,
                        month_string=argv$month_string,
                        day_string=argv$day_string,
                        hour_string=argv$hour_string,
                        sec_string=argv$sec_string,
                        format=argv$ffin_date.format)
      if (!file.exists(ffin)) {
        print(paste("file not found",ffin))
        next
      }
      t_to_read<-format(
                  as.POSIXct(
                   as.numeric(
        as.POSIXct(tseq[t],format=argv$ffin_date.format,tz="GMT")) 
        + argv$ffin_hour_offset*3600, origin="1970-01-01",tz="GMT"),
                  "%Y%m%d%H%M%S")
      if (argv$debug) print(paste("time_to_read time file",t_to_read,tseq[t],ffin))
      r<-read_griddeddata()
    }
    if (is.null(r)) {
      print(paste("warning: problem while reading time file",t_to_read,ffin))
      next
    }
  } # end, try an alternative input file
  if (!any(!is.na(values<-getValues(r)))) {
    print(paste("warning: all NAs for time file",t_to_read,ffin))
    next
  }
  if (argv$verbose & first) {
    print(paste("extension of the raster file (xmin,xmax,ymin,ymax)",
                round(extent(r)[1],6),
                round(extent(r)[2],6),
                round(extent(r)[3],6),
                round(extent(r)[4],6)))
  }
}
