# + replace elements of a string with date-time elements
replaceDate<-function(string=NULL,
                      date.str=NULL,
                      year_string="yyyy",
                      month_string="mm",
                      day_string="dd",
                      hour_string="hh",
                      min_string="MM",
                      sec_string="SS",
                      format="%Y-%m-%d %H:%M:%S") {
#------------------------------------------------------------------------------
  if (is.null(string) | is.null(date.str)) return(NULL)
  Rdate<-as.POSIXlt(str2Rdate(date.str,format=format))
  yyyy<-Rdate$year+1900
  mm<-formatC(Rdate$mon+1,width=2,flag="0")
  dd<-formatC(Rdate$mday,width=2,flag="0")
  hh<-formatC(Rdate$hour,width=2,flag="0")
  MM<-formatC(Rdate$min,width=2,flag="0")
  SS<-formatC(Rdate$sec,width=2,flag="0")
  out<-gsub(year_string,yyyy,string)
  out<-gsub(month_string,formatC(mm,width=2,flag="0"),out)
  out<-gsub(day_string,formatC(dd,width=2,flag="0"),out)
  out<-gsub(hour_string,formatC(hh,width=2,flag="0"),out)
  out<-gsub(min_string,formatC(MM,width=2,flag="0"),out)
  out<-gsub(sec_string,formatC(SS,width=2,flag="0"),out)
  out
}

