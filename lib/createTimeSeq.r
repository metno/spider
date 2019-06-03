`str2Rdate` <-
function(ts,format="%Y-%m-%d %H:%M:%S") {
# ===========================================================================
# converts a string into an R (POSIXt,POSIXct) date object
# date objects can be used with arithmetic operations +/-
# ts is a character or a vector of characters with date information
# in the format specified in format
# Output is a date object

     # the lengthy bunch of testing is necessary because strptime needs
     # explicit specifications for month and day, otherwise it returns NA.
     # this extension allows inputs of the format "%Y-%m" in which case the
     # the first day of the month is taken as a reference.

     #Êcheck if year/month/day is specified
     ysp <- length(c(grep("%Y",format,fixed=TRUE),
                     grep("%y",format,fixed=TRUE)))
     msp <- length(c(grep("%m",format,fixed=TRUE),
                     grep("%b",format,fixed=TRUE),
                     grep("%B",format,fixed=TRUE)))
     jsp <- length(c(grep("%j",format,fixed=TRUE)))
     dsp <- length(c(grep("%d",format,fixed=TRUE)))
     if (ysp > 1) { stop("ERROR: Multiple specification of year in 
                         date format.") }
     if (ysp == 0) { stop("ERROR: No year specification in 
                         date format.") }
     if (msp > 1) { stop("ERROR: Multiple specification of month in 
                         date format.") }
     if (dsp > 1) { stop("ERROR: Multiple specification of day in 
                         date format.") }

     # append month or day if not specified
     tss <- ts
     formati <- format
     if (jsp == 0) {
     if (msp == 0) { 
        tss <- paste(tss,"01",sep="")
        formati <- paste(formati,"%m",sep="")
     }
     if (dsp == 0) { 
        tss <- paste(tss,"01",sep="") 
        formati <- paste(formati,"%d",sep="")
     }
     }

     # this is necessary because strptime() returns NA otherwise
     as.POSIXct(strptime(tss,format=formati),tz="GMT")
}

#+ Create a time sequence having daily/hourly timestep
createTimeSeq<-function(start_date="2015.01.01.01",
                        stop_date="2015.12.31.23",
                        format="%Y.%m.%d.%H",
                        time_step=1,
                        unit="hours",
                        season=NULL,
                        hourOFday.sel=NULL,
                        dayOFmonth.sel=NULL,
                        N.prev=NULL,
                        N.succ=NULL,
                        RdateOnlyOut=F,
                        verbose=F) {
#==============================================================================
#  This file is free software: you may copy, redistribute and/or modify it  
#  under the terms of the GNU General Public License as published by the  
#  Free Software Foundation, either version 2 of the License, or (at your  
#  option) any later version.  
#  
#  This file is distributed in the hope that it will be useful, but  
#  WITHOUT ANY WARRANTY; without even the implied warranty of  
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  
#  General Public License for more details.  
#  
#  You should have received a copy of the GNU General Public License  
#  along with this program.  If not, see <http://www.gnu.org/licenses/>. 
#==============================================================================
  # set parameters
  if (!is.null(stop_date))
    if  (is.na(stop_date)) stop_date<-NULL
  if (!is.null(season))
    if  (any(is.na(season))) season<-NULL
  if (!is.null(hourOFday.sel))
    if  (any(is.na(hourOFday.sel))) hourOFday.sel<-NULL
  if (!is.null(dayOFmonth.sel))
    if  (any(is.na(dayOFmonth.sel))) dayOFmonth.sel<-NULL
  if (!is.null(N.prev))
    if  (is.na(N.prev)) N.prev<-NULL
  if (!is.null(N.succ))
    if  (is.na(N.succ)) N.succ<-NULL
  #
  mon.s<-0:11
  if (!is.null(season)) {
    mon.s<-integer(0)
    if (any(season=="MAM")) mon.s<-c(mon.s,2,3,4)
    if (any(season=="JJA")) mon.s<-c(mon.s,5,6,7)
    if (any(season=="SON")) mon.s<-c(mon.s,8,9,10)
    if (any(season=="DJF")) mon.s<-c(mon.s,11,0,1)
  }
  hour.s<-0:23
  if (!is.null(hourOFday.sel)) hour.s<-hourOFday.sel
  day.s<-1:31
  if (!is.null(dayOFmonth.sel)) day.s<-dayOFmonth.sel
  # elaboration
  Rstart_date<-as.POSIXlt(str2Rdate(start_date,format=format))
  if (unit %in% c("sec","secs","second","seconds")) {
    bystr<-time_step
  } else {
    bystr<-paste(time_step," ",unit,sep="")
  }
  if (is.null(stop_date)) {
    Rstop_date<-Rstart_date+as.difftime(N.succ*time_step, unit=unit)
    Rstart_date<-Rstart_date-as.difftime(N.prev*time_step, unit=unit)
  } else {
    Rstop_date<-as.POSIXlt(str2Rdate(stop_date,format=format))
  }
  tseq<-as.POSIXlt(seq(Rstart_date,Rstop_date,by=bystr),"UTC")
  # 
  ix<-which( (tseq$mon %in% mon.s)  & 
             (tseq$mday %in% day.s) & 
             (tseq$hour %in% hour.s) )
  if (length(ix)==0) return(integer(0))
  if (RdateOnlyOut) return(tseq[ix])
  yyyymm.v<-paste(formatC(tseq$year[ix]+1900,width=4,flag="0"),
                  formatC(tseq$mon[ix]+1,width=2,flag="0"),sep="")
  yyyymmddhh.v<-paste(formatC(tseq$year[ix]+1900,width=4,flag="0"),
                      formatC(tseq$mon[ix]+1,width=2,flag="0"),
                      formatC(tseq$mday[ix],width=2,flag="0"),
                      formatC(tseq$hour[ix],width=2,flag="0"),sep="")
  yyyymmdd.v<-paste(formatC(tseq$year[ix]+1900,width=4,flag="0"),
                    formatC(tseq$mon[ix]+1,width=2,flag="0"),
                    formatC(tseq$mday[ix],width=2,flag="0"),sep="")
  nt<-length(yyyymmddhh.v)
  return(list(n=nt,
              yyyymm=yyyymm.v,
              yyyymmdd=yyyymmdd.v,
              yyyymmddhh=yyyymmddhh.v,
              yyyy=formatC(tseq$year[ix]+1900,width=4,flag="0"),
              mm=formatC(tseq$mon[ix]+1,width=2,flag="0"),
              dd=formatC(tseq$mday[ix],width=2,flag="0"),
              hh=formatC(tseq$hour[ix],width=2,flag="0")))
}


