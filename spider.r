#!/usr/bin/env Rscript
# --~- spider.r -~--
# SPIDER - poSt Process grIdded Datasets nEtcdf foRmat
# See the software repository here: https://github.com/metno/seNorge_2018
#..............................................................................
#Copyright and license
# Copyright (C) 2018 MET Norway. The software is licensed under GPL version 3 
# or (at your option) any later version.
# https://www.gnu.org/licenses/gpl-3.0.en.html
# 
# History:
# 21.01.2019 - Cristian Lussana. Original code.
# -----------------------------------------------------------------------------
#
rm(list=ls())
#
# -----------------------------------------------------------------------------
# Libraries
suppressPackageStartupMessages(library("argparser"))
suppressPackageStartupMessages(library("sp"))
suppressPackageStartupMessages(library("raster"))
suppressPackageStartupMessages(library("rgdal"))
suppressPackageStartupMessages(library("ncdf4"))
suppressPackageStartupMessages(library("dotnc"))
suppressPackageStartupMessages(library("gibson"))
#options(warn = 2, scipen = 999)
options(scipen = 999)
#
# -----------------------------------------------------------------------------
# Constants
# CRS strings
proj4.wgs84<-"+proj=longlat +datum=WGS84"
proj4.ETRS_LAEA<-"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
proj4.utm33<-"+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4.lcc<-"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
# DQC - identify adjacent nodes - Establish (local-) triangulation (Delauney)
# netcdf fixed parameters
varname<-c("windspeed_10m")
varunit<-c("m/s")
varlongname<-c("wind_speed")
varstandardname<-c("wind_speed")
varversion<-c("1.0")
reference<-c("")
diground<-2
summary<-c("daily wind speed (from 06 UTC prev day to 06 UTC day) based on MEPS hourly wind speed.")
sourcestring<-"MET Norway"
title<-"FFMRR-MEPS"
comment<-"Our open data are licensed under Norwegian Licence for Open Government Data (NLOD) or a Creative Commons Attribution 4.0 International License at your preference. Credit should be given to The Norwegian Meteorological institute, shortened “MET Norway”, as the source of data."


# + replace elements of a string with date-time elements
`replaceDate`<-function(string=NULL,
                        date.str=NULL,
                        year_string="yyyy",
                        month_string="mm",
                        day_string="dd",
                        hour_string="hh",
                        format="%Y-%m-%d %H:%M:%S") {
#------------------------------------------------------------------------------
  if (is.null(string) | is.null(date.str)) return(NULL)
  Rdate<-as.POSIXlt(str2Rdate(date.str,format=format))
  yyyy<-Rdate$year+1900
  mm<-formatC(Rdate$mon+1,width=2,flag="0")
  dd<-formatC(Rdate$mday,width=2,flag="0")
  hh<-formatC(Rdate$hour,width=2,flag="0")
  out<-gsub(year_string,yyyy,string)
  out<-gsub(month_string,formatC(mm,width=2,flag="0"),out)
  out<-gsub(day_string,formatC(dd,width=2,flag="0"),out)
  out<-gsub(hour_string,formatC(hh,width=2,flag="0"),out)
  out
}

#+
plot_debug<-function(ff,
                     r,
                     r1=NULL,
                     lbr=20,
                     x,
                     y,
                     proj4,
                     proj4plot=NULL) {
  rmn<-range(getValues(r),na.rm=T)[1]
  rmx<-range(getValues(r),na.rm=T)[2]
  rbr<-seq(rmn,rmx,length=lbr)
  col<-c(rev(rainbow((lbr-1))))
  png(file=ff,width=800,height=800)
  image(r,breaks=rbr,col=col)
  if (!is.null(r1)) contour(r1,levels=c(0,1),add=T)
#  xy<-as.data.frame(cbind(x,y))
#  coordinates(xy)<-c("x","y")
#  proj4string(xy)<-CRS(proj4)
#  if (!is.null(proj4plot)) xy<-spTransform(xy,CRS(proj4plot))
#  points(xy,cex=0.8,pch=19)
  dev.off()
}

boom<-function(str) {
  print(str)
  q(status=1)
}

#==============================================================================
# MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN -
#==============================================================================
t0<-Sys.time()
# [] Read command line arguments and/or set parameters to default
# create parser object
p <- arg_parser("ffmrr")
#..............................................................................
p <- add_argument(p, "date1",
                  help="period start date",type="character")
p <- add_argument(p, "--date2",
                  help="period end date",
                  type="character",
                  default="none")
p <- add_argument(p, "--date.format",
                  help="format of the date/time",
                  type="character",
                  default="%Y-%m-%dT%H")
p <- add_argument(p, "--ffin_date.format",
                  help="format of the date/time",
                  type="character",
                  default="%Y-%m-%dT%H")
#
p <- add_argument(p, "--date_out",
                  help="timestamp for the output netcdf file",
                  type="character",
                  default="none")
p <- add_argument(p, "--date_out.format",
                  help="format of the date/time",
                  type="character",
                  default="%Y-%m-%dT%H")
#
p <- add_argument(p, "--time_step",
                  help="time step",
                  type="numeric",
                  default=1)
p <- add_argument(p, "--time_unit",
                  help="time unit",
                  type="character",
                  default="hours")
p <- add_argument(p, "--time_n_prev",
                  help="number of previous time steps",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--time_n_succ",
                  help="number of successive time steps",
                  type="numeric",
                  default=NA)
#..............................................................................
p <- add_argument(p, "--crop",
                  help="return a geographic subset of the domain (xmin, xmax, ymin, ymax)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p<- add_argument(p, "--crop_proj4",
                 help="proj4 string",
                 type="character",
                 default=NA)
p <- add_argument(p, "--upscale",
                  help="upscale to a coarser grid (see ffmaster)",
                  flag=T)
p <- add_argument(p, "--upscale_trim",
                  help="upscale mode, should we apply \"trim\" function?",
                  flag=T)
p <- add_argument(p, "--upscale_mask",
                  help="upscale mode, should we apply \"mask\" function?",
                  flag=T)
p <- add_argument(p, "--time_aggregation",
                  help="aggregate data over time dimension (default=T if all others=F)",
                  flag=T)
#..............................................................................
p <- add_argument(p, "--correction_factor",
                  help="correction factor",
                  type="numeric",
                  default=NA)
#..............................................................................
p<- add_argument(p, "--fun",
                 help="aggregation function",
                 type="character",
                 default="none")
p<- add_argument(p, "--frac",
                 help="fraction of available data",
                 type="numeric",
                 default=0.9)
#..............................................................................
# IO
# input file(s)
p<- add_argument(p, "--ffin_template",
                 help="path to + name (template) of the input observation files",
                 type="character",
                 default="none")
p <- add_argument(p, "--ffin_varname",
                  help="variable name in the netCDF file",
                  type="character",
                  default="land_area_fraction")
p <- add_argument(p, "--ffin_topdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the fg upside down",
                  flag=T)
p <- add_argument(p, "--ffin_ndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3)
p <- add_argument(p, "--ffin_tpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3)
p <- add_argument(p, "--ffin_epos",
                  help="position of the dimension ''ensemble'' in the netCDF file",
                  type="numeric",
                  default=3)
p <- add_argument(p, "--ffin_dimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--ffin_proj4",
                  help="proj4 string",
                  type="character",
                  default="projection_lambert")
p <- add_argument(p, "--ffin_proj4_var",
                  help="variable that include the specification of the proj4 string",
                  type="character",
                  default="projection_lambert")
p <- add_argument(p, "--ffin_proj4_att",
                  help="attribute with the specification of the proj4 string",
                  type="character",
                  default="proj4")
p <- add_argument(p, "--ffin_e",
                  help="ensemble member to read in the netCDF file",
                  type="numeric",
                  default=NA)
# master grid
p<- add_argument(p, "--ffmaster",
                 help="path to + name (template) of the input observation files",
                 type="character",
                 default="none")
p <- add_argument(p, "--ffmaster_varname",
                  help="variable name in the netCDF file",
                  type="character",
                  default="land_area_fraction")
p <- add_argument(p, "--ffmaster_topdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the fg upside down",
                  flag=T)
p <- add_argument(p, "--ffmaster_ndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3)
p <- add_argument(p, "--ffmaster_tpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3)
p <- add_argument(p, "--ffmaster_epos",
                  help="position of the dimension ''ensemble'' in the netCDF file",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--ffmaster_dimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--ffmaster_proj4",
                  help="proj4 string",
                  type="character",
                  default="projection_lambert")
p <- add_argument(p, "--ffmaster_proj4_var",
                  help="variable that include the specification of the proj4 string",
                  type="character",
                  default="projection_lambert")
p <- add_argument(p, "--ffmaster_proj4_att",
                  help="attribute with the specification of the proj4 string",
                  type="character",
                  default="proj4")
p <- add_argument(p, "--ffmaster_e",
                  help="ensemble member to read in the netCDF file",
                  type="numeric",
                  default=NA)
# output file
p<- add_argument(p, "--ffout",
                 help="output file",
                 type="character",
                 default="out.nc")
p<- add_argument(p, "--ffout_gridtype",
                 help="output grid type",
                 type="character",
                 default="none")
p<- add_argument(p, "--ffout_varname",
                 help="output varname",
                 type="character",
                 default="none")
p<- add_argument(p, "--ffout_varlongname",
                 help="output longvarname",
                 type="character",
                 default="none")
p<- add_argument(p, "--ffout_varstandardname",
                 help="output standardvarname",
                 type="character",
                 default="none")
p<- add_argument(p, "--ffout_varversion",
                 help="output var version",
                 type="character",
                 default="")
p<- add_argument(p, "--ffout_varunit",
                 help="output var version",
                 type="character",
                 default="")
p<- add_argument(p, "--ffout_times_unit",
                 help="output var version",
                 type="character",
                 default="H")
p<- add_argument(p, "--ffout_reference",
                 help="output reference",
                 type="character",
                 default="")
p<- add_argument(p, "--ffout_proj4",
                 help="output proj4",
                 type="character",
                 default="")
p <- add_argument(p, "--ffout_lonlat",
                  help="logical lon lat in the output",
                  flag=T)
p <- add_argument(p, "--ffout_diground",
                  help="rounding digits",
                  type="numeric",
                  default=2)
p<- add_argument(p, "--ffout_summary",
                 help="output var unit",
                 type="character",
                 default="")
p<- add_argument(p, "--ffout_sourcestring",
                 help="output",
                 type="character",
                 default="")
p<- add_argument(p, "--ffout_title",
                 help="output",
                 type="character",
                 default="")
p<- add_argument(p, "--ffout_comment",
                 help="output",
                 type="character",
                 default="")
p<- add_argument(p, "--year_string",
                 help="yyyy",
                 type="character",
                 default="yyyy")
p<- add_argument(p, "--month_string",
                 help="mm",
                 type="character",
                 default="mm")
p<- add_argument(p, "--day_string",
                 help="dd",
                 type="character",
                 default="dd")
p<- add_argument(p, "--hour_string",
                 help="hh",
                 type="character",
                 default="hh")
#..............................................................................
argv <- parse_args(p)
#------------------------------------------------------------------------------
if (!argv$time_aggregation & !argv$upscale) argv$time_aggregation<-T
#------------------------------------------------------------------------------
# Time sequence
if (argv$date2=="none") {
  if ( is.na(argv$time_n_prev) & is.na(argv$time_n_prev) ) bomb(paste0("error in date definition"))
  if (!is.na(argv$time_n_prev)) {
    aux<-rev(seq(strptime(argv$date1,format=argv$date.format),length=argv$time_n_prev,by=paste((-argv$time_step),argv$time_unit)))
    argv$date2<-argv$date1
    argv$date1_def<-format(aux[1],format=argv$date.format)
    rm(aux)
  }
  if (!is.na(argv$time_n_succ)) {
    aux<-rev(seq(strptime(argv$date1,format=argv$date.format),length=argv$time_n_succ,by=paste(argv$time_step,argv$time_unit)))
    argv$date2<-format(aux[1],format=argv$date.format)
    rm(aux)
  }
} else {
  argv$date1_def<-argv$date1
}
#
if (argv$date_out=="none") {
  argv$date_out<-argv$date1
  argv$date_out.format<-argv$date.format
}
tseq<-createTimeSeq(start_date=argv$date1_def,
                    stop_date=argv$date2,
                    format=argv$date.format,
                    time_step=argv$time_step,
                    unit=argv$time_unit,
                    season=NULL,
                    hourOFday.sel=NULL,
                    dayOFmonth.sel=NULL,
                    N.prev=NULL,
                    N.succ=NULL,
                    RdateOnlyOut=T,
                    verbose=F)
n_tseq<-length(tseq)
#------------------------------------------------------------------------------
# Read Input files
n<-0
for (t in 1:n_tseq) {
  ffin<-replaceDate(string=argv$ffin_template,
                    date.str=format(tseq[t],format=argv$ffin_date.format,tz="GMT"),
                    year_string=argv$year_string,
                    month_string=argv$month_string,
                    day_string=argv$day_string,
                    hour_string=argv$hour_string,
                    format=argv$ffin_date.format)
  if (!file.exists(ffin)) {
    print(paste("file not found",ffin))
    next
  }
  print(paste("time file",tseq[t],ffin))
  if (!is.null( attr(try(nc4.getTime(ffin)),"class") )) next
  if (argv$ffin_date.format==argv$date.format) {
    ffin_t<-try(nc4.getTime(ffin))
  } else {
    ffin_t<-format(tseq[t],format="%Y%m%d%H%M",tz="GMT")
    tsteps<-nc4.getTime(ffin)
    ix<-which(tsteps==ffin_t)
    if (length(ix)==0) next
  }
  ffin_epos<-argv$ffin_epos
  if (is.na(argv$ffin_epos)) ffin_epos<-NULL
  ffin_e<-argv$ffin_e
  if (is.na(argv$ffin_e)) ffin_e<-NULL
  ffin_ndim<-argv$ffin_ndim
  ffin_dimnames<-argv$ffin_dimnames
  ffin_tpos<-argv$ffin_tpos
  if (!is.na(argv$ffin_epos)) {
    nc<-nc_open(ffin)
    if (nc$ndims==3) {
      ffin_epos<-NULL
      ffin_e<-NULL
      ffin_dimnames<-c("x","y","time")
      ffin_tpos<-3
      ffin_ndim<-3
    }
    nc_close(nc)
  }
  raux<-try(read_dotnc(nc.file=ffin,
                       nc.varname=argv$ffin_varname,
                       topdown=argv$ffin_topdown,
                       out.dim=list(ndim=ffin_ndim,
                                    tpos=ffin_tpos,
                                    epos=ffin_epos,
                                    names=ffin_dimnames),
                       proj4=argv$ffin_proj4,
                       nc.proj4=list(var=argv$proj4_var,
                                     att=argv$proj4_att),
                       selection=list(t=ffin_t,e=ffin_e)))
  if (is.null(raux)) next
  if (!any(!is.na(getValues(raux$stack)))) next
  if (!any(is.na(argv$crop))) {
    if (argv$ffin_proj4==argv$crop_proj4)
      raux$stack<-crop(raux$stack,
                       extent(argv$crop[1],argv$crop[2],argv$crop[3],argv$crop[4]))
  }
  if (!exists("s"))  {
    s<-raux$stack
  } else {
    s<-stack(s,raux$stack)
  }
  n<-n+1
  rm(raux)
}
#------------------------------------------------------------------------------
# Upscale to coarser grid
if (argv$upscale) {
  if (!file.exists(argv$ffmaster)) print(paste("file not found",argv$ffmaster))
  if (!is.null( attr(try(nc4.getTime(argv$ffmaster)),"class") )) 
    boom("error while reading master file")
  ffmaster_t<-nc4.getTime(argv$ffmaster)[1]
  ffmaster_epos<-argv$ffmaster_epos
  if (is.na(argv$ffmaster_epos)) ffmaster_epos<-NULL
  ffmaster_e<-argv$ffmaster_e
  if (is.na(argv$ffmaster_e)) ffmaster_e<-NULL
  ffmaster_ndim<-argv$ffmaster_ndim
  ffmaster_dimnames<-argv$ffmaster_dimnames
  ffmaster_tpos<-argv$ffmaster_tpos
  if (!is.na(argv$ffmaster_epos)) {
    nc<-nc_open(argv$ffmaster)
    if (nc$ndims==3) {
      ffmaster_epos<-NULL
      ffmaster_e<-NULL
      ffmaster_dimnames<-c("x","y","time")
      ffmaster_tpos<-3
      ffmaster_ndim<-3
    }
    nc_close(nc)
  }
  rmaster<-try(read_dotnc(nc.file=argv$ffmaster,
                       nc.varname=argv$ffmaster_varname,
                       topdown=argv$ffmaster_topdown,
                       out.dim=list(ndim=ffmaster_ndim,
                                    tpos=ffmaster_tpos,
                                    epos=ffmaster_epos,
                                    names=ffmaster_dimnames),
                       proj4=argv$ffmaster_proj4,
                       nc.proj4=list(var=argv$ffmaster_proj4_var,
                                     att=argv$ffmaster_proj4_att),
                       selection=list(t=ffmaster_t,e=ffmaster_e)))
  if (is.null(rmaster)) next
  rmaster<-rmaster$stack
  if (argv$upscale_trim) rmaster<-trim(rmaster) 
  # loop over the raster stack
  for (i in 1:n) {
    r<-subset(s,subset=i)
    values<-getValues(r)
    if ( length(ix<-which(!is.na(values)))>0 ) {
      coord.new<-spTransform( 
                  SpatialPoints(xyFromCell(r,ix),
                                 proj4string=CRS(argv$ffin_proj4)) 
                                            ,CRS(argv$ffmaster_proj4))
      if (argv$fun=="mean") {
        r1<-rasterize(x=coord.new, y=rmaster, field=values[ix], fun=mean)
      }
      if (argv$upscale_mask) r1<-mask(r1,mask=rmaster) 
      if (!exists("s_up"))  {
        s_up<-r1
      } else {
        s_up<-stack(s_up,r1)
      }
      rm(r1,coord.new)
    }
    rm(r,values)  
  } # END loop over the raster stack
  s<-s_up
}
#------------------------------------------------------------------------------
# Aggregate gridpoint-by-gridpoint over time
if (argv$time_aggregation) {
  if ((n/n_tseq)>=argv$frac) {
    if (argv$fun=="sum")  r<-sum(s,na.rm=T)
    if (argv$fun=="mean") r<-mean(s,na.rm=T)
    if (argv$fun=="max")  r<-max(s,na.rm=T)
    if (argv$fun=="min")  r<-min(s,na.rm=T)
  } else {
    boom(paste("number of time steps available=",n," is less than required"))
  }
}
#------------------------------------------------------------------------------
# Adjust output
if (!exists("r")) r<-s_up
if (!is.na(argv$correction_factor)) r<-r*argv$correction_factor
#------------------------------------------------------------------------------
# Write output
xy<-xyFromCell(r,1:ncell(r))
x<-sort(unique(xy[,1]))
y<-sort(unique(xy[,2]),decreasing=T)
r.list<-list()
grid<-array(data=NA,dim=c(length(x),length(y)))
grid[,]<-matrix(data=r, ncol=length(y), nrow=length(x))
r.list[[1]]<-grid
rm(grid,r)
out<-write_dotnc(grid.list=r.list,
                 times=format(strptime(argv$date_out,argv$date_out.format),"%Y%m%d%H%M"),
                 file.name=argv$ffout,
                 grid.type=argv$ffout_gridtype,
                 x=x,
                 y=y,
                 var.name=argv$ffout_varname,
                 var.longname=argv$ffout_varlongname,
                 var.standardname=argv$ffout_varstandardname,
                 var.version=argv$ffout_varversion,
                 times.unit=argv$ffout_times_unit,
                 reference=argv$ffout_reference,
                 proj4.string=argv$ffout_proj4,
                 var.unit=argv$ffout_varunit,
                 lonlat.out=argv$ffout_lonlat,
                 round.dig=argv$ffout_diground,
                 summary=argv$ffout_summary,
                 source.string=argv$ffout_sourcestring,
                 title=argv$ffout_title,
                 comment=argv$ffout_comment,
                 atts.var.add=NULL)
t1<-Sys.time()
print(paste("writing output file",argv$ffout," / time",round(t1-t0,1),attr(t1-t0,"unit")))
#
q(status=0) 
