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


boom<-function(str) {
  print(str)
  q(status=1)
}

# + replace elements of a string with date-time elements
replaceDate<-function(string=NULL,
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
read_griddeddata<-function(mode="data") { #data,master,data_dem,master_dem
  if (mode=="data") {
    ff<-ffin
    ff_tpos<-argv$ffin_tpos
    ff_epos<-argv$ffin_epos
    ff_e<-argv$ffin_e
    ff_varname<-argv$ffin_varname
    ff_topdown<-argv$ffin_topdown
    ff_ndim<-argv$ffin_ndim
    ff_dimnames<-argv$ffin_dimnames
    ff_proj4<-argv$ffin_proj4
    ff_proj4_var<-argv$ffin_proj4_var
    ff_proj4_att<-argv$ffin_proj4_att
  } else if (mode=="data_dem") {
    ff<-argv$ffindem
    ff_tpos<-argv$ffindem_tpos
    ff_epos<-argv$ffindem_epos
    ff_e<-argv$ffindem_e
    ff_varname<-argv$ffindem_varname
    ff_topdown<-argv$ffindem_topdown
    ff_ndim<-argv$ffindem_ndim
    ff_dimnames<-argv$ffindem_dimnames
    ff_proj4<-argv$ffin_proj4
    ff_proj4_var<-argv$ffin_proj4_var
    ff_proj4_att<-argv$ffin_proj4_att
  } else if (mode=="master") {
    ff<-argv$ffmaster
    ff_tpos<-argv$ffmaster_tpos
    ff_epos<-argv$ffmaster_epos
    ff_e<-argv$ffmaster_e
    ff_varname<-argv$ffmaster_varname
    ff_topdown<-argv$ffmaster_topdown
    ff_ndim<-argv$ffmaster_ndim
    ff_dimnames<-argv$ffmaster_dimnames
    ff_proj4<-argv$ffmaster_proj4
    ff_proj4_var<-argv$ffmaster_proj4_var
    ff_proj4_att<-argv$ffmaster_proj4_att
  } else if (mode=="master_dem") {
    ff<-argv$ffmasterdem
    ff_tpos<-argv$ffmasterdem_tpos
    ff_epos<-argv$ffmasterdem_epos
    ff_e<-argv$ffmasterdem_e
    ff_varname<-argv$ffmasterdem_varname
    ff_topdown<-argv$ffmasterdem_topdown
    ff_ndim<-argv$ffmasterdem_ndim
    ff_dimnames<-argv$ffmasterdem_dimnames
    ff_proj4<-argv$ffmaster_proj4
    ff_proj4_var<-argv$ffmaster_proj4_var
    ff_proj4_att<-argv$ffmaster_proj4_att
  }
  # time dimension not present
  if (is.na(ff_tpos)) {
    ff_tpos<-NULL
    ff_t<-NULL
  # time dimension present
  } else {
    # read input file time steps
    if (!is.null( attr(tsteps_in<-try(nc4.getTime(ff)),"class") )) {
      print(paste("warning: not able to read time dimension from file ",ff))
      return(NULL)
    }
    # data, check time to read is available
    if (mode=="data") {
      ff_t<-format(tseq[t],format="%Y%m%d%H%M",tz="GMT")
      if (!(ff_t %in% tsteps_in)) {
        print(paste("warning: time step to read",ff_t,"not in input file",ff))
        return(NULL)
      }
    # master grid, set time to read as the first time step
    } else if (mode=="master") {
      ff_t<-tsteps_in[1]
    }
  }
  if (is.na(ff_epos)) ff_epos<-NULL
  if (is.na(ff_e)) ff_e<-NULL
  raux<-try(read_dotnc(nc.file=ff,
                       nc.varname=ff_varname,
                       topdown=ff_topdown,
                       out.dim=list(ndim=ff_ndim,
                                    tpos=ff_tpos,
                                    epos=ff_epos,
                                    names=ff_dimnames),
                       proj4=ff_proj4,
                       nc.proj4=list(var=ff_proj4_var,
                                     att=ff_proj4_att),
                       selection=list(t=ff_t,e=ff_e)))
  if (!is.null(raux)) raux<-raux$stack
  raux
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
#..............................................................................
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
p<- add_argument(p, "--spider_path",
                 help="where is the spider.r file?",
                 type="character",
                 default=".")
p <- add_argument(p, "--config_file",
                  help="configuration file",
                  type="character",
                  default=NULL)
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
#..............................................................................
p <- add_argument(p, "--upscale",
                  help="upscale to a coarser grid (see ffmaster)",
                  flag=T)
#..............................................................................
p <- add_argument(p, "--downscale",
                  help="downscale to a finer grid (see ffmaster). \"--fun\" implemented are \"ngb\" or \"bilinear\"",
                  flag=T)
#..............................................................................
p <- add_argument(p, "--latte",
                  help="interpoLATion verTical profilE. Interpolation over master grid based on a non-linear vertical profile",
                  flag=T)
p <- add_argument(p, "--cores",
                  help="set the number of cores for parallel runs. Rpackage \"parallel\" required. 0 stands for \"use detectCores\". Default do not use it.",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--latte_halfbox",
                  help="half-width of the square box used to select the nearest observations. same units as master CRS.",
                  type="numeric",
                  default=1)
p <- add_argument(p, "--latte_pmax",
                  help="maximum number of observations to use in the neighbourhood of each observation",
                  type="integer",
                  default=50)
p <- add_argument(p, "--latte_fglab",
                  help="method used to create the first-guess (\"linear\",\"Frei\")",
                  type="character",
                  default="Frei")
p <- add_argument(p, "--latte_gamma",
                  help="lapse rate value",
                  type="numeric",
                  default=-0.0065)
#..............................................................................
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
p <- add_argument(p, "--master_trim",
                  help="should we apply \"trim\" function to the master?",
                  flag=T)
p <- add_argument(p, "--master_mask",
                  help="should we use the master grid to maskout gridpoints?",
                  flag=T)
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
# input file(s)
p<- add_argument(p, "--ffindem",
                 help="path to + name (template) of the input observation files",
                 type="character",
                 default="none")
p <- add_argument(p, "--ffindem_varname",
                  help="variable name in the netCDF file",
                  type="character",
                  default="land_area_fraction")
p <- add_argument(p, "--ffindem_topdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the fg upside down",
                  flag=T)
p <- add_argument(p, "--ffindem_ndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3)
p <- add_argument(p, "--ffindem_tpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3)
p <- add_argument(p, "--ffindem_epos",
                  help="position of the dimension ''ensemble'' in the netCDF file",
                  type="numeric",
                  default=3)
p <- add_argument(p, "--ffindem_dimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--ffindem_e",
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
# master grid
p<- add_argument(p, "--ffmasterdem",
                 help="path to + name (template) of the input observation files",
                 type="character",
                 default="none")
p <- add_argument(p, "--ffmasterdem_varname",
                  help="variable name in the netCDF file",
                  type="character",
                  default="land_area_fraction")
p <- add_argument(p, "--ffmasterdem_topdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the fg upside down",
                  flag=T)
p <- add_argument(p, "--ffmasterdem_ndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3)
p <- add_argument(p, "--ffmasterdem_tpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3)
p <- add_argument(p, "--ffmasterdem_epos",
                  help="position of the dimension ''ensemble'' in the netCDF file",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--ffmasterdem_dimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--ffmasterdem_e",
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
#-----------------------------------------------------------------------------
# read configuration file
if (!is.na(argv$config_file)) {
  if (file.exists(argv$config_file)) {
    source(argv$config_file)
    argv_tmp<-append(argv,conf)
    names_argv_tmp<-names(argv_tmp)
    argv_def<-list()
    names_argv_def<-integer(0)
    k<-0
    for (i in 1:length(argv_tmp)) {
      if (names_argv_tmp[i] %in% names_argv_def) next
      k<-k+1
      j<-which(names_argv_tmp==names_argv_tmp[i])
      argv_def[[k]]<-argv_tmp[[j[length(j)]]]
      names_argv_def<-c(names_argv_def,names_argv_tmp[i])
    }
    names(argv_def)<-names_argv_def
    rm(argv_tmp,names_argv_tmp,names_argv_def)
    rm(argv)
    argv<-argv_def
    rm(argv_def)
  } else {
    print("WARNING: config file not found")
    print(argv$config_file)
  }
}
#------------------------------------------------------------------------------
if (!argv$time_aggregation & !argv$upscale & 
    !argv$downscale & !argv$latte) 
  argv$time_aggregation<-T
#-----------------------------------------------------------------------------
# Multi-cores run
if (!is.na(argv$cores)) {
  suppressPackageStartupMessages(library("parallel"))
  if (argv$cores==0) argv$cores <- detectCores()
  print(paste("--> multi-core run, cores=",argv$cores))
}
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
  r<-read_griddeddata()
  if (is.null(r)) {
    print(paste("warning: problem while reading time file",tseq[t],ffin))
    next
  }
  if (!any(!is.na(values<-getValues(r)))) {
    print(paste("warning: all NAs for time file",tseq[t],ffin))
    next
  }
  #----------------------------------------------------------------------------
  # crop
  if (!any(is.na(argv$crop))) {
    if (argv$ffin_proj4==argv$crop_proj4) {
      r<-crop(r,
              extent(argv$crop[1],argv$crop[2],argv$crop[3],argv$crop[4]))
    } else {
      coord.new<-spTransform( 
                  SpatialPoints(xyFromCell(r,ix),
                                 proj4string=CRS(argv$crop_proj4)) 
                                            ,CRS(argv$ffin_proj4))
    }
  }
  #----------------------------------------------------------------------------
  # Upscale to coarser grid
  if (argv$upscale) {
    if (!exists("rmaster")) {
      rmaster<-read_griddeddata("master")
      if (is.null(rmaster)) boom("ERROR problem reading master grid")
    }
    ix<-which(!is.na(values))
    if (argv$ffin_proj4==argv$ffmaster_proj4) {
      coord.new<-xyFromCell(r,ix)
    } else {
      coord.new<-spTransform( 
                  SpatialPoints(xyFromCell(r,ix),
                                 proj4string=CRS(argv$ffin_proj4)) 
                                            ,CRS(argv$ffmaster_proj4))
    }
    if (argv$fun=="mean") {
      r1<-rasterize(x=coord.new, y=rmaster, field=values[ix], fun=mean)
    } else if (argv$fun=="max") {
      r1<-rasterize(x=coord.new, y=rmaster, field=values[ix], fun=max)
    } else if (argv$fun=="min") {
      r1<-rasterize(x=coord.new, y=rmaster, field=values[ix], fun=min)
    }
    if (argv$master_mask) r1<-mask(r1,mask=rmaster)
    r<-r1
    rm(r1,coord.new)
  }
  #----------------------------------------------------------------------------
  # Downscale to finer grid
  if (argv$downscale) {
    if (!exists("rmaster")) {
      rmaster<-read_griddeddata("master")
      if (is.null(rmaster)) boom("ERROR problem reading master grid")
    }
    if (!(argv$fun %in% c("ngb","bilinear"))) 
      boom("--fun must be either \"ngb\" or \"bilinear\"")
    if (argv$ffin_proj4==argv$ffmaster_proj4) {
      r1<-resample(r, rmaster, method=argv$fun)
    } else {
      r1<-projectRaster(r, rmaster, method=argv$fun)
    }
    if (argv$master_mask) r1<-mask(r1,mask=rmaster)
    r<-r1
    rm(r1)
  }
  #----------------------------------------------------------------------------
  # Interpolation over master grid based on a non-linear vertical profile
  if (argv$latte) { # LATTE - interpoLATion verTical profilE
    if (!exists("rmaster")) {
      rmaster<-read_griddeddata("master")
      if (is.null(rmaster)) boom("ERROR problem reading master grid")
      if (!any(!is.na(values_ma<-getValues(rmaster)))) {
        print(paste("warning: all NAs for master grid file",argv$ffmaster))
        next
      }
    }
    if (!exists("rmaster_dem")) {
      rmaster_dem<-read_griddeddata("master_dem")
      if (is.null(rmaster_dem)) boom("ERROR problem reading master dem")
      if (!any(!is.na(values_ma_dem<-getValues(rmaster_dem)))) {
        print(paste("warning: all NAs for master dem file",argv$ffmasterdem))
        next
      }
      if (length(ix_ma<-which(!is.na(values_ma) & !is.na(values_ma_dem)))==0) {
        print(paste("warning: all NAs for conjunction of master & dem files"))
        next
      }
      xy_ma<-xyFromCell(r,ix_ma)
      nmaster<-length(ix_ma)
      xgrid_spint<-xy_ma[ix_ma,1]
      ygrid_spint<-xy_ma[ix_ma,2]
      zgrid_spint<-values_ma_dem[ix_ma]
      yo_to_check<-rep(NA,nmaster)
    }
    if (!exists("r_dem")) {
      r_dem<-read_griddeddata("data_dem")
      if (is.null(r_dem)) boom("ERROR problem reading dem")
      if (!any(!is.na(values_dem<-getValues(r_dem)))) {
        print(paste("warning: all NAs for dem file",argv$ffindem))
        next
      }
      rm(r_dem)
    }
    ix_in<-which(!is.na(values) & !is.na(values_dem))
    if (argv$ffin_proj4==argv$ffmaster_proj4) {
      coord.new<-xyFromCell(r,ix_in)
    } else {
      coord.new<-spTransform( 
                  SpatialPoints(xyFromCell(r,ix_in),
                                 proj4string=CRS(argv$ffin_proj4)) 
                                            ,CRS(argv$ffmaster_proj4))
    }
    if (!file.exists(fffun<-file.path(argv$spider_path,"lib","oivar.r")))
      boom(paste("file not found",fffun))
    source(fffun)
    # 
    nobs<-length(ix_in)
    xobs_spint<-coord.new[ix_in]
    yobs_spint<-coord.new[ix_in]
    zobs_spint<-values_dem[ix_in]
    yo_spint<-values[ix_in]
    fg_min<-min(yo_spint)-as.numeric(diff(range(yo_spint)))
    fg_max<-max(yo_spint)+as.numeric(diff(range(yo_spint)))
    if (!is.na(argv$cores)) {
      arr<-t(mcmapply(oi_var_gridpoint_by_gridpoint,
                      1:nmaster,
                      mc.cores=argv$cores,
                      SIMPLIFY=T,
                      box_o_nearest_halfwidth=argv$latte_halfbox,
                      pmax=argv$latte_pmax,
                      fg=argv$latte_fglab,
                      fg_gamma=argv$latte_gamma,
                      fg_min=fg_min,
                      fg_max=fg_max,
                      return_fg_only=T))
    # no-multicores
    } else {
      arr<-t(mapply(oi_var_gridpoint_by_gridpoint,
                    1:nmaster,
                    SIMPLIFY=T,
                    box_o_nearest_halfwidth=argv$latte_halfbox,
                    pmax=argv$latte_pmax,
                    fg=argv$latte_fglab,
                    fg_gamma=argv$latte_gamma,
                    fg_min=fg_min,
                    fg_max=fg_max,
                    return_fg_only=T))
    }
    r<-rmaster; r[]<-NA
    r[ix_ma]<-arr[,1]
  }
  #----------------------------------------------------------------------------
  # store in a raster stack 
  if (!exists("s"))  {
    s<-r
  } else {
    s<-stack(s,r)
  }
  n<-n+1
  rm(r,values)
} # end time loop
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
if (!exists("r")) r<-s
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
#------------------------------------------------------------------------------
# Normal exit
q(status=0) 
