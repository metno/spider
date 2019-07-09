#!/usr/bin/env Rscript
# --~- spider.r -~--
# SPIDER - poSt Process grIdded Datasets nEtcdf foRmat
# See the software repository here: https://github.com/cristianlussana/spider
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

# -----------------------------------------------------------------------------
# FUNCTIONS

#+ Manage errors
boom<-function(str,status=1) { print(str); q(status=status) }

#+ Box-Cox transformation
boxcox<-function(x,lambda) {
  if (lambda==0) {
    return(log(x))
  } else {
    return((x**lambda-1)/lambda)
  }
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
                  help="format of the input date/time",
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
p <- add_argument(p, "--metno_radar_dqc",
                  help="data quality control over metno radar data",
                  flag=T)
p <- add_argument(p, "--reflectivity_to_precip",
                  help="transform reflectivity to precipitation rate",
                  flag=T)
#..............................................................................
p <- add_argument(p, "--gridded_dqc",
                  help="data quality control over gridded data",
                  flag=T)
p <- add_argument(p, "--gridded_dqc.min",
                  help="minimum allowed value",
                  type="numeric",
                  default=-10000)
p <- add_argument(p, "--gridded_dqc.max",
                  help="maximum allowed value",
                  type="numeric",
                  default=10000)

#..............................................................................
p <- add_argument(p, "--time_aggregation",
                  help="aggregate data over time dimension (default=T if all others=F)",
                  flag=T)
#..............................................................................
p <- add_argument(p, "--summ_stat",
                  help="summary step-by-step statistics",
                  flag=T)
p<- add_argument(p, "--summ_stat_fun",
                 help="function applied",
                 type="character",
                 default="wave_nrgx")
p<- add_argument(p, "--ffout_summ_stat",
                 help="full file name for the summary statistics",
                 type="character",
                 default="summstat.txt")
p<- add_argument(p, "--ffout_summ_stat_append",
                 help="append output",
                 flag=T)
p<- add_argument(p, "--summ_stat_condition_threshold",
                 help="apply statistics on a selection of cases, where the field has at least \"ncells\" (or a fraction \"fcells\" of not NAs values) with a value higher than \"threshold\" ",
                 type="character",
                 default=NA)
p<- add_argument(p, "--summ_stat_condition_ncells",
                 help="apply statistics on a selection of cases, where the field has at least \"ncells\" (or a fraction \"fcells\" of not NAs values) with a value higher than \"threshold\"",
                 type="numeric",
                 default=NA)
p<- add_argument(p, "--summ_stat_condition_fcells",
                 help="apply statistics on a selection of cases, where the field has at least \"ncells\" (or a fraction \"fcells\" of not NAs values) with a value higher than \"threshold\"",
                 type="numeric",
                 default=NA)
#..............................................................................
p <- add_argument(p, "--correction_factor",
                  help="correction factor",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--offset",
                  help="offset",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--dem_correction_factor",
                  help="dem correction factor",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--dem_offset",
                  help="dem offset",
                  type="numeric",
                  default=NA)
#..............................................................................
p<- add_argument(p, "--fun",
                 help="aggregation function",
                 type="character",
                 default="none")
p<- add_argument(p, "--fun_weights",
                 help="aggregation function weights",
                 type="character",
                 default="1,1,...,1")
p<- add_argument(p, "--frac",
                 help="fraction of available data",
                 type="numeric",
                 default=0.9)
p<- add_argument(p, "--fill_gaps",
                 help="fill the gaps before applying \"fun\"",
                 flag=T)
p<- add_argument(p, "--stop_if_two_gaps",
                 help="\"fill the gaps\" mode, stop if two consecutive gaps are found",
                 flag=T)
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
p<- add_argument(p, "--ffin_template_alternative",
                 help="path to + name (template) of the input observation files",
                 type="character",
                 default=NA)
p <- add_argument(p, "--ffin_hour_offset",
                  help="hour offset",
                  type="numeric",
                  default=0)
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
                 default=NA)
p <- add_argument(p, "--ffmasterdem_varname",
                  help="variable name in the netCDF file",
                  type="character",
                  default=NA)
p <- add_argument(p, "--ffmasterdem_topdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the fg upside down",
                  flag=T)
p <- add_argument(p, "--ffmasterdem_ndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--ffmasterdem_tpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=NA)
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
p<- add_argument(p, "--min_string",
                 help="MM",
                 type="character",
                 default="MM")
p<- add_argument(p, "--sec_string",
                 help="SS",
                 type="character",
                 default="SS")
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
if (!argv$time_aggregation & !argv$upscale & !argv$downscale & !argv$latte &
    !argv$summ_stat) 
  argv$time_aggregation<-T
#
gridded_output<-F
if (argv$time_aggregation | argv$upscale | argv$downscale | argv$latte) 
  gridded_output<-T
#
if (argv$summ_stat & argv$summ_stat_fun=="wave_nrgx") 
  suppressPackageStartupMessages(library("waveslim"))
argv$summ_stat_condition_threshold<-as.numeric(gsub("_","-",argv$summ_stat_condition_threshold))
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
  if ( is.na(argv$time_n_prev) & 
       is.na(argv$time_n_prev) ) bomb(paste0("error in date definition"))
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
if (!file.exists(fftimeseq<-file.path(argv$spider_path,"lib","createTimeSeq.r")))
  boom(paste("file not found",fftimeseq))
source(fftimeseq)
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
if (!file.exists(ffrepdate<-file.path(argv$spider_path,"lib","replaceDate.r")))
  boom(paste("file not found",ffrepdate))
source(ffrepdate)
if (!file.exists(ffread<-file.path(argv$spider_path,"lib","read_griddeddata.r")))
  boom(paste("file not found",ffread))
source(ffread)
t_ok<-vector()
first<-T
for (t in 1:n_tseq) {
  ffin<-replaceDate(string=argv$ffin_template,
                    date.str=format(tseq[t],
                              format=argv$ffin_date.format,tz="GMT"),
                    year_string=argv$year_string,
                    month_string=argv$month_string,
                    day_string=argv$day_string,
                    hour_string=argv$hour_string,
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
              "%Y%m%d%H%M")
  print(paste("time_to_read time file",t_to_read,tseq[t],ffin))
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
                  "%Y%m%d%H%M")
      print(paste("time_to_read time file",t_to_read,tseq[t],ffin))
      r<-read_griddeddata()
    }
    if (is.null(r)) {
      print(paste("warning: problem while reading time file",t_to_read,ffin))
      next
    }
  }
  if (!any(!is.na(values<-getValues(r)))) {
    print(paste("warning: all NAs for time file",t_to_read,ffin))
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
                  SpatialPoints(rbind(c(argv$crop[1],argv$crop[3]),
                                      c(argv$crop[1],argv$crop[4]),
                                      c(argv$crop[2],argv$crop[3]),
                                      c(argv$crop[2],argv$crop[4])),
                                 proj4string=CRS(argv$crop_proj4)) 
                                            ,CRS(argv$ffin_proj4))
      bbox<-attr(coord.new,"bbox")
      r<-crop(r,
              extent(bbox[1,1],bbox[1,2],bbox[2,1],bbox[2,2]))
      rm(coord.new,bbox)
    }
    if (!any(!is.na(values<-getValues(r)))) {
      print(paste("warning: all NAs after crop"))
      next
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
    if (!any(!is.na(values<-getValues(r)))) {
      print(paste("warning: all NAs after upscale"))
      next
    }
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
    if (!any(!is.na(values<-getValues(r)))) {
      print(paste("warning: all NAs after downscale"))
      next
    }
  }
  #----------------------------------------------------------------------------
  # Interpolation over master grid based on a non-linear vertical profile
  if (argv$latte) { # LATTE - interpoLATion verTical profilE
    if (!exists("rmaster")) {
      cat("read master...")
      rmaster<-read_griddeddata("master")
      if (is.null(rmaster)) boom("ERROR problem reading master grid")
      if (!any(!is.na(values_ma<-getValues(rmaster)))) {
        print(paste("warning: all NAs for master grid file",argv$ffmaster))
        next
      }
      cat("ok!\n")
    }
    if (!exists("rmaster_dem")) {
      cat("read master dem...")
      rmaster_dem<-read_griddeddata("master_dem")
      if (is.null(rmaster_dem)) boom("ERROR problem reading master dem")
      if (!any(!is.na(values_ma_dem<-getValues(rmaster_dem)))) {
        print(paste("warning: all NAs for master dem file",argv$ffmasterdem))
        next
      }
      # ix_ma, indexes to points that unmasked and not NAs
      if (length(ix_ma<-which(!is.na(values_ma) & !is.na(values_ma_dem)))==0) {
        print(paste("warning: all NAs for conjunction of master & dem files"))
        next
      }
      cat("ok!\n")
      xy_ma<-xyFromCell(rmaster,ix_ma) #dim nmaster 2
      nmaster<-length(ix_ma)
      xgrid_spint<-xy_ma[,1]
      ygrid_spint<-xy_ma[,2]
      zgrid_spint<-values_ma_dem[ix_ma]
      yo_to_check<-rep(NA,nmaster)
    }
    if (!exists("r_dem")) {
      cat("read data dem...")
      r_dem<-read_griddeddata("data_dem")
      if (is.null(r_dem)) boom("ERROR problem reading dem")
      if (!any(!is.na(values_dem<-getValues(r_dem)))) {
        print(paste("warning: all NAs for dem file",argv$ffindem))
        next
      }
      rm(r_dem)
      cat("ok!\n")
    }
    ix_in<-which(!is.na(values) & !is.na(values_dem))
    if (argv$ffin_proj4==argv$ffmaster_proj4) {
      coord.new<-xyFromCell(r,ix_in) #nobs 2
    } else {
      cat("coordinate conversion...")
      coord.new<-spTransform( 
                  SpatialPoints(xyFromCell(r,ix_in),
                                 proj4string=CRS(argv$ffin_proj4)) 
                                            ,CRS(argv$ffmaster_proj4))
      coord.new<-attr(coord.new,"coords") #nobs 2
      cat("ok!\n")
    }
    if (!file.exists(fffun<-file.path(argv$spider_path,"lib","oivar.r")))
      boom(paste("file not found",fffun))
    source(fffun)
    # 
    nobs<-length(ix_in)
    xobs_spint<-coord.new[,1]
    yobs_spint<-coord.new[,2]
    zobs_spint<-values_dem[ix_in]
    yo_spint<-values[ix_in]
    fg_min<-min(yo_spint)-as.numeric(diff(range(yo_spint)))
    fg_max<-max(yo_spint)+as.numeric(diff(range(yo_spint)))
    cat("who ordered latte?...")
    if (!is.na(argv$cores)) {
      arr<-mcmapply(oi_var_gridpoint_by_gridpoint,
                    1:nmaster,
                    mc.cores=argv$cores,
                    SIMPLIFY=T,
                    box_o_nearest_halfwidth=argv$latte_halfbox,
                    pmax=argv$latte_pmax,
                    fg=argv$latte_fglab,
                    fg_gamma=argv$latte_gamma,
                    fg_min=fg_min,
                    fg_max=fg_max,
                    return_fg_only=T)
    # no-multicores
    } else {
      arr<-mapply(oi_var_gridpoint_by_gridpoint,
                  1:nmaster,
                  SIMPLIFY=T,
                  box_o_nearest_halfwidth=argv$latte_halfbox,
                  pmax=argv$latte_pmax,
                  fg=argv$latte_fglab,
                  fg_gamma=argv$latte_gamma,
                  fg_min=fg_min,
                  fg_max=fg_max,
                  return_fg_only=T)
    }
    cat("there you are!\n")
    if (any(is.na(arr))) 
      print(paste0("@@ warning: problems in regridding over ",length(which(is.na(arr))),
                   " points"))
    r<-rmaster; r[]<-NA
    r[ix_ma]<-arr
    if (!any(!is.na(values<-getValues(r)))) {
      print(paste("warning: all NAs after latte"))
      next
    }
  }
  #----------------------------------------------------------------------------
  # radar data quality control
  if (argv$metno_radar_dqc) {
    var_dqcrad<-c("is_nodata",
                  "is_blocked",
                  "is_seaclutter",
                  "is_groundclutter",
                  "is_otherclutter")
    nv_dqcrad<-length(var_dqcrad)
    for (v in 1:nv_dqcrad) {   
      u<-read_griddeddata("data",var=var_dqcrad[v])
      if (is.null(u)) {
        print(paste("warning: problem reading radar dqc var=",var_dqcrad[v]))
        next
      }
      if (!any(!is.na(values_u<-getValues(u)))) {
        print(paste("warning: all NAs for  radar dqc var=",var_dqcrad[v]))
        next
      }
      r[which(getValues(u)==1)]<-NA
    } # end for v
    if (!any(!is.na(values<-getValues(r)))) {
      print(paste("warning: all NAs after radar dqc"))
      next
    }
  } 
  #----------------------------------------------------------------------------
  # data quality control
  # filter out unplausible values, otherwise writing the outpur might crash
  if (!is.na(argv$gridded_dqc.min) & !is.na(argv$gridded_dqc.max)) {
    rval<-getValues(r)
    r[which(rval<argv$gridded_dqc.min | rval>argv$gridded_dqc.max)]<-NA
    rm(rval)
  }
  if (argv$gridded_dqc) {
    suppressPackageStartupMessages(library("igraph"))
    rval<-getValues(r)
    # a. remove unplausible values
    r[which(rval<argv$gridded_dqc.min | rval>argv$gridded_dqc.max)]<-NA
    # b. remove patches of connected cells that are too small
    #  check for small and isolated clumps (patches) of connected cells with 
    #  precipitation greater than a predefined threshold
    #   threshold 0 mm/h. remove all the clumps made of less than 100 cells
    #   threshold 1 mm/h. remove all the clumps made of less than 50 cells
    dqc.clump.thr<-c(0,1)
    dqc.clump.n<-c(100,50)
    for (i in 1:length(dqc.clump.thr)) {
      raux<-r
      if (any(rval<=dqc.clump.thr[i])) 
        raux[which(rval<=dqc.clump.thr[i])]<-NA
      rclump<-clump(raux)
      fr<-freq(rclump)
      ix<-which(!is.na(fr[,2]) & fr[,2]<=dqc.clump.n[i])
      if (length(ix)>0) {
        rval[getValues(rclump) %in% fr[ix,1]]<-NA
        r[]<-rval
      }
      rm(raux,fr,ix,rclump)
    }
    # c. remove outliers. Check for outliers in square boxes of 51km by 51km
    t0a<-Sys.time()
    raux<-r
    daux<-boxcox(x=rval,lambda=0.5)
    raux[]<-daux
    # compute mean and sd
    raux_agg<-aggregate(raux,fact=5,fun=mean,na.rm=T)
    daux_agg<-getValues(raux_agg)
    ix_aux<-which(!is.na(daux_agg))
    xyaux<-xyFromCell(raux_agg,ix_aux)
    xrad_aux<-xyaux[,1]
    yrad_aux<-xyaux[,2]
    vrad_aux<-daux_agg[ix_aux]
    get_rad_stat<-function(i,dh_ref=25000) { 
      deltax<-abs(xrad_aux[i]-xrad_aux)
      deltay<-abs(yrad_aux[i]-yrad_aux)
      ix<-which( deltax<dh_ref & deltay<dh_ref )
      dist<-deltax; dist[]<-NA
      dist[ix]<-sqrt(deltax[ix]*deltax[ix]+deltay[ix]*deltay[ix])
      ix<-which( !is.na(dist) & dist<dh_ref )
      return(c(mean(vrad_aux[ix]),sd(vrad_aux[ix])))
    }
    if (!is.na(argv$cores)) {
      arr<-mcmapply(get_rad_stat,
                    1:length(ix_aux),
                    mc.cores=argv$cores,
                    SIMPLIFY=T)
    # no-multicores
    } else {
      arr<-mapply(get_rad_stat,
                  1:length(ix_aux),
                  SIMPLIFY=T)
    }
    raux_agg[]<-NA; raux_agg[ix_aux]<-arr[1,]
    raux<-disaggregate(raux_agg,fact=5)
    if (ncell(raux)>ncell(r)) {
      raux<-crop(raux,r)
    } else if (ncell(raux)<ncell(r)) {
      raux<-extend(raux,r)
    }
    avg<-getValues(raux)
    raux_agg[]<-NA; raux_agg[ix_aux]<-arr[2,]
    raux<-disaggregate(raux_agg,fact=5,method="bilinear",na.rm=T)
    if (ncell(raux)>ncell(r)) {
      raux<-crop(raux,r)
    } else if (ncell(raux)<ncell(r)) {
      raux<-extend(raux,r)
    }
    stdev<-getValues(raux)
    ix<-which(stdev>0 & !is.na(daux) & !is.na(avg) & !is.na(stdev))
    rm(arr,raux_agg,ix_aux,xrad_aux,yrad_aux,vrad_aux,daux_agg,xyaux)
    # outliers are defined as in Lanzante,1997: abs(value-mean)/st.dev > 5
    suspect<-which((abs(daux[ix]-avg[ix])/stdev[ix])>5) 
    if (length(suspect)>0) rval[ix[suspect]]<-NA
    r[]<-rval
    rm(raux,daux,avg,stdev,ix,suspect,rval)
    t1a<-Sys.time()
    print(paste(" remove outliers - time",round(t1a-t0a,1),
                                          attr(t1a-t0a,"unit")))
  }
  #----------------------------------------------------------------------------
  # convert from equivalent_reflectivity_factor to rain rate (mm/h) 
  if (argv$reflectivity_to_precip) { 
    r<-(10**(r/10)/200)**(5/8)
    if (!any(!is.na(values<-getValues(r)))) {
      print(paste("warning: all NAs after transforming reflectivity to precip"))
      next
    }
  } 
  #----------------------------------------------------------------------------
  # summary statistics 
  if (argv$summ_stat) {
    if (length( ixvalid<-which(!is.na(values)) )>0) {
      ncells<-NA
      fcells<-NA
      if (!is.na(argv$summ_stat_condition_threshold)) {
        ixtrue<-which( !is.na(values) & 
                       values>argv$summ_stat_condition_threshold )
        ncells<-length(ixtrue) 
        fcells<-length(ixtrue)/length(ixvalid)
        if ( !is.na(argv$summ_stat_condition_ncells) &
             ncells<argv$summ_stat_condition_ncells ) 
          next
        if ( !is.na(argv$summ_stat_condition_fcells) &
             fcells<argv$summ_stat_condition_fcells ) 
          next
      } 
    } else {
      next
    }
    if (argv$summ_stat_fun=="wave_nrgx") {
      if (!exists("nnboot")) {
        mindim<-min(dim(r)[1:2])
        resx<-res(r)[1]
        resy<-res(r)[2]
        # number of bootstrap samples
        nnboot<-10
        # largest spatial scale (#cells) within the domain
        dimdy<-2**floor(log2(mindim))
        # number of scales 1,...,dimdy
        nnscales<-log2(dimdy)+1
        # scales 1,...,dimdy (#cells)
        listscales<-2**(seq(1,nnscales)-1)*1#cell
        # select bootstrap grids SW corners 
        spanx<-ncol(r)-dimdy
        spany<-nrow(r)-dimdy
        xsw<-xmin(r)+
             round(runif(nnboot,min=0,max=spanx))*resx
        ysw<-ymin(r)+
             round(runif(nnboot,min=0,max=spany))*resy
      }
      En2o_boot<-array(data=NA,dim=c(nnscales,nnboot))
      for(boot in seq(1,nnboot)){
        boots<-formatC(boot,width=2,flag="0")
        # dimdy x dimdy grid
        rboot_ext<-extent(xsw[boot],xsw[boot]+resx*dimdy,
                          ysw[boot],ysw[boot]+resy*dimdy)
        rboot<-crop(r,rboot_ext)
        if (usemask<-any(is.na(getValues(rboot)))) {
          rbootmask<-aggregate(rboot,fact=8,fun=mean,na.rm=F)
          rbootmask<-disaggregate(rbootmask,fact=8)
        }
        obs<-as.matrix(rboot)
        obs[is.na(obs)]<-0
        # N= nnscales-1
        N<-log2(dim(rboot)[1])
        Eo.dwt<-dwt.2d(obs, wf = "haar", J = N)
        En2o<-vector(mode="numeric",length=N)
        for (i in 1:N) {
          is<-formatC(i,width=2,flag="0")
          if (usemask) {
            ragg1<-raster(ext=rboot_ext,
                          res=c(2**i*resx,2**i*resy),
                          crs=crs(r), vals=NA)
            ragg2<-ragg1; ragg3<-ragg1
            ragg1[]<-(Eo.dwt[[1+3*(i-1)]]/2**i)**2
            ragg2[]<-(Eo.dwt[[2+3*(i-1)]]/2**i)**2
            ragg3[]<-(Eo.dwt[[3+3*(i-1)]]/2**i)**2
            En2o[i] <- cellStats(mask(disaggregate(ragg1,fact=2**i),rbootmask),stat="mean",na.rm=T) + 
                       cellStats(mask(disaggregate(ragg2,fact=2**i),rbootmask),stat="mean",na.rm=T) +
                       cellStats(mask(disaggregate(ragg3,fact=2**i),rbootmask),stat="mean",na.rm=T) 
            rm(ragg1,ragg2,ragg3) 
          } else {
            En2o[i] <- mean((Eo.dwt[[1 + 3 * (i - 1)]]/2^i)^2) + 
                       mean((Eo.dwt[[2 + 3 * (i - 1)]]/2^i)^2) +
                       mean((Eo.dwt[[3 + 3 * (i - 1)]]/2^i)^2)

          }
#          ragg1<-raster(ext=rboot_ext,
#                        res=c(2**i*resx,2**i*resy),
#                        crs=crs(r), vals=NA)
#          ragg2<-ragg1; ragg3<-ragg1
#          ragg1[]<-(Eo.dwt[[1+3*(i-1)]]/2**i)**2
#          ragg2[]<-(Eo.dwt[[2+3*(i-1)]]/2**i)**2
#          ragg3[]<-(Eo.dwt[[3+3*(i-1)]]/2**i)**2
#          png(file=paste0("map_",boots,"_comp_",is,"_1.png"),
#              width=800,height=800)
#          plot(ragg1)
#          dev.off()
#          png(file=paste0("map_",boots,"_comp_",is,"_2.png"),
#              width=800,height=800)
#          plot(ragg2)
#          dev.off()
#          png(file=paste0("map_",boots,"_comp_",is,"_3.png"),
#              width=800,height=800)
#          plot(ragg3)
#          dev.off()
        }
        # energies of mother wavelets
        En2o_boot[1:N,boot]<-En2o[1:N]
        # nnscales = N+1, energy of the father wavelet = mean(obs)**2
        En2o_boot[(N+1),boot]<-cellStats(rboot,stat="mean",na.rm=T)**2
#        png(file=paste0("map_",boots,".png"),width=800,height=800)
#        plot(rboot,breaks=c(0,0.1,0.5,1,2,3,4,5,15),
#             col=c("gray",rev(rainbow(7))))
#        dev.off()
#        png(file=paste0("nrgx_",boots,".png"),width=800,height=800)
#        plot(En2o_boot[,boot],ylim=c(0,2),axes=F)
#        axis(1,labels=listscales,at=1:nnscales)
#        axis(2)
#        abline(h=0)
#        dev.off()
      }
      # control sum(En2o) == mean(obs^2)
      En2o_t<-array(data=NA,dim=c(nnscales+1))
      En2o_t[1:nnscales]<-rowMeans(En2o_boot,na.rm=T)  
      # Total squared-energy, sum(En2o) == mean(obs^2)
      En2o_t[(nnscales+1)]<-sum(En2o_t[1:nnscales],na.rm=T)
      if (!file.exists(argv$ffout_summ_stat) | 
          (!argv$ffout_summ_stat_append & first)) {
        str<-"time;"
        for (aux in 1:nnscales) 
          str<-paste0(str,
                      "En2_",
                      formatC(listscales[aux],width=4,flag="0"),
                      ";")
        str<-paste0(str,"En2_tot;\n")
        cat(file=argv$ffout_summ_stat,append=F,str)
        rm(str,aux)
      }
      cat(file=argv$ffout_summ_stat,append=T,
          paste(t_to_read,
                gsub(",",";",toString(round(En2o_t,9))),"\n",sep=";"))
      rm(obs,En2o_boot,En2o_t,Eo.dwt)
    } #endif summ_stat_fun=="wave_nrgx"
     else if (argv$summ_stat_fun=="standard") {
      if (!file.exists(argv$ffout_summ_stat) | 
          (!argv$ffout_summ_stat_append & first)) {
        cat(file=argv$ffout_summ_stat,append=F,
            paste0("time;mean;stdev;min;",
                   "q01;q05;q10;q20;q25;q50;q75;q80;q90;q95;q99;",
                   "max;thr_gt;ncell;fcell\n"))
      }
      qvec<-as.vector(quantile(values[ixvalid],
            probs=c(0,0.01,0.05,0.1,0.2,0.25,0.5,0.75,0.8,0.9,0.95,0.99,1)))
      cat(file=argv$ffout_summ_stat,append=T,
          paste0(t_to_read,";",
                 round(mean(values[ixvalid]),3),";",
                 round(sd(values[ixvalid]),3),";",
                 round(qvec[1],3),";",
                 round(qvec[2],3),";",
                 round(qvec[3],3),";",
                 round(qvec[4],3),";",
                 round(qvec[5],3),";",
                 round(qvec[6],3),";",
                 round(qvec[7],3),";",
                 round(qvec[8],3),";",
                 round(qvec[9],3),";",
                 round(qvec[10],3),";",
                 round(qvec[11],3),";",
                 round(qvec[12],3),";",
                 round(qvec[13],3),";",
                 round(argv$summ_stat_condition_threshold,4),";",
                 round(ncells,0),";",
                 round(fcells,4),"\n"))
    } #endif summ_stat_fun=="standard"
  }
  #----------------------------------------------------------------------------
  # store in a raster stack 
  if (gridded_output )  {
    if (!exists("s"))  {
      s<-r
    } else {
      s<-stack(s,r)
    }
  }
  n<-n+1
  t_ok[n]<-t
  rm(r,values)
  first<-F
} # end time loop
#------------------------------------------------------------------------------
# Aggregate gridpoint-by-gridpoint over time
if (gridded_output)  {
  if (argv$fill_gaps) {
    imiss<-which(!(1:n_tseq %in% t_ok))
    nmiss<-length(imiss)
    if (nmiss>0) {
      if (any(diff(imiss)==1) & argv$stop_if_two_gaps) 
        boom(paste("ERROR two consecutive gaps found"))
      r<-raster(s,layer=1)
      for (i in imiss) {
        r[]<-NA
        iprev<-ifelse(i==1,2,(i-1))
        inext<-ifelse(i==n_tseq,(n_tseq-1),(i+1))
        r[]<-(getValues(raster(s,layer=iprev))+getValues(raster(s,layer=inext)))/2
        if (!any(!is.na(getValues(r))) & argv$stop_if_two_gaps )
          boom(paste("ERROR two consecutive gaps found"))
        s<-stack(s,r)
        n<-n+1
        t_ok[n]<-i
      }
    }
    if (exists("r")) rm(r,i,iprev,inext)
    rm(imiss,nmiss)
  }
  if (argv$time_aggregation) {
    if ((n/n_tseq)>=argv$frac) {
      # set weights
      weights_aux<-suppressWarnings(
                     as.numeric(strsplit(argv$fun_weights,",")[[1]])
                   [which(!is.na(
                     as.numeric(strsplit(argv$fun_weights,",")[[1]])))])
      n_aux<-length(weights_aux)
      weights<-rep(NA,n)
      weights[1]<-weights_aux[1]
      weights[2:(n-1)]<-rep(weights_aux[2:(n_aux-1)],n)[1:(n-2)]
      weights[n]<-weights_aux[n_aux]
      rm(weights_aux)
      # apply function
      # case of all weights = 1
      if (!any(weights!=1)) {
        if (argv$fun=="sum")  r<-sum(s,na.rm=T)
        if (argv$fun=="mean") r<-mean(s,na.rm=T)
        if (argv$fun=="max")  r<-max(s,na.rm=T)
        if (argv$fun=="min")  r<-min(s,na.rm=T)
        if (argv$fun=="radar_mean")  {
          first<-T
          for (t in 1:n) {
            weight<-ifelse(format(tseq[t_ok[t]],format="%M%S",tz="GMT")=="0000",
                           0.5,1)
            dat<-getValues(subset(s,subset=t))
            if (first) {
              dat_mean<-dat
              dat_cont<-dat
              dat_cont[]<-NA 
              dat_cont[!is.na(dat)]<-1
              first<-F
            } else {
              ix_nona<-which(!is.na(dat))
              ix_nonas<-which(!is.na(dat) & is.na(dat_cont))
              if (length(ix_nonas)>0) {
                dat_cont[ix_nonas]<-0
                dat_mean[ix_nonas]<-0
              }
              if (length(ix_nona)>0) {
                dat_cont[ix_nona]<-dat_cont[ix_nona]+1
                dat_mean[ix_nona]<-dat_mean[ix_nona]+
                        (weight*dat[ix_nona]-dat_mean[ix_nona])/
                        dat_cont[ix_nona]
              }
              rm(ix_nona,ix_nonas)
            }
            rm(dat,weight)
          }
          r<-subset(s,subset=1)
          r[]<-NA
          ix<-which(!is.na(dat_cont) & (dat_cont/n_tseq)>=argv$frac)
          if (length(ix)>0) r[ix]<-dat_mean[ix]
          rm(dat_mean,dat_cont,first,ix)
        }
      # use weights
      } else {
        print("Using weights")
        first<-T
        for (t in t_ok) {
          dat<-weights[t]*getValues(subset(s,subset=t))
          if (first) {
            dat_res<-dat
            dat_cont<-dat
            dat_cont[]<-NA 
            dat_cont[!is.na(dat)]<-1
            first<-F
          } else {
            if (argv$fun=="sum")  dat_res<-dat+dat_res
            if (argv$fun=="mean") {
              ix_nona<-which(!is.na(dat))
              ix_nonas<-which(!is.na(dat) & is.na(dat_cont))
              if (length(ix_nonas)>0) {
                dat_cont[ix_nonas]<-0
                dat_res[ix_nonas]<-0
              }
              if (length(ix_nona)>0) {
                dat_cont[ix_nona]<-dat_cont[ix_nona]+1
                dat_res[ix_nona]<-dat_res[ix_nona]+
                        (dat[ix_nona]-dat_res[ix_nona])/
                        dat_cont[ix_nona]
              }
              rm(ix_nona,ix_nonas)
            }
            if (argv$fun=="max")  dat_res<-pmax(dat,dat_res,na.rm=T)
            if (argv$fun=="min")  dat_res<-pmin(dat,dat_res,na.rm=T)
          }
        }
        r<-subset(s,subset=t_ok[1])
        r[]<-dat_res
      } # endif use weights
    } else {
      boom(paste("number of time steps available=",n," is less than required"))
    }
  }
} # endif exists "s"
#
#------------------------------------------------------------------------------
# Gridded output
if (gridded_output)  {
  # adjust 
  if (!exists("r")) r<-s
  rm(s)
  # write
  xy<-xyFromCell(r,1:ncell(r))
  x<-sort(unique(xy[,1]))
  y<-sort(unique(xy[,2]),decreasing=T)
  r.list<-list()
  grid<-array(data=NA,dim=c(length(x),length(y)))
  grid[,]<-matrix(data=r, ncol=length(y), nrow=length(x))
  r.list[[1]]<-grid
  rm(grid,r)
  out<-write_dotnc(grid.list=r.list,
                   times=format(strptime(argv$date_out,
                                         argv$date_out.format),"%Y%m%d%H%M"),
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
  print(paste("writing output file",argv$ffout,
              " / time",round(t1-t0,1),attr(t1-t0,"unit")))
} # endif exists "s"
#------------------------------------------------------------------------------
# Normal exit
q(status=0) 
