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
ffout_default<-"out.nc"

# -----------------------------------------------------------------------------
# FUNCTIONS

#+ Manage errors
boom<-function(str,status=1) { print(str); q(status=status) }

#==============================================================================
# MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN -
#==============================================================================
t0<-Sys.time()
# [] Command line arguments, parameter defaults -@@BEGIN@@ (jump to @@END@@)
# create parser object
p <- arg_parser("ffmrr")
#..............................................................................
p <- add_argument(p, "date1",
                  help="period start date (if \"none\" then date1 and date2 are derived from file)",
                  type="character")
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
p <- add_argument(p, "--ffin_date.file",
                  help="input date/time provided in a text file (rows with \"ffin_date.format\")",
                  type="character",
                  default=NA)
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
p <- add_argument(p, "--date_filter_by_month",
                  help="month(s) to consider, within the time period chosen",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--one_timestep_for_file",
                  help="read the first timestep from each file",
                  flag=T)
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
p <- add_argument(p, "--gridded_dqc.min_pad",
                  help="substitute values smaller than min with this value",
                  type="character",
                  default=NA)
p <- add_argument(p, "--gridded_dqc.max",
                  help="maximum allowed value",
                  type="numeric",
                  default=10000)
p <- add_argument(p, "--gridded_dqc.max_pad",
                  help="substitute values greater than max with this value",
                  type="character",
                  default=NA)
p <- add_argument(p, "--gridded_dqc.clump_r",
                  help="remove patches of connected cells that are too small. Set thresholds defining the patches.",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--gridded_dqc.clump_n",
                  help="remove patches of connected cells that are too small. Set minimum number of cells defining an acceptable patch.",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--gridded_dqc.clump_pad",
                  help="remove patches of connected cells that are too small. Set pad values.",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--gridded_dqc.outlier_aggfact",
                  help="check for outliers, aggregation factor (unit: number of cells)",
                  type="numeric",
                  default=5)
p <- add_argument(p, "--gridded_dqc.outlier_reflen",
                  help="check for outliers, reference length scale (unit: grid coord unit)",
                  type="numeric",
                  default=25000)
p <- add_argument(p, "--gridded_dqc.outlier_pad",
                  help="remove patches of connected cells that are too small. Set pad values.",
                  type="character",
                  default=NA)

#..............................................................................
p <- add_argument(p, "--time_aggregation",
                  help="aggregate data over time dimension (default=T if all others=F)",
                  flag=T)
p <- add_argument(p, "--time_aggregation_online",
                  help="online step-by-step aggregation, instead of reading all the files then aggregate",
                  flag=T)
#..............................................................................
p <- add_argument(p, "--time_cat",
                  help="concatenate files overt time",
                  flag=T)
#..............................................................................
p <- add_argument(p, "--summ_stat",
                  help="summary step-by-step statistics",
                  flag=T)
p<- add_argument(p, "--summ_stat_fun",
                 help="function applied (\"list_values\", \"wave_nrgx\", \"standard\", \"freqdist\", \"ellipsis\")",
                 type="character",
                 default="wave_nrgx")
p<- add_argument(p, "--ffout_summ_stat",
                 help="full file name for the summary statistics",
                 type="character",
                 default="summstat.txt")
p<- add_argument(p, "--ffout_summ_stat_proj4",
                 help="summary statistics proj4 (used by \"list_values\")",
                 type="character",
                 default=NA)
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
p<- add_argument(p, "--list_values_coord_rounddig",
                 help="rounding digits for summ_stat_fun = list_values",
                 type="numeric",
                 default=6)
p<- add_argument(p, "--list_values_rounddig",
                 help="rounding digits for summ_stat_fun = list_values",
                 type="numeric",
                 default=6)
p<- add_argument(p, "--list_values_min",
                 help="minimum allowed value",
                 type="character",
                 default=NA)
p<- add_argument(p, "--list_values_min_replace",
                 help="values smaller than min are replaced with this value",
                 type="character",
                 default=NA)
p<- add_argument(p, "--list_values_max",
                 help="maximum allowed value",
                 type="character",
                 default=NA)
p<- add_argument(p, "--list_values_max_replace",
                 help="values greater than max are replaced with this value",
                 type="character",
                 default=NA)
p<- add_argument(p, "--summ_stat_condition_fcells",
                 help="apply statistics on a selection of cases, where the field has at least \"ncells\" (or a fraction \"fcells\" of not NAs values) with a value higher than \"threshold\"",
                 type="numeric",
                 default=NA)
p <- add_argument(p, "--summ_stat_r",
                  help="thresholds, used for summ_stat = \"freqdist\"",
                  type="character",
                  default=NA,
                  nargs=Inf)
p<- add_argument(p, "--summ_stat_b",
                 help="type. used for summ_stat = \"freqdist\". One of 'below' (< x), 'below=' (<= x), '=within' (<= x <), 'within' (< x <), 'within=' (< x <=), '=within=' (<= x <=), 'above' (> x), or 'above=' (>= x). For threshold plots (ets, hit, within, etc) 'below/above' computes frequency below/above the threshold, and 'within' computes the frequency between consecutive thresholds",
                 type="character",
                 default="below")
#..............................................................................
p<- add_argument(p, "--ffout_summ_verif",
                 help="full file name for the summary statistics in verification mode",
                 type="character",
                 default="summverif.txt")
p<- add_argument(p, "--ffout_summ_verif_append",
                 help="append output",
                 flag=T)
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
                 help="weights used in the aggregation. If we call the weights specified by the user with w1, w2, ..., wN, then the result= w1_norm * field_1 + w2_norm * field_2 + ... + wN_norm * field_N. In the program the weights are normalized such that their sum is equal to 1, in other words w1_norm= w1/sum(w1,w2,...,wN), w2_norm= w2/sum(w1,w2,...,wN) and so on. NOTE: for the aggregation function \"radar_mean\" the weights are set by default to \"0.5,1,...,0.5\"",
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
p <- add_argument(p, "--r",
                  help="thresholds, used for fun = \"freq\" or \"count\", either one value or two values",
                  type="character",
                  default=NA,
                  nargs=Inf)
p<- add_argument(p, "--b",
                 help="type. used for summ_stat = \"freqdist\". One of 'below' (< x), 'below=' (<= x), '=within' (<= x <), 'within' (< x <), 'within=' (< x <=), '=within=' (<= x <=), 'above' (> x), or 'above=' (>= x). For threshold plots (ets, hit, within, etc) 'below/above' computes frequency below/above the threshold, and 'within' computes the frequency between consecutive thresholds",
                 type="character",
                 default="below")
#..............................................................................
p <- add_argument(p, "--master_trim",
                  help="should we apply \"trim\" function to the master?",
                  flag=T)
p <- add_argument(p, "--master_mask",
                  help="should we use the master grid to maskout gridpoints?",
                  flag=T)
#..............................................................................
p <- add_argument(p, "--polygon_mask",
                  help="mask out with polygons",
                  flag=T)
p<- add_argument(p, "--ffin_polygon_shp",
                 help="filename with polygons (shapefile)",
                 type="character",
                 default=NA)
p<- add_argument(p, "--polygon_layer",
                 help="layer name in the input file with polygons (shapefile)",
                 type="character",
                 default=NA)
p<- add_argument(p, "--polygon_data_field",
                 help="data field name in the input file with polygons (shapefile)",
                 type="character",
                 default=NA)
p<- add_argument(p, "--polygon_ids",
                 help="polygon IDs to use",
                 type="character",
                 default=NA,
                 nargs=Inf)
#..............................................................................
p <- add_argument(p, "--point_mask",
                  help="extract only a list of points",
                  flag=T)
p<- add_argument(p, "--point_mask_x",
                 help="list of easting-coordinates (negative: eg use _2 for -2)",
                 type="character",
                 default=NA,
                 nargs=Inf)
p<- add_argument(p, "--point_mask_y",
                 help="list of northing-coordinates (negative: eg use _2 for -2)",
                 type="character",
                 default=NA,
                 nargs=Inf)
p<- add_argument(p, "--point_mask_labels",
                 help="list of labels (negative: eg use _2 for -2)",
                 type="character",
                 default=NA,
                 nargs=Inf)
p<- add_argument(p, "--point_mask_proj4",
                 help="proj4 string",
                 type="character",
                 default=NA)
p<- add_argument(p, "--point_mask_method",
                 help="interpolation method (simple or bilinear)",
                 type="character",
                 default="simple")
#..............................................................................
p <- add_argument(p, "--verif",
                  help="verification against a reference dataset",
                  flag=T)
p<- add_argument(p, "--verif_metric",
                 help="verification score",
                 type="character",
                 default="bias")
p <- add_argument(p, "--verif_r",
                  help="thresholds",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p<- add_argument(p, "--verif_b",
                 help="type One of 'below' (< x), 'below=' (<= x), '=within' (<= x <), 'within' (< x <), 'within=' (< x <=), '=within=' (<= x <=), 'above' (> x), or 'above=' (>= x). For threshold plots (ets, hit, within, etc) 'below/above' computes frequency below/above the threshold, and 'within' computes the frequency between consecutive thresholds",
                 type="character",
                 default="below")
p<- add_argument(p, "--verif_corr_method",
                 help="verification, correlation method \"pearson\" (default), \"kendall\", \"spearman\"",
                 type="character",
                 default="pearson")
p <- add_argument(p, "--verif_seeps_threshold",
                  help="verification, seeps precip yes/no threshold (mm)",
                  type="numeric",
                  default=1)
p <- add_argument(p, "--verif_seeps_type",
                  help="verification, seeps error (default, 0-1, 0=the best) or skill-score (0-1, 1=the best). Special type: \"light_rain_threshold\" then the output is not SEEPS but the threshold used for distinction between light and heavy rainfall",
                  type="character",
                  default="error")
p <- add_argument(p, "--verif_roblinreg_threshold",
                  help="verification, roblinreg precip yes/no threshold (mm)",
                  type="numeric",
                  default=1)
p <- add_argument(p, "--verif_roblinreg_type",
                  help="verification, condition to apply to threshold.",
                  type="character",
                  default="above=")
p <- add_argument(p, "--verif_contab_threshold",
                  help="verification, contingency table for binary events (a,b,c,d,ets) threshold defining the event",
                  type="numeric",
                  default=1)
p <- add_argument(p, "--verif_contab_threshold1",
                  help="verification, contingency table for binary events (a,b,c,d,ets) additional threshold defining the event",
                  type="numeric",
                  default=1)
p <- add_argument(p, "--verif_contab_type",
                  help="verification, contingency table for binary events (see \"--verif_b\"). Special labels: \"wet\" (event=yes if above= threshold), \"dry\" (below threshold), \"light\" (threshold =within light_threshold, where light_threshold is the value that includes the lowest 2/3 of the wet-precipitation values), \"heavy\" (above light_threshold) ",
                  type="character",
                  default="error")

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
# master grid (digital elevation model)
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
#
p<- add_argument(p, "--ffin_ref_template",
                 help="path to + name (template) of the input observation files",
                 type="character",
                 default=NA)
p<- add_argument(p, "--ffin_ref_template_alternative",
                 help="path to + name (template) of the input observation files",
                 type="character",
                 default=NA)
p <-  add_argument(p, "--tseq_ref_hour_offset",
                  help="hour offset (this will affect the filename)",
                  type="numeric",
                  default=0)
p <- add_argument(p, "--ffin_ref_hour_offset",
                  help="hour offset (this will affect the netcdf timestep readed, but not the filename)",
                  type="numeric",
                  default=0)
p <- add_argument(p, "--ffin_ref_varname",
                  help="variable name in the netCDF file",
                  type="character",
                  default="land_area_fraction")
p <- add_argument(p, "--ffin_ref_topdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the fg upside down",
                  flag=T)
p <- add_argument(p, "--ffin_ref_ndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3)
p <- add_argument(p, "--ffin_ref_tpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3)
p <- add_argument(p, "--ffin_ref_epos",
                  help="position of the dimension ''ensemble'' in the netCDF file",
                  type="numeric",
                  default=3)
p <- add_argument(p, "--ffin_ref_dimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--ffin_ref_proj4",
                  help="proj4 string",
                  type="character",
                  default="projection_lambert")
p <- add_argument(p, "--ffin_ref_proj4_var",
                  help="variable that include the specification of the proj4 string",
                  type="character",
                  default="projection_lambert")
p <- add_argument(p, "--ffin_ref_proj4_att",
                  help="attribute with the specification of the proj4 string",
                  type="character",
                  default="proj4")
p <- add_argument(p, "--ffin_ref_e",
                  help="ensemble member to read in the netCDF file",
                  type="numeric",
                  default=NA)
# output file
p<- add_argument(p, "--ffout",
                 help="output file",
                 type="character",
                 default=ffout_default)
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
                 help="string, placeholder for year",
                 type="character",
                 default="yyyy")
p<- add_argument(p, "--month_string",
                 help="string, placeholder for month",
                 type="character",
                 default="mm")
p<- add_argument(p, "--day_string",
                 help="string, placeholder for day",
                 type="character",
                 default="dd")
p<- add_argument(p, "--hour_string",
                 help="string, placeholder for hour",
                 type="character",
                 default="hh")
p<- add_argument(p, "--min_string",
                 help="string, placeholder for minute",
                 type="character",
                 default="MM")
p<- add_argument(p, "--sec_string",
                 help="string, placeholder for second",
                 type="character",
                 default="SS")
#..............................................................................
p <- add_argument(p, "--pam",
                  help="plot a map",
                  flag=T)
p<- add_argument(p, "--ffout_pam_template",
                 help="path to + name (template) of the output png file",
                 type="character",
                 default=NA)
p<- add_argument(p, "--pam_width",
                 help="png resolution, width",
                 type="numeric",
                 default=800)
p<- add_argument(p, "--pam_height",
                 help="png resolution, height",
                 type="numeric",
                 default=800)
p<- add_argument(p, "--pam_ffshp_borders",
                 help="path to + name to the borders (shapefile)",
                 type="character",
                 default=NA)
p<- add_argument(p, "--pam_ffshp_borders_layer",
                 help="layer",
                 type="character",
                 default=NA)
p<- add_argument(p, "--pam_ffshp_borders_lwd",
                 help="borders line width",
                 type="character",
                 default=NA)
p<- add_argument(p, "--pam_fool_path",
                 help="path to the fool library (color_table dir included)",
                 type="character",
                 default=NA)
p<- add_argument(p, "--pam_fool_coltab",
                 help="fool color table abbreviation",
                 type="character",
                 default=NA)
p<- add_argument(p, "--pam_fool_coltab_rev",
                 help="reverse fool color table",
                 flag=T)
p <- add_argument(p, "--pam_fool_breaks",
                  help="breaks when pam_fool_coltab is specified. breaks are specified as: range to plot (min max) and one can specify the number of ticks in the legend (pam_leg_cbar_nticks)",
                  type="numeric",
                  default=NA,
                  nargs=2)
p <- add_argument(p, "--pam_colors",
                  help="colors.",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--pam_breaks",
                  help="breaks. if specified has the priority over \"pam_fool_breaks\".",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--pam_disagg_fact",
                  help="plot a more detailed map. the field is refined using bilinear interpolation",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--pam_xlim",
                  help="x-coordinate range (min max)",
                  type="numeric",
                  default=NA,
                  nargs=2)
p <- add_argument(p, "--pam_ylim",
                  help="y-coordinate range (min max)",
                  type="numeric",
                  default=NA,
                  nargs=2)
p<- add_argument(p, "--pam_leg_type",
                 help="legend yes/no and type (NA no legend). types: \"color_bar\" (use when colors are specified through \"pam_fool_coltab\"), \"color_bar_mybreaks\".",
                 type="character",
                 default=NA)
p<- add_argument(p, "--pam_leg_dig",
                 help="legend round to significant digits",
                 type="numeric",
                 default=2)
p<- add_argument(p, "--pam_leg_cbar_nticks",
                 help="number of thicks when pam_leg_type is set to \"color_bar\"",
                 type="numeric",
                 default=11)
p<- add_argument(p, "--pam_leg_height",
                 help="legend height (\"standard\", \"wide\")",
                 type="character",
                 default="standard")
#..............................................................................
p <- add_argument(p, "--verbose",
                  help="verbose mode",
                  flag=T)
p <- add_argument(p, "--debug",
                  help="debug mode",
                  flag=T)
#..............................................................................
argv <- parse_args(p)
# Command line arguments, parameter defaults - @@END@@
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
# Load functions
for (file in list.files(path = file.path(argv$spider_path,"lib"),
                        pattern = ".r", full.names=T) ) 
  source(file)
rm(file)
#------------------------------------------------------------------------------
# default is time_aggregation
if ( !argv$time_aggregation & 
     !argv$time_cat & 
     !argv$upscale & 
     !argv$downscale & 
     !argv$latte &
     !argv$summ_stat & 
     !argv$pam & 
     !argv$verif ) 
  argv$time_aggregation<-T
#
gridded_output<-F
if ( argv$time_aggregation | 
     argv$time_cat | 
     argv$upscale | 
     argv$downscale | 
     argv$latte | 
    (argv$verif & argv$ffout!=ffout_default) ) 
  gridded_output<-T
#
if (argv$summ_stat & argv$summ_stat_fun=="wave_nrgx") 
  suppressPackageStartupMessages(library("waveslim"))
if (argv$summ_stat & argv$summ_stat_fun=="ellipsis") 
  {suppressPackageStartupMessages(library("cluster"))
   suppressPackageStartupMessages(library("igraph"))}
argv$summ_stat_condition_threshold<-as.numeric(gsub("_","-",
                                      argv$summ_stat_condition_threshold))
argv$point_mask_x<-as.numeric(gsub("_","-",argv$point_mask_x))
argv$point_mask_y<-as.numeric(gsub("_","-",argv$point_mask_y))
#
# convert character to numbers
if (any(!is.na(argv$summ_stat_r))) {
  dots<-any(argv$summ_stat_r=="...")
  if ( any(argv$summ_stat_r=="...") ) {
    start<-as.numeric(gsub("_","-",argv$summ_stat_r[1]))
    by<-as.numeric(gsub("_","-",argv$summ_stat_r[2]))-start
    stop<-as.numeric(gsub("_","-",argv$summ_stat_r[4]))
    argv$summ_stat_r<-seq(start,stop,by=by)
    rm(start,stop,by)    
  } else {
    aux<-vector(mode="numeric",length=length(argv$summ_stat_r))
    for (i in 1:length(argv$summ_stat_r)) 
      aux[i]<-as.numeric(gsub("_","-",argv$summ_stat_r[i]))
    argv$summ_stat_r<-aux
    rm(aux)
  }
}

if (any(!is.na(argv$verif_r))) {
  dots<-any(argv$verif_r=="...")
  if ( any(argv$verif_r=="...") ) {
    start<-as.numeric(gsub("_","-",argv$verif_r[1]))
    by<-as.numeric(gsub("_","-",argv$verif_r[2]))-start
    stop<-as.numeric(gsub("_","-",argv$verif_r[4]))
    argv$verif_r<-seq(start,stop,by=by)
    rm(start,stop,by)    
  } else {
    aux<-vector(mode="numeric",length=length(argv$verif_r))
    for (i in 1:length(argv$verif_r)) 
      aux[i]<-as.numeric(gsub("_","-",argv$verif_r[i]))
    argv$verif_r<-aux
    rm(aux)
  }
}

if (any(!is.na(argv$r))) {
  aux<-vector(mode="numeric",length=length(argv$r))
  for (i in 1:length(argv$r)) aux[i]<-as.numeric(gsub("_","-",argv$r[i]))
  argv$r<-aux
  rm(aux)
}

if (any(!is.na(argv$gridded_dqc.clump_r))) {
  aux<-vector(mode="numeric",length=length(argv$gridded_dqc.clump_r))
  for (i in 1:length(argv$gridded_dqc.clump_r)) 
    aux[i]<-as.numeric(gsub("_","-",argv$gridded_dqc.clump_r[i]))
  argv$gridded_dqc.clump_r<-aux
  rm(aux)
  if (length(argv$gridded_dqc.clump_r)!=length(argv$gridded_dqc.clump_n)) 
    boom(paste0("gridded_dqc clum check. number of thresholds \"r\" is ",
                length(argv$gridded_dqc.clump_r),
                " while number of thresholds \"n\" is ",
                length(argv$gridded_dqc.clump_n)))
  if (length(argv$gridded_dqc.clump_r)!=length(argv$gridded_dqc.clump_pad)) 
    boom(paste0("gridded_dqc clum check. number of thresholds \"r\" is ",
                length(argv$gridded_dqc.clump_r),
                " while number of thresholds \"pad\" is ",
                length(argv$gridded_dqc.clump_pad)))
  aux<-vector(mode="numeric",length=length(argv$gridded_dqc.clump_pad))
  for (i in 1:length(argv$gridded_dqc.clump_pad)) 
    aux[i]<-as.numeric(gsub("_","-",argv$gridded_dqc.clump_pad[i]))
  argv$gridded_dqc.clump_pad<-aux
  rm(aux)
}

if (!is.na(argv$list_values_min)) 
  argv$list_values_min<-as.numeric(gsub("_","-",argv$list_values_min))
if (!is.na(argv$list_values_max)) 
  argv$list_values_max<-as.numeric(gsub("_","-",argv$list_values_max))
if (!is.na(argv$list_values_min_replace)) 
  argv$list_values_min_replace<-as.numeric(gsub("_","-",
                                argv$list_values_min_replace))
if (!is.na(argv$list_values_max_replace)) 
  argv$list_values_max_replace<-as.numeric(gsub("_","-",
                                argv$list_values_max_replace))
if (!is.na(argv$gridded_dqc.min_pad)) 
  argv$gridded_dqc.min_pad<-as.numeric(gsub("_","-",argv$gridded_dqc.min_pad))
if (!is.na(argv$gridded_dqc.max_pad)) 
  argv$gridded_dqc.max_pad<-as.numeric(gsub("_","-",argv$gridded_dqc.max_pad))
if (!is.na(argv$gridded_dqc.outlier_pad)) 
  argv$gridded_dqc.outlier_pad<-as.numeric(gsub("_","-",
                                argv$gridded_dqc.outlier_pad))
#-----------------------------------------------------------------------------
# Multi-cores run
if (!is.na(argv$cores)) {
  suppressPackageStartupMessages(library("parallel"))
  if (argv$cores==0) argv$cores <- detectCores()
  print(paste("--> multi-core run, cores=",argv$cores))
}
#------------------------------------------------------------------------------
# Time sequence
if (!is.na(argv$ffin_date.file)) {
  if (!file.exists(argv$ffin_date.file))
     boom(paste0("file not found",argv$ffin_date.file))
  tin<-read.table(file=argv$ffin_date.file,header=F,stringsAsFactors=F,strip.white=T)
  tseq<-as.POSIXlt(str2Rdate(tin$V1,argv$ffin_date.format),tz="UTC")
} else {
  if (argv$date1=="none") {
    if (!file.exists(argv$ffin_template))
       boom(paste0("file not found",argv$ffin_template))
    tseq<-as.POSIXlt(str2Rdate(nc4.getTime(argv$ffin_template),
                     format="%Y%m%d%H%M"),tz="UTC")
  } else {
    if (argv$date2=="none") {
      if ( is.na(argv$time_n_prev) & 
           is.na(argv$time_n_succ) ) boom(paste0("error in date definition"))
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
        aux<-rev(seq(strptime(argv$date1,format=argv$date.format),
                              length=argv$time_n_succ,
                              by=paste(argv$time_step,argv$time_unit)))
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
    if (!is.na(argv$ffin_ref_template)) {
      if (argv$tseq_ref_hour_offset==0) {
        tseq_ref<-tseq
      } else {
        tseq_ref<-as.POSIXlt(as.POSIXct(tseq,tz="GMT")+
                             argv$tseq_ref_hour_offset*3600,tz="GMT")
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
  print("time sequence")
  print(tseq)
  print(paste("number of time steps =",n_tseq))
}
#------------------------------------------------------------------------------
# -.- Main loop overt time -.-
#  Read Input files / elaboration
# loop over all the n_tseq times, however some timesteps may not be used
# used timesteps are "n" and they are stored into t_ok (n-vector)
#  t_ok[i] is the i-th index to the tseq element used
t_ok<-vector()
n<-0
first<-T
for (t in 1:n_tseq) { # MAIN LOOP @@BEGIN@@ (jump to @@END@@)
  if (argv$verbose & t%%100==0) 
    print(paste("timestep",t,"/",n_tseq,
          "elapsed time",round(Sys.time()-t0,2),attr(Sys.time()-t0,"unit")))
  ffin<-replaceDate(string=argv$ffin_template,
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
  #----------------------------------------------------------------------------
  # read reference file
  if (!is.na(argv$ffin_ref_template)) {
    ffin_ref<-replaceDate(string=argv$ffin_ref_template,
                          date.str=format(tseq_ref[t],
                                    format=argv$ffin_date.format,tz="GMT"),
                          year_string=argv$year_string,
                          month_string=argv$month_string,
                          day_string=argv$day_string,
                          hour_string=argv$hour_string,
                          sec_string=argv$sec_string,
                          format=argv$ffin_date.format)
    if (!file.exists(ffin_ref)) {
      print(paste("file not found",ffin_ref))
      next
    }
    t_to_read<-format(
                as.POSIXct(
                 as.numeric(
      as.POSIXct(tseq[t],format=argv$ffin_date.format,tz="GMT")) 
      + argv$ffin_hour_offset*3600, origin="1970-01-01",tz="GMT"),
                "%Y%m%d%H%M%S")
    if (argv$debug) print(paste("time_to_read time file",t_to_read,tseq[t],ffin_ref))
    r_ref<-read_griddeddata(mode="ref")
    # case of problems while reading the input file (e.g., missing timestep)
    if (is.null(r_ref)) {
      print(paste("warning: problem while reading time file",t_to_read,ffin_ref))
      next
    }
    if (!any(!is.na(values<-getValues(r_ref)))) {
      print(paste("warning: all NAs for time file",t_to_read,ffin_ref))
      next
    }
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
    if (argv$master_trim) r1<-trim(r1)
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
  # Use the mask(s) if needed
  if (argv$master_mask & !argv$latte & !argv$downscale & !argv$upscale) {
    if (!exists("rmaster")) {
      rmaster<-read_griddeddata("master")
      if (is.null(rmaster)) boom("ERROR problem reading master grid")
    }
    r<-mask(r,mask=rmaster)
    if (!any(!is.na(values<-getValues(r)))) {
      print(paste("warning: all NAs after mask"))
      next
    }
  }
  #
  if (argv$polygon_mask) {
    if (!file.exists(argv$ffin_polygon_shp)) {
      print(paste("warning: file not found",argv$ffin_polygon_shp))
      next
    }
    poly<-suppressWarnings(suppressMessages(readOGR(argv$ffin_polygon_shp,argv$polygon_layer,
           stringsAsFactors=F,verbose=F)))
    if (as.character(poly@proj4string)!=as.character(crs(r))) poly<-spTransform(poly,crs(r))
    if (any(!is.na(argv$polygon_ids))) {
      pix<-which(names(poly@data)==argv$polygon_data_field)
      if (length(pix)==0) 
        boom(paste("Error while reading shapefile",argv$ffin_polygon_shp,
                   "layer",argv$polygon_layer,
                   "data field",argv$polygon_data_field,
                   ", please use one of",toString(names(poly@data))))
      if (length(ix<-which(poly@data[,pix] %in% as.character(argv$polygon_ids)))>0) {
        poly<-poly[ix,]
      } else {
        print("warning: the shapefile doe not contain the dpscified IDs. Skip this timestep")
        next
      }
    } 
    r<-mask(r,poly)
    if (!any(!is.na(values<-getValues(r)))) {
      print(paste("warning: all NAs after polygon mask"))
      next
    }
  }
  #----------------------------------------------------------------------------
  # Extract a list of points
  if (argv$point_mask) {
    if ( any(is.na(argv$point_mask_x)) | any(is.na(argv$point_mask_y)) |
         length(argv$point_mask_x)!=length(argv$point_mask_y) |
         length(argv$point_mask_x)!=length(argv$point_mask_labels) ) {
      print("warning: something is wrong with the list of points")
      print(argv$point_mask_x)
      print(argv$point_mask_y)
      print(argv$point_mask_label)
      next
    }
    if (argv$point_mask_proj4!=as.character(crs(r))) { 
      coord.new<- attr( spTransform( SpatialPoints(cbind(argv$point_mask_x,
                                                         argv$point_mask_y),
                                                         proj4string=CRS(argv$point_mask_proj4)),
                               crs(r)), "coords")
      point_x<-coord.new[,1]
      point_y<-coord.new[,2]
    } else {
      point_x<-argv$point_mask_x
      point_y<-argv$point_mask_y
    }
    if (!any(!is.na( values<-extract(r,cbind(point_x,point_y),
                                     method=argv$point_mask_method) ) ) ) {
      print(paste("warning: all NAs after point mask"))
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
  if (argv$gridded_dqc) {
    # check for unplausible values
    if (!is.na(argv$gridded_dqc.min) & !is.na(argv$gridded_dqc.max)) {
      rval<-getValues(r)
      r[which(rval<argv$gridded_dqc.min)]<-argv$gridded_dqc.min_pad
      r[which(rval>argv$gridded_dqc.max)]<-argv$gridded_dqc.max_pad
      rm(rval)
    }
    # remove small patches of connected cells
    if (!any(is.na(argv$gridded_dqc.clump_r)) & 
        !(any(is.na(argv$gridded_dqc.clump_n)))) {
      rval<-getValues(r)
      suppressPackageStartupMessages(library("igraph"))
      for (i in 1:length(argv$gridded_dqc.clump_r)) {
        raux<-r
        if (any(rval<=argv$gridded_dqc.clump_r[i])) 
          raux[which(rval<=argv$gridded_dqc.clump_r[i])]<-NA
        rclump<-clump(raux)
        fr<-freq(rclump)
        ix<-which( !is.na(fr[,2]) & 
                   fr[,2]<=argv$gridded_dqc.clump_n[i] )
        if (length(ix)>0) {
          rval[getValues(rclump) %in% fr[ix,1]]<-argv$gridded_dqc.clump_pad[i]
          r[]<-rval
        }
        rm(raux,fr,ix,rclump)
      }
    }
    if (!is.na(argv$gridded_dqc.outlier_aggfact)) {
      # c. remove outliers. Check for outliers in square boxes
      t0a<-Sys.time()
      raux<-r
      daux<-boxcox(x=rval,lambda=0.5)
      raux[]<-daux
      # compute mean and sd on a coarser grid
      raux_agg<-aggregate(raux,
                          fact=argv$gridded_dqc.outlier_aggfact,
                          fun=mean,
                          na.rm=T)
      daux_agg<-getValues(raux_agg)
      ix_aux<-which(!is.na(daux_agg))
      xyaux<-xyFromCell(raux_agg,ix_aux)
      xrad_aux<-xyaux[,1]
      yrad_aux<-xyaux[,2]
      vrad_aux<-daux_agg[ix_aux]
      get_rad_stat<-function(i,dh_ref=25000) { 
        deltax<-abs(xrad_aux[i]-xrad_aux)
        if (length(ix<-which( deltax<dh_ref ))==0) return(NA,NA)
        deltay<-abs(yrad_aux[i]-yrad_aux[ix])
        if (length(iy<-which( deltay<dh_ref ))==0) return(NA,NA)
        ix<-ix[iy]
#        dist<-deltax; dist[]<-NA
#        dist[ix]<-sqrt(deltax[ix]*deltax[ix]+deltay[ix]*deltay[ix])
#        ix<-which(dist<dh_ref)
        return(c(mean(vrad_aux[ix]),sd(vrad_aux[ix])))
      }
      if (!is.na(argv$cores)) {
        arr<-mcmapply(get_rad_stat,
                      1:length(ix_aux),
                      mc.cores=argv$cores,
                      SIMPLIFY=T,
                      dh_ref=argv$gridded_dqc.outlier_reflen)
      # no-multicores
      } else {
        arr<-mapply(get_rad_stat,
                    1:length(ix_aux),
                    SIMPLIFY=T,
                    dh_ref=argv$gridded_dqc.outlier_reflen)
      }
      # disaggregate mean and sd on the original grid
      raux_agg[]<-NA
      raux_agg[ix_aux]<-arr[1,] # mean
      raux<-disaggregate(raux_agg,
                         fact=argv$gridded_dqc.outlier_aggfact,
                         method="bilinear",
                         na.rm=T)
      if (ncell(raux)>ncell(r)) {
        raux<-crop(raux,r)
      } else if (ncell(raux)<ncell(r)) {
        raux<-extend(raux,r)
      }
      avg<-getValues(raux)
      raux_agg[]<-NA
      raux_agg[ix_aux]<-arr[2,] # sd
      raux<-disaggregate(raux_agg,
                         fact=argv$gridded_dqc.outlier_aggfact,
                         method="bilinear",
                         na.rm=T)
      if (ncell(raux)>ncell(r)) {
        raux<-crop(raux,r)
      } else if (ncell(raux)<ncell(r)) {
        raux<-extend(raux,r)
      }
      stdev<-getValues(raux)
      # check for outliers
      ix<-which(stdev>0 & !is.na(daux) & !is.na(avg) & !is.na(stdev))
      rm(arr,raux_agg,ix_aux,xrad_aux,yrad_aux,vrad_aux,daux_agg,xyaux)
      # outliers are defined as in Lanzante,1997: abs(value-mean)/st.dev > 5
      suspect<-which( ( abs(daux[ix]-avg[ix]) / stdev[ix] ) > 5 ) 
      if (length(suspect)>0) rval[ix[suspect]]<-argv$gridded_dqc.outlier_pad
      r[]<-rval
      rm(raux,daux,avg,stdev,ix,suspect,rval)
      t1a<-Sys.time()
      print(paste(" remove outliers - time",round(t1a-t0a,1),
                                            attr(t1a-t0a,"unit")))
    }
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
    #--------------------------------------------------------------------------
    # argv$summ_stat_fun=="list_values"
    if (argv$summ_stat_fun=="list_values") {
      if (!file.exists(argv$ffout_summ_stat) | 
          (!argv$ffout_summ_stat_append & first)) {
        cat(file=argv$ffout_summ_stat,append=F,
            paste0("time;label;x;y;value\n"))
      }
      if (any(is.na(argv$point_mask_x))) {
        labels<-1:ncell(r)
        xy<-xyFromCell(r,1:ncell(r))
        x<-xy[,1]
        y<-xy[,2]
      } else {
        labels<-argv$point_mask_labels
        x<-round(argv$point_mask_x,6)
        y<-round(argv$point_mask_y,6)
      }
      if ( !is.na(argv$ffout_summ_stat_proj4) & 
           argv$ffout_summ_stat_proj4!=argv$ffin_proj4) {
        coord.new<-attr(spTransform( 
                        SpatialPoints(cbind(x,y),
                                      proj4string=CRS(argv$ffin_proj4)) 
                                                 ,CRS(argv$ffout_summ_stat_proj4)),
                   "coords")
        x<-coord.new[,1]
        y<-coord.new[,2]
      }
      if (!is.na(argv$list_values_min)) 
        values[which(values<argv$list_values_min)]<-argv$list_values_min_replace
      if (!is.na(argv$list_values_max)) 
        values[which(values>argv$list_values_max)]<-argv$list_values_max_replace
      cat(file=argv$ffout_summ_stat,append=T,
          paste0(t_to_read,";",
                 labels,";",
                 round(x,argv$list_values_coord_rounddig),";",
                 round(y,argv$list_values_coord_rounddig),";",
                 round(values,argv$list_values_rounddig),"\n",collapse=""))
    #--------------------------------------------------------------------------
    # argv$summ_stat_fun=="wave_nrgx"
    } else if (argv$summ_stat_fun=="wave_nrgx") {
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
    #--------------------------------------------------------------------------
    # argv$summ_stat_fun=="standard"
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
    #--------------------------------------------------------------------------
    # argv$summ_stat_fun=="freqdist"
     else if (argv$summ_stat_fun=="freqdist") {
      # first time in, define variables
      # NOTE: case of "within", num is a vector with dimension nr-1
      #       otherwise, num is a vector with dimension nr
      if (first) {
        header_string<-integer(0)
        nr<-length(argv$summ_stat_r)
        for (i in 1:nr) {
          if (argv$summ_stat_b=="=within" | argv$summ_stat_b=="within" |
              argv$summ_stat_b=="within=" | argv$summ_stat_b=="=within=") {
            if (i<nr) header_string<-paste0(header_string,
                                            paste0(";num_",argv$summ_stat_b,"_",
                                                   argv$summ_stat_r[i],"_",
                                                   argv$summ_stat_r[(i+1)]))
          } else {
            header_string<-paste0(header_string,
                                  paste0(";num_",argv$summ_stat_b,"_",
                                         argv$summ_stat_r[i]))
          }
        }
        header_string<-paste0(header_string,";numtot")
        if (argv$summ_stat_b=="=within" | argv$summ_stat_b=="within" |
            argv$summ_stat_b=="within=" | argv$summ_stat_b=="=within=") {
          num<-vector(mode="numeric",length=(nr-1))
        } else {
          num<-vector(mode="numeric",length=nr)
        }
        num[]<-NA
      }
      # if required, write the file header
      if (!file.exists(argv$ffout_summ_stat) | 
           (!argv$ffout_summ_stat_append & first)) {
         cat(file=argv$ffout_summ_stat,append=F,
             paste0("time",header_string,"\n"))
      }
      # count the number of cases 
      val<-getValues(r)
      numtot<-length(which(!is.na(val)))
      val<-val[which(!is.na(val))]
      num[]<-NA
      for (i in 1:nr) {
        if ( argv$summ_stat_b %in% c("within","=within","within=","=within=") ) {
          if (i<nr) num[i]<-score_fun(x=val,
                                      lab="count_x",
                                      threshold=argv$summ_stat_r[i],
                                      threshold1=argv$summ_stat_r[(i+1)],
                                      type=argv$summ_stat_b)
        } else { 
          num[i]<-score_fun(x=val,
                            lab="count_x",
                            threshold=argv$summ_stat_r[i],
                            type=argv$summ_stat_b)
        }
      }
      if (exists("ix")) rm(ix)
      rm(val)
      # output
      data_string<-integer(0)
      for (i in 1:nr) {
        if (argv$summ_stat_b=="=within" | argv$summ_stat_b=="within" |
            argv$summ_stat_b=="within=" | argv$summ_stat_b=="=within=") {
          if (i<nr) data_string<-paste0(data_string,";",num[i])
        } else {
          data_string<-paste0(data_string,";",num[i])
        }
      }
      data_string<-paste0(data_string,";",numtot)
      cat(file=argv$ffout_summ_stat,append=T,
          paste0(t_to_read,data_string,"\n"))
    #endif summ_stat_fun=="freqdist"
    #--------------------------------------------------------------------------
    # argv$summ_stat_fun=="ellipsis"
    } else if (argv$summ_stat_fun=="ellipsis") {
      dat<-getValues(r)
      ix<-score_fun(x=dat,
                    lab="index_x",
                    threshold=argv$summ_stat_r,
                    type=argv$summ_stat_b) 
      dat[]<-NA
      dat[ix]<-1
      r[]<-dat
      r<-clump(r,directions=8,gaps=F)
      clump_lab<-freq(r)
      ix_clump_big<-which(!is.na(clump_lab[,1]) & clump_lab[,2]>100)
      clump_lab_val<-clump_lab[ix_clump_big,1]
      clump_lab_n<-clump_lab[ix_clump_big,2]
      ix_r_clump_small<-which(!(getValues(r) %in% clump_lab_val))
      r[ix_r_clump_small]<-NA
      dat<-getValues(r)
      xy<-xyFromCell(r,1:ncell(r))
      x<-xy[,1]
      y<-xy[,2]
      n_clump<-length(clump_lab_val)
      ell_x<-vector(mode="numeric",length=n_clump); ell_x[]<-NA
      ell_y<-vector(mode="numeric",length=n_clump); ell_y[]<-NA
      ell_smajor<-vector(mode="numeric",length=n_clump); ell_smajor[]<-NA
      ell_sminor<-vector(mode="numeric",length=n_clump); ell_sminor[]<-NA
      ell_smadir_eve<-vector(mode="numeric",length=n_clump); ell_smadir_eve[]<-NA
#png(file=paste0("ellipsis_",t_to_read,".png"),width=800,height=800)
#image(r)
      ell_list_t<-list()
      for (i in 1:n_clump) {
        ixi<-which(dat==clump_lab_val[i])
        xy<-cbind(x[ixi],y[ixi])
        ell<-ellipsoidhull(xy)
        ell_list_t[[i]]<-ell
        ell_x[i]<-ell$loc[1]
        ell_y[i]<-ell$loc[2]
        eigenval<-eigen(ell$cov)$values
        eigenvec<-eigen(ell$cov)$vectors
        e <- sqrt(eigenval)
        ell_smajor[i] <- sqrt(ell$d2) * e[1] /1000  # semi-major axis [Km]
        ell_sminor[i] <- sqrt(ell$d2) * e[2] /1000  # semi-minor axis [Km]
        # orientation of the ellipse: angle between the major axis and the y-axis
        # dir=0 N-S orientation; dir=45 NE-SW; dir=90 E-W; dir=135 NW-SE; dir=180 N-S
        ell_smadir_eve[i]<-atan2(eigenvec[1,1],eigenvec[2,1])
        if (ell_smadir_eve[i]<0) ell_smadir_eve[i]<-ell_smadir_eve[i]+pi
        ell_smadir_eve[i]<-ell_smadir_eve[i]/pi*180.
#lines(predict(ell))
      }
      if (!exists("ell_list")) ell_list<-list()
      ell_list[[t]]<-ell_list_t
#dev.off()
#next
    } 
    #endif summ_stat_fun=="ellipsis"
  } # end if summary statistics
  #----------------------------------------------------------------------------
  # prepare for verification statistics 
  if (argv$verif) {
    if (gridded_output) {
      # scores that require to store the whole dataset in memory
      if (argv$verif_metric %in% c("corr","msess","ets","a","b","c","d","seeps","roblinreg")) {
        if (!exists("mat")) {
          ix_ver<-which(!is.na(getValues(r)) & !is.na(getValues(r_ref)))
          mat<-getValues(r)[ix_ver]
          mat_ref<-getValues(r_ref)[ix_ver]
          npoints<-length(mat_ref)
          if (!exists("rmaster")) {rmaster<-r; rmaster[]<-NA}
        } else {
          mat<-cbind(mat,getValues(r)[ix_ver])
          mat_ref<-cbind(mat_ref,getValues(r_ref)[ix_ver])
        }
      # scores that are computed online
      } else {
        if ( argv$verif_metric %in% c("mbias","rmsf") )
          r<-r/r_ref
        if ( argv$verif_metric %in% c("bias","mae","rmse") )
          r<-r-r_ref
        if (!exists("ix_ver")) ix_ver<-which(!is.na(getValues(r)))
        dat<-getValues(r)[ix_ver]
        if ( argv$verif_metric=="mae" ) 
          dat<-abs(dat)
        if ( argv$verif_metric %in% c("rmse","rmsf") ) 
          dat<-dat**2
        if (!exists("dat_mean")) {
          if (!exists("rmaster")) {rmaster<-r; rmaster[]<-NA}
          dat_mean<-dat
          dat_cont<-dat
          dat_cont[]<-NA 
          dat_cont[!is.na(dat)]<-1
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
                    (dat[ix_nona]-dat_mean[ix_nona])/
                    dat_cont[ix_nona]
          }
          rm(ix_nona,ix_nonas)
        }
        rm(dat)
      }
    } # end if gridded output is TRUE
      # compute verif statistics on a step-by-step basis
      else {
      # first time in, define variables
      # NOTE: case of "within", num is a vector with dimension nr-1
      #       otherwise, num is a vector with dimension nr
      if (first) {
        header_string<-integer(0)
        nr<-length(argv$verif_r)
        if (nr>0) {
          for (i in 1:nr) {
            if (argv$verif_b=="=within" | argv$verif_b=="within" |
                argv$verif_b=="within=" | argv$verif_b=="=within=") {
              if (i<nr) header_string<-paste0(header_string,
                                              paste0(";thr_",argv$verif_b,"_",
                                                     argv$verif_r[i],"_",
                                                     argv$verif_r[(i+1)]))
            } else {
              header_string<-paste0(header_string,
                                    paste0(";thr_",argv$verif_b,"_",
                                           argv$verif_r[i]))
            }
          }
          header_string<-paste0(header_string,";numtot")
          if (argv$verif_b=="=within" | argv$verif_b=="within" |
              argv$verif_b=="within=" | argv$verif_b=="=within=") {
            score<-vector(mode="numeric",length=(nr-1))
          } else {
            score<-vector(mode="numeric",length=nr)
          }
          score[]<-NA
        # no thresolds
        } else {
          header_string<-paste0(";score;numtot")
          score<-NA
        }
      }
      # if required, write the file header
      if (!file.exists(argv$ffout_verif) | 
           (!argv$ffout_verif_append & first)) {
         cat(file=argv$ffout_verif,append=F,
             paste0("time",header_string,"\n"))
      }
      # compute score
      val<-getValues(r)
      ref<-getValues(r_ref)
      numtot<-length(aux<-which(!is.na(val) & !is.na(ref)))
      val<-val[ix]
      ref<-ref[ix]; rm(ix)
      # use thresholds if needed
      if (nr>0) {
        score[]<-NA
        for (i in 1:nr) {
          if ( argv$verif_b %in% c("within","=within","within=","=within=") ) {
            if (i<nr) score[i]<-score_fun(x=val,
                                          x_ref=ref,
                                          lab=argv$verif_metric,
                                          threshold=argv$verif_r[i],
                                          threshold1=argv$verif_r[(i+1)],
                                          type=argv$verif_b)
          } else { 
            score[i]<-score_fun(x=val,
                                x_ref=ref,
                                lab=argv$verif_metric,
                                threshold=argv$verif_r[i],
                                type=argv$verif_b)
          }
        }
      } else {
        score<-score_fun(x=val,
                         x_ref=ref,
                         lab=argv$verif_metric)
      } # end compute score
      rm(val,ref)
      # output
      if (nr>0) {
        data_string<-integer(0)
        for (i in 1:nr) {
          if ( argv$verif_b %in% c("within","=within","within=","=within=") ) {
            if (i<nr) data_string<-paste0(data_string,";",score[i])
          } else {
            data_string<-paste0(data_string,";",score[i])
          }
        }
        data_string<-paste0(data_string,";",numtot)
      } else {
        data_string<-paste0(score,";",numtot)
      }
      cat(file=argv$ffout_verif,append=T,
          paste0(t_to_read,";",data_string))
    } # end if gridded_output yes/no
  } # end if verif
  #----------------------------------------------------------------------------
  # store in a raster stack 
  if (gridded_output & !argv$verif & !argv$time_aggregation_online)  {
    if (!exists("s"))  {
      s<-r
    } else {
      s<-stack(s,r)
    }
  } else if (gridded_output & !argv$verif & argv$time_aggregation_online) {
    if (!exists("s"))  {
      s<-r
    } else {
      if (argv$fun=="sum")  {
        sval<-getValues(s); rval<-getValues(r)
        ix_aux<-which(!is.na(sval) & !is.na(rval) & is.finite(sval) & is.finite(rval))
        if (length(ix_aux)>0) s[ix_aux]<-sval[ix_aux]+rval[ix_aux]
        rm(sval,rval,ix_aux)
      }
    }
  }
  #----------------------------------------------------------------------------
  # plot
  if (argv$pam) {
     ffout_png<-replaceDate(string=argv$ffout_pam_template,
                            date.str=format(tseq[t],
                                      format=argv$ffin_date.format,tz="GMT"),
                            year_string=argv$year_string,
                            month_string=argv$month_string,
                            day_string=argv$day_string,
                            hour_string=argv$hour_string,
                            sec_string=argv$sec_string,
                            format=argv$ffin_date.format)
    pam(ffout=list(name=ffout_png,
                   width=argv$pam_width,
                   height=argv$pam_height),
        borders_par=list(name=argv$pam_ffshp_borders,
                         layer=argv$pam_ffshp_borders_layer,
                         lwd=argv$pam_ffshp_borders_lwd),
        fig_par=list(mar=c(.5,.5,.5,.5),
                     xlim=argv$pam_xlim,
                     ylim=argv$pam_ylim,
                     fool_coltab=argv$pam_fool_coltab,
                     fool_coltab_rev=argv$pam_fool_coltab_rev,
                     fool_path=argv$pam_fool_path,
                     fool_breaks=argv$pam_fool_breaks,
                     breaks=argv$pam_breaks,
                     disagg_fact=argv$pam_disagg_fact,
                     colors=argv$pam_colors),
        leg_par=list(type=argv$pam_leg_type,
                     nticks=argv$pam_leg_cbar_nticks,
                     height=argv$pam_leg_height,
                     dig=argv$pam_leg_dig),
        raster_to_plot=r
       ) 
  }
  #----------------------------------------------------------------------------
  # update counters of valid timesteps
  n<-n+1
  t_ok[n]<-t
  rm(r,values)
  if (exists("r_ref")) rm(r_ref)
  first<-F
} # MAIN LOOP @@END@@
#
if (argv$debug) {
  print("time steps \"ok\":")
  print(t_ok)
  print(paste("number of time steps \"ok\" =",n))
}
#------------------------------------------------------------------------------
# RData output file
if (argv$summ_stat_fun=="ellipsis") {
  save(file=argv$ffout_summ_stat,
       ell_list,tseq,t_ok,n)
}
#------------------------------------------------------------------------------
# Aggregate gridpoint-by-gridpoint over time
if (gridded_output)  {
  #----------------------------------------------------------------------------
  # fill the gaps
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
  #----------------------------------------------------------------------------
  # time aggregation
  if (argv$time_aggregation & !argv$time_aggregation_online) {
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
        if (argv$fun=="sum")  { r<-sum(s,na.rm=T) } 
        else if (argv$fun=="mean") { r<-mean(s,na.rm=T) }
        else if (argv$fun=="max")  { r<-max(s,na.rm=T) }
        else if (argv$fun=="min")  { r<-min(s,na.rm=T) }
        else if (argv$fun=="count" | argv$fun=="freq")  {
          if (length(argv$r)>1) { 
            threshold<-argv$r[1]
            threshold1<-argv$r[2] 
          } else { 
            threshold<-argv$r 
            threshold1<-NA
          }
          type<-argv$b
          ix<-which(!is.na(getValues(subset(s,subset=1))))
          npoints<-length(ix)
          mat<-array(data=NA,dim=c(npoints,nlayers(s))) 
          for (l in 1:nlayers(s)) mat[,l]<-getValues(subset(s,subset=l))[ix]
          if (!is.na(argv$cores)) {
            dat<-mcmapply(score_fun,
                          1:npoints,
                          mc.cores=argv$cores,
                          SIMPLIFY=T, 
                          lab="count_x",
                          threshold=threshold,
                          threshold1=threshold1,
                          type=type)
          # no-multicores
          } else {
            dat<-mapply(score_fun,
                        1:npoints,
                        SIMPLIFY=T,
                        lab="count_x",
                        threshold=threshold,
                        threshold1=threshold1,
                        type=type)
          }
          r<-subset(s,subset=1)
          r[]<-NA
          if (argv$fun=="count") {r[ix]<-dat} else {r[ix]<-dat/nlayers(s)}
          rm(mat,dat,ix)
        }
        else if (argv$fun=="radar_mean")  {
          first<-T
          for (t in 1:n) {
            weight<-ifelse(format(tseq[t_ok[t]],format="%M%S",tz="GMT")=="0000",
                           0.5,1)
            dat<-getValues(subset(s,subset=t))
            ix_nona<-which(!is.na(dat))
            if (first) {
              ndat<-length(dat)
              dat_mean<-rep(0,length=ndat)
              dat_cont<-rep(0,length=ndat)
              weight_sum<-rep(0,length=ndat)
              first<-F
            }
            dat_cont[ix_nona]<-dat_cont[ix_nona]+1
            dat_mean[ix_nona]<-dat_mean[ix_nona]+weight*dat[ix_nona]
            weight_sum[ix_nona]<-weight_sum[ix_nona]+weight
            rm(ix_nona,dat,weight)
          }
          r<-subset(s,subset=1)
          r[]<-NA
          ix<-which(dat_cont>0 & (dat_cont/n_tseq)>=argv$frac)
          if (length(ix)>0) r[ix]<-dat_mean[ix] / weight_sum[ix]
          rm(ndat,dat_mean,dat_cont,first,ix,weight_sum)
        }
      # use weights
      } else {
        print("Using weights")
        first<-T
        for (t in t_ok) {
          dat<-weights[t]*getValues(subset(s,subset=t))
          if (first) {
            ndat<-length(dat)
            dat_res<-rep(0,length=ndat)
            dat_cont<-rep(0,length=ndat)
            weight_sum<-rep(0,length=ndat)
            first<-F
          }
          ix_nona<-which(!is.na(dat))
          dat_cont[ix_nona]<-dat_cont[ix_nona]+1
          weight_sum[ix_nona]<-weight_sum[ix_nona]+weights[t]
          aux<-weights[t]*dat[ix_nona]
          if (argv$fun=="sum")  {
            dat_res[ix_nona]<-dat_res[ix_nona]+aux
          } else if (argv$fun=="max") {
            dat_res[ix_nona]<-pmax(dat_res[ix_nona],aux,na.rm=T)
          } else if (argv$fun=="min") {
            dat_res[ix_nona]<-pmin(dat_res[ix_nona],aux,na.rm=T)
          } else if (argv$fun=="mean") {
            dat_res[ix_nona]<-dat_res[ix_nona]+aux
          }
          rm(ix_nona,aux)
        }
        r<-subset(s,subset=t_ok[1])
        r[]<-NA
        ix<-which(dat_cont>0 & (dat_cont/n_tseq)>=argv$frac)
        if (length(ix)>0) {
          if (argv$fun=="mean") {
            r[ix]<- dat_res[ix] / weight_sum[ix]
          } else {
            r[ix]<-dat_res[ix]
          }
        }
        rm(ndat,dat_res,dat_cont,first,ix,weight_sum)
      } # endif use weights
    } else {
      boom(paste("number of time steps available=",n," is less than required"))
    }
  }
  #----------------------------------------------------------------------------
  # verification
  if (argv$verif) {
    if (argv$verif_metric %in% c("corr","msess","ets","a","b","c","d","seeps","roblinreg")) {
      if (argv$verif_metric=="corr") {
        threshold<-NA
        threshold1<-NA
        type<-argv$verif_corr_method
      } else if (argv$verif_metric=="msess") {
        threshold<-NA
        threshold1<-NA
        type<-NA
      } else if (argv$verif_metric=="seeps") {
        threshold<-argv$verif_seeps_threshold
        threshold1<-argv$verif_seeps_threshold
        type<-argv$verif_seeps_type
      } else if (argv$verif_metric=="roblinreg") {
        threshold<-argv$verif_roblinreg_threshold
        threshold1<-argv$verif_roblinreg_threshold
        type<-argv$verif_roblinreg_type
      } else if (argv$verif_metric %in% c("a","b","c","d","ets")) {
        threshold<-argv$verif_contab_threshold
        threshold1<-argv$verif_contab_threshold1
        type<-argv$verif_contab_type
      }
      if (!is.na(argv$cores)) {
        dat_mean<-mcmapply(score_fun,
                           1:npoints,
                           mc.cores=argv$cores,
                           SIMPLIFY=T, 
                           lab=argv$verif_metric,
                           threshold=threshold,
                           threshold1=threshold1,
                           type=type)
      # no-multicores
      } else {
        dat_mean<-mapply(score_fun,
                         1:npoints,
                         SIMPLIFY=T,
                         lab=argv$verif_metric,
                         threshold=threshold,
                         threshold1=threshold1,
                         type=type)
      }
      rm(mat,mat_ref)
      if (exists("mat_ref_mean")) rm(mat_ref_mean)
      dat_cont<-dat_mean
      dat_cont[]<-n
    } else if (argv$verif_metric %in% c("rmse","rmsf")) {
      dat_mean<-sqrt(dat_mean)
    }
    # define r again
    r<-rmaster
    rm(rmaster)
    ix<-which(!is.na(dat_cont) & (dat_cont/n_tseq)>=argv$frac)
    if (length(ix)>0) r[ix_ver[ix]]<-dat_mean[ix]
    if (exists("dat_mean")) rm(dat_mean)
    if (exists("dat_cont")) rm(dat_cont)
    if (exists("ix_ver")) rm(ix_ver)
    if (exists("ix")) rm(ix)
  }
  #-----------------------------------------------------------------------------
  # Gridded output
  # adjust 
  if (!exists("r")) r<-s
  if (exists("s")) rm(s)
  # write
  xy<-xyFromCell(r,1:ncell(r))
  x<-sort(unique(xy[,1]))
  y<-sort(unique(xy[,2]),decreasing=T)
  r.list<-list()
  if (argv$time_cat) {
    grid<-array(data=NA,dim=c(length(x),length(y),nlayers(r)))
    for (i in 1:nlayers(r)) 
      grid[,,i]<-matrix(data=subset(r,subset=i), 
                        ncol=length(y), nrow=length(x))
    date_out<-format(tseq[t_ok],"%Y%m%d%H%M",tz="UTC")
  } else {
    grid<-array(data=NA,dim=c(length(x),length(y)))
    grid[,]<-matrix(data=r, ncol=length(y), nrow=length(x))
    date_out<-format(strptime(argv$date_out,
                              argv$date_out.format,tz="UTC"),
                              "%Y%m%d%H%M",tz="UTC")
  }
  r.list[[1]]<-grid
  rm(grid,r)
  out<-write_dotnc(grid.list=r.list,
                   times=date_out,
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
} # endif gridded_output
#------------------------------------------------------------------------------
# Normal exit
q(status=0) 
