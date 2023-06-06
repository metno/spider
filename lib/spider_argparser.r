#+
argparser<-function() {
#------------------------------------------------------------------------------
p <- arg_parser("spider")
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
p <- add_argument(p, "--time_bnds_string",
                  help="time bounds with respect to date_out (e.g., \"-1 day\" \"-1 min\"). The end of the aggregation period is assumed to be date_out.",
                  type="character",
                  default="none")
p <- add_argument(p, "--time_bnds_string_as_two_dates",
                  help="time bounds as two dates (format %Y%m%d%H%M)",
                  type="character",
                  default=NA,
                  nargs=Inf)
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
p <- add_argument(p, "--date_out_time_step",
                  help="time step",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--date_out_time_unit",
                  help="time unit",
                  type="character",
                  default=NA)
p <- add_argument(p, "--date_out_time_n_prev",
                  help="number of previous time steps",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--date_out_time_n_succ",
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
                  type="character",
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
p <- add_argument(p, "--latte_express",
                  help="interpoLATion verTical profilE. Fast algorithm with shortcuts. Gain speed, loose optimal conditions. Interpolation over master grid based on a non-linear vertical profile",
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
                  help="lapse rate value. Units degC/m",
                  type="numeric",
                  default=-0.0065)
p <- add_argument(p, "--latte_agg_fact",
                  help="aggregation factor (original to aggregated grid). Used in latte_express. Units number of grid points",
                  type="numeric",
                  default=50)
p <- add_argument(p, "--latte_weight_dh_scale",
                  help="horizontal length scale used in the blending of sub-regional verical profiles to weight them. Used in latte_express. same units as master CRS.",
                  type="numeric",
                  default=25000)
#..............................................................................
p <- add_argument(p, "--estvertprof",
                  help="interpoLATion verTical profilE. Interpolation over master grid based on a non-linear vertical profile",
                  flag=T)
p <- add_argument(p, "--evp_halfbox",
                  help="half-width of the square box used to select the nearest observations. same units as master CRS.",
                  type="numeric",
                  default=1)
p <- add_argument(p, "--evp_pmax",
                  help="maximum number of observations to use in the neighbourhood of each observation",
                  type="integer",
                  default=50)
p <- add_argument(p, "--evp_fglab",
                  help="method used to create the first-guess (\"linear\",\"Frei\")",
                  type="character",
                  default="Frei")
p <- add_argument(p, "--evp_gamma",
                  help="lapse rate value. Units degC/m",
                  type="numeric",
                  default=-0.0065)
p <- add_argument(p, "--evp_agg_fact",
                  help="aggregation factor (original to aggregated grid). Units number of grid points",
                  type="numeric",
                  default=50)
p <- add_argument(p, "--evp_weight_dh_scale",
                  help="horizontal length scale used in the blending of sub-regional verical profiles to weight them. same units as master CRS.",
                  type="numeric",
                  default=25000)
p <- add_argument(p, "--evp_gamma_min",
                  help="lapse rate minimum allowed value. Units degC/m",
                  default=-0.0065)
p <- add_argument(p, "--evp_gamma_max",
                  help="lapse rate maximum allowed value. Units degC/m",
                  default=-0.0065)
p <- add_argument(p, "--evp_t0_min",
                  help="t0 minimum allowed value. Units degC.",
                  default=-60)
p <- add_argument(p, "--evp_t0_max",
                  help="t0 maximum allowed value. Units degC.",
                  default=40)
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
p <- add_argument(p, "--gridded_dqc_afterAgg",
                  help="data quality control over gridded data (do it after the aggregation)",
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
p<- add_argument(p, "--space_fun",
                 help="space aggregation function",
                 type="character",
                 default=NA)
p<- add_argument(p, "--time_fun",
                 help="time aggregation function",
                 type="character",
                 default=NA)
p<- add_argument(p, "--precise_mean_rescale",
                 help="if \"fun\" is \"time_aggregation\" and time_fun is \"precise_mean\", then rescale",
                 type="numeric",
                 default=1)
p<- add_argument(p, "--fun_weights",
                 help="weights used in the aggregation. If we call the weights specified by the user with w1, w2, ..., wN, then the result= w1_norm * field_1 + w2_norm * field_2 + ... + wN_norm * field_N. In the program the weights are normalized such that their sum is equal to 1, in other words w1_norm= w1/sum(w1,w2,...,wN), w2_norm= w2/sum(w1,w2,...,wN) and so on. NOTE: for the aggregation function \"radar_mean\" the weights are set by default to \"0.5,1,...,0.5\" and a rescaling factor \"radar_mean_rescale\"is used.",
                 type="character",
                 default="1,1,...,1")
p<- add_argument(p, "--radar_mean_rescale",
                 help="if \"fun\" is \"radar_mean\", then rescale",
                 type="numeric",
                 default=1)
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
p<- add_argument(p, "--fun_narm",
                 help="should be NAs removed when applying fun?",
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
p <- add_argument(p, "--values_mask",
                  help="mask out with values and conditions (greater than x, smaller than y, and so on)",
                  flag=T)
p<- add_argument(p, "--values_mask_vals1",
                 help="threshold used for masking out points",
                 type="character",
                 nargs=Inf,
                 default=NA)
p<- add_argument(p, "--values_mask_vals2",
                 help="threshold used for masking out points (use when condition is \"within\")",
                 type="character",
                 nargs=Inf,
                 default=NA)
p<- add_argument(p, "--values_mask_cond",
                 help="type One of 'below' (< x), 'below=' (<= x), '=within' (<= x <), 'within' (< x <), 'within=' (< x <=), '=within=' (<= x <=), 'above' (> x), or 'above=' (>= x). \"values_mask_vals1\" is used for 'above' and 'below', while \"values_mask_vals2\" is used as an extra threshold for 'within'",
                 type="character",
                 nargs=Inf,
                 default=NA)
p<- add_argument(p, "--values_mask_pad",
                 help="padding values (replacement values for masked out points)",
                 type="character",
                 nargs=Inf,
                 default=NA)
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
p<- add_argument(p, "--ffin_point_mask",
                 help="file with list of points (header label, x, y)",
                 type="character",
                 default=NA)
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
p <- add_argument(p, "--which_quantile",
                  help="if verif_metric is quantile, then this specifies which quantile (0-1)",
                  type="numeric",
                  default=0.5)
p <- add_argument(p, "--quantile_geq_threshold",
                  help="if verif_metric is quantile, then this specifies that only values greater or equal to this threshold must be used",
                  type="numeric",
                  default=NA)
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
p<- add_argument(p, "--get_master_from_input_grid",
                 help="master grid is derived from input grid",
                 flag=T)
p<- add_argument(p, "--master_from_input_aggfact",
                 help="aggregation factor to get the master grid",
                 type="numeric",
                 default=5)
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
p <- add_argument(p, "--ffmaster_get_timestamp_from_file",
                  help="read the timestamp from the master file",
                  flag=T)
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
                 help="output varname(s)",
                 type="character",
                 default="none")
p<- add_argument(p, "--ffout_varlongname",
                 help="attribute of each variable with the long-varname(s)",
                 type="character",
                 default="none")
p<- add_argument(p, "--ffout_varstandardname",
                 help="attribute of each variable with the standard varname(s)",
                 type="character",
                 default="none")
p<- add_argument(p, "--ffout_varversion",
                 help="attribute of each variable with the var version(s)",
                 type="character",
                 default="")
p<- add_argument(p, "--ffout_varunit",
                 help="attribute of each variable with the var unit(s)",
                 type="character",
                 default="")
p <- add_argument(p, "--ffout_cell_methods",
                  help="attribute of each variable with the aggregation methods (e.g. ''time: sum'')",
                  type="character",
                  default=NA,
                  nargs=Inf)
p<- add_argument(p, "--ffout_times_ref",
                 help="output time ref",
                 type="character",
                 default="190001010000")
p<- add_argument(p, "--ffout_times_unit",
                 help="output var version",
                 type="character",
                 default="H")
p<- add_argument(p, "--ffout_reference",
                 help="global attribute with dataset reference",
                 type="character",
                 default="")
p<- add_argument(p, "--ffout_proj4",
                 help="output ''proj4'' strings",
                 type="character",
                 default="")
p <- add_argument(p, "--ffout_lonlat",
                  help="logical, lon lat in the output",
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
p<- add_argument(p, "--ffout_license",
                 help="output",
                 type="character",
                 default="https://www.met.no/en/free-meteorological-data/Licensing-and-crediting")
p<- add_argument(p, "--ffout_cf_1.7",
                 help="output",
                 type="logical",
                 default=T)
p<- add_argument(p, "--ffout_cf_1.0",
                 help="output",
                 type="logical",
                 default=F)
p<- add_argument(p, "--ffout_lonlat_minmax",
                 help="output",
                 type="logical",
                 default=F)
p<- add_argument(p, "--ffout_revy",
                 help="output reverse y-axis",
                 type="logical",
                 default=F)
p<- add_argument(p, "--ffout_x_rounddig",
                 help="output x-axis number of digits",
                 type="integer",
                 default=6)
p<- add_argument(p, "--ffout_y_rounddig",
                 help="output y-axis number of digits",
                 type="integer",
                 default=6)

p<- add_argument(p, "--year_string",
                 help="string, placeholder for year",
                 type="character",
                 default="%Y")
p<- add_argument(p, "--month_string",
                 help="string, placeholder for month",
                 type="character",
                 default="%m")
p<- add_argument(p, "--day_string",
                 help="string, placeholder for day",
                 type="character",
                 default="%d")
p<- add_argument(p, "--hour_string",
                 help="string, placeholder for hour",
                 type="character",
                 default="%H")
p<- add_argument(p, "--min_string",
                 help="string, placeholder for minute",
                 type="character",
                 default="%M")
p<- add_argument(p, "--sec_string",
                 help="string, placeholder for second",
                 type="character",
                 default="%S")
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
p <- add_argument(p, "--gridclimind",
                  help="gridded climate indices",
                  flag=T)
p<- add_argument(p, "--gridclimind_index",
                 help="climate indices, one of: \"degree_days_sum\", \"degree_days\", \"prcptot\",\"freq\",\"maxcons\",\"HD17\",\"sdii\",\"quantile\",\"metnoheatwave\",\"metnoheatwave2023\",\"rx5day\",\"rx3day\",\"gsl\"",
                 type="character",
                 default=NA)
p<- add_argument(p, "--prcptot_r",
                 help="threshold for total precipitation",
                 type="numeric",
                 default=NA)
p<- add_argument(p, "--prcptot_b",
                 help="type one of 'below' (< x), 'below=' (<= x), 'above' (> x), or 'above=' (>= x).",
                 type="character",
                 default="above=")
p<- add_argument(p, "--degday_r",
                 help="threshold for degree days sum",
                 type="numeric",
                 default=NA)
p<- add_argument(p, "--degday_b",
                 help="type one of 'below' (< x), 'below=' (<= x), 'above' (> x), or 'above=' (>= x).",
                 type="character",
                 default="above")
p<- add_argument(p, "--freq_r",
                 help="threshold(s) for frequency of occurrence",
                 type="numeric",
                 nargs=Inf,
                 default=NA)
p<- add_argument(p, "--freq_b",
                 help="type one of 'below' (< x), 'below=' (<= x), 'above' (> x), 'above=' (>= x), 'within' (<x<), '=within' (<=x<), '=within=' (<=x<=), 'within=' (<x<=).",
                 type="character",
                 default="above=")
p<- add_argument(p, "--maxcons_r",
                 help="threshold(s) for maximum number of consecutive occurrencies",
                 type="numeric",
                 nargs=Inf,
                 default=NA)
p<- add_argument(p, "--maxcons_b",
                 help="type one of 'below' (< x), 'below=' (<= x), 'above' (> x), 'above=' (>= x), 'within' (<x<), '=within' (<=x<), '=within=' (<=x<=), 'within=' (<x<=).",
                 type="character",
                 default="above=")
p<- add_argument(p, "--freq_as_perc",
                 help="return freq as percentage over the total number of samples",
                 flag=T)
p<- add_argument(p, "--metnohw_tmin_threshold",
                 help="MET Norway heat wave index, definition of the threshold for tmin",
                 type="numeric",
                 default=16)
p<- add_argument(p, "--metnohw_tmax_threshold",
                 help="MET Norway heat wave index, definition of the threshold for tmax",
                 type="numeric",
                 default=28)
p<- add_argument(p, "--gsl_tg_threshold",
                 help="Growing season lenght, daily mean temperature threshold (degC)",
                 type="numeric",
                 default=5)
p<- add_argument(p, "--gsl_ndays_threshold",
                 help="Growing season lenght, number of days for the warm/cold spell (days)",
                 type="integer",
                 default=6)

#..............................................................................
p <- add_argument(p, "--temporal_trend",
                  help="trend through time. NOTE: the output nc-file has preset varname, varlongname, varstandardname, varversion, diground.",
                  flag=T)
p<- add_argument(p, "--temporal_trend_elab",
                 help="trend through time elaboration, one of: \"Theil_Sen_regression\", \"Mann_Kendall_trend_test\",\"Mann_Kendall_trend_test_ALT\"",
                 type="character",
                 default=NA)
p<- add_argument(p, "--temporal_trend_FDR",
                 help="false discovery rate for the Benjamini‐Hochberg meta-test of the p-values used to assess statistical significance",
                 type="numeric",
                 default=0.05)

#..............................................................................
p <- add_argument(p, "--return_level",
                  help="return level. Compute return levels over an output grid given a sequence of precipitation fields over an input grid. Output and input grid may differ.",
                  flag=T)
p<- add_argument(p, "--return_level_elab",
                 help="elaboration, one of: \"fitGEV_bayesian\"",
                 type="character",
                 default=NA)
p<- add_argument(p, "--return_level_m1",
                 help="begin elaboration from this output point (if NA then use all points)",
                 type="integer",
                 default=NA)
p<- add_argument(p, "--return_level_m2",
                 help="end elaboration at this output point (if NA then use all points)",
                 type="integer",
                 default=NA)
p<- add_argument(p, "--return_level_aggfact",
                 help="aggregation factor (in number of input grid point) to pass from input grid to output grid (if NA inptu and output are the same grid)",
                 type="integer",
                 default=NA)
p<- add_argument(p, "--return_level_loop_deltam",
                 help="optimization of computing resources used in the loop over input point (the larger, the faster but more memory is used)",
                 type="integer",
                 default=10000)
p<- add_argument(p, "--return_level_nn2k",
                 help="number of nearest point to consider for the regionalization (or spatial resampling)",
                 type="integer",
                 default=310)
p<- add_argument(p, "--return_level_nn2radius",
                 help="radius within nearest point are considered for the regionalization (or spatial resampling). Should be linked to nn2k",
                 type="integer",
                 default=10000)
p<- add_argument(p, "--return_level_iter_reg",
                 help="number of iterations used for regionalization (or spatial resampling)",
                 type="integer",
                 default=10)
p<- add_argument(p, "--return_level_year4retlev",
                 help="number of years used to compute the return levels (can be more than one)",
                 type="integer",
                 nargs=Inf,
                 default=NA)
p<- add_argument(p, "--return_level_randomseed",
                 help="set to a value different from NA to have reproducible results (the value does not matters)",
                 type="integer",
                 default=1)
p<- add_argument(p, "--return_level_iter_bay",
                 help="number of iterations used in the MCMC part of the Bayesian estimation",
                 type="integer",
                 default=50000)
p<- add_argument(p, "--return_level_burn",
                 help="burn in parameter, number of iterations not considered when using MCMC part of the Bayesian estimation",
                 type="integer",
                 default=47000)
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
#
# convert character to numbers OR replace _ with - in strings
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

if (any(!is.na(argv$time_bnds_string))) {
  argv$time_bnds_string<-gsub("_","-",argv$time_bnds_string)
}

if (any(!is.na(argv$crop))) {
  for (i in 1:length(argv$crop)) 
    argv$crop[i] <- gsub("_","-",argv$crop[i])
  argv$crop <- as.numeric(argv$crop)
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

if ( any( !is.na( argv$gridded_dqc.clump_r))) {
  aux <- vector( mode="numeric", length=length(argv$gridded_dqc.clump_r))
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
argv$gridded_dqc.outlier_pad <- as.numeric( argv$gridded_dqc.outlier_pad)
argv$gridded_dqc.min_pad <- as.numeric( argv$gridded_dqc.min_pad)
argv$gridded_dqc.max_pad <- as.numeric( argv$gridded_dqc.max_pad)
argv$gridded_dqc.clump_pad <- as.numeric( argv$gridded_dqc.clump_pad)
argv$gridded_dqc.clump_r <- as.numeric( argv$gridded_dqc.clump_r)
argv$gridded_dqc.clump_n <- as.numeric( argv$gridded_dqc.clump_n)
#
if (is.na(argv$space_fun)) argv$space_fun<-argv$fun
if (is.na(argv$time_fun)) argv$time_fun<-argv$fun

if (any(!is.na(argv$values_mask_vals1))) { 
  aux <- vector(mode="numeric",length=length(argv$values_mask_vals1))
  for (i in 1:length(argv$values_mask_vals1)) 
    aux[i] <- as.numeric(gsub("_","-",argv$values_mask_vals1[i]))
  argv$values_mask_vals1 <- aux
}

if (any(!is.na(argv$values_mask_vals2))) {
  aux <- vector(mode="numeric",length=length(argv$values_mask_vals2))
  for (i in 1:length(argv$values_mask_vals2)) 
    aux[i] <- as.numeric(argv$values_mask_vals2[i])
  argv$values_mask_vals2 <- aux
}

if (any(!is.na(argv$values_mask_pad))) {
  aux <- vector(mode="numeric",length=length(argv$values_mask_pad))
  for (i in 1:length(argv$values_mask_pad)) { 
    if (argv$values_mask_pad[i]=="NA") {
      aux[i] <- NA
    } else {
      aux[i] <- as.numeric(argv$values_mask_pad[i])
    }
  }
  argv$values_mask_pad <- aux
}

return(argv)
}
