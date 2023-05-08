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
rm( list = ls())
#
# -----------------------------------------------------------------------------
# Libraries
suppressPackageStartupMessages( library( "argparser"))
suppressPackageStartupMessages( library( "sp"))
suppressPackageStartupMessages( library( "raster"))
suppressPackageStartupMessages( library( "rgdal"))
suppressPackageStartupMessages( library( "ncdf4"))
suppressPackageStartupMessages( library( "dotnc"))
#options(warn = 2, scipen = 999)
options( scipen = 999999999)
#
# -----------------------------------------------------------------------------
# Constants
# CRS strings
proj4.wgs84     <- "+proj=longlat +datum=WGS84"
proj4.ETRS_LAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
proj4.utm33     <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4.lcc       <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
ffout_default   <- "out.nc"

# -----------------------------------------------------------------------------
# FUNCTIONS

# + manage fatal error
boom <- function( str=NA, code=NA) {
  cat("Fatal Error ")
  if ( !is.na(code)) {
    if ( code == 1) cat("file not found ")
  }
  if ( !is.na(str)) cat( str)
  cat("\n")
  quit( status= 1)
}

#+ the end 
rip <- function( str=NA, code=NA, t0=NA) {
  cat( "the End : ")
  if ( !is.na(code) ) {
    if ( code == 1 ) cat( "normal exit : ")
  }
  if ( !is.na(t0)) {
    t1 <- Sys.time()
    cat( paste( "total time=", round(t1-t0,1), attr(t1-t0,"unit")," "))
  }
  if ( !is.na(str)) cat( str)
  cat("\n")
  quit( status= 0 )
}


#==============================================================================
# MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN -
#==============================================================================
t0<-Sys.time()
# path to the titan functions is stored in the enviroment var TITANR_FUN
spider_path <- Sys.getenv( "SPIDER_PATH")
#------------------------------------------------------------------------------
# Load functions
for (file in list.files(path = file.path(spider_path,"lib"),
                        pattern = ".r", full.names=T) ) 
  source(file)
rm(file)
#-----------------------------------------------------------------------------
# Read command line arguments
argv <- argparser()
#-----------------------------------------------------------------------------
# Multi-cores run
if ( !is.na( argv$cores)) {
  suppressPackageStartupMessages( library( "parallel"))
  if ( argv$cores==0) argv$cores <- detectCores()
  cat( paste( "--> multi-core run, cores=", argv$cores, "\n"))
}
#------------------------------------------------------------------------------
# Time sequence
res <- spider_timeseq( argv)
n_tseq <- res$n_tseq
tseq   <- res$tseq
date_out        <- res$date_out
date_out.format <- res$date_out.format
n_tseq_out      <- res$n_tseq_out
date_out_ix     <- res$date_out_ix
tseq_ref <- res$tseq_ref
tseq_out <- res$tseq_out
rm( res)
#------------------------------------------------------------------------------
# Initialization
# default is time_aggregation
if ( !argv$time_aggregation        & 
     !argv$time_aggregation_online & 
     !argv$time_cat                & 
     !argv$upscale                 & 
     !argv$downscale               &  
     !argv$latte                   &
     !argv$latte_express           &
     !argv$estvertprof             &
     !argv$summ_stat               & 
     !argv$gridclimind             & 
     !argv$pam                     & 
     !argv$verif ) 
  argv$time_aggregation<-T
#
# decide if gridded output is required
gridded_output <- F
if ( argv$time_aggregation        | 
     argv$time_aggregation_online | 
     argv$time_cat                | 
     argv$upscale                 | 
     argv$downscale               | 
     argv$latte_express           |
     argv$estvertprof             |
     argv$gridclimind             | 
     argv$return_level            | 
     argv$latte                   | 
    (argv$verif & argv$ffout!=ffout_default) ) 
  gridded_output<-T
#
# load special libraries
if (argv$summ_stat & argv$summ_stat_fun=="wave_nrgx") 
  suppressPackageStartupMessages( library( "waveslim"))
if (argv$summ_stat & argv$summ_stat_fun=="ellipsis") { 
  suppressPackageStartupMessages( library( "cluster"))
  suppressPackageStartupMessages( library(  "igraph")) 
}
if (argv$return_level) { 
  suppressPackageStartupMessages( library( "extRemes"))
  suppressPackageStartupMessages( library(  "tidyr")) 
  suppressPackageStartupMessages( library(  "RANN")) 
}
#
# adjust negative numbers
argv$summ_stat_condition_threshold<-as.numeric(gsub("_","-",
                                      argv$summ_stat_condition_threshold))
if ( is.na( argv$ffin_point_mask)) {
  argv$point_mask_x <- as.numeric( gsub( "_", "-", argv$point_mask_x))
  argv$point_mask_y <- as.numeric( gsub( "_", "-", argv$point_mask_y))
} else {
  tab <- read.table( file= argv$ffin_point_mask, header=T, sep=",",
                     stringsAsFactors=F, strip.white=T)
  argv$point_mask_x <- tab$x
  argv$point_mask_y <- tab$y
  argv$point_mask_labels <- tab$label
  rm( tab)
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
#  if (argv$verbose & t%%100==0) 
    cat( paste( "time timestep", tseq[t], t, "/", n_tseq,
                  "elapsed time", round(Sys.time()-t0,2), 
                  attr(Sys.time()-t0,"unit"),"\n"))
  if ( any(is.na(tseq_ref))) { time_ref_t <- NA } else { time_ref_t <- tseq_ref[t]}
  res <- spider_readEmAll( 
    time     = tseq[t], 
    time_ref = time_ref_t)
  if ( is.null(res)) next
  ffin           <- res$ffin
  t_to_read_ffin <- res$t_to_read_ffin
  # 
  r       <- res$r
  # reference dataset
  r_ref   <- res$r_ref
  # master
  if ( !exists( "rmaster")) {
    if ( class( res$rmaster) == "logical") { 
      rmaster <- r
    } else { 
      rmaster <- res$rmaster
    }
    argv$ffmaster_proj4 <- res$ffmaster_proj4
  }
  if ( !exists( "rmaster_dem")) rmaster_dem <- res$rmaster_dem
  if ( !exists( "r_dem"))             r_dem <- res$r_dem
  rm( res)
  #----------------------------------------------------------------------------
  # crop
  if ( !any( is.na( argv$crop))) {
    res <- spider_crop()
    if ( is.null( res)) next
    r     <- res$r
    r_dem <- res$r_dem
    r_ref <- res$r_ref
    rmaster <- res$rmaster
    rmaster_dem <- res$rmaster_dem
    rm( res)
  }
  #----------------------------------------------------------------------------
  # Upscale to coarser grid
  if ( argv$upscale) {
    r <- spider_upscale()
    if ( is.null(r)) next
  }
  #----------------------------------------------------------------------------
  # Downscale to finer grid
  if ( argv$downscale) {
    r <- spider_downscale()
    if ( is.null(r)) next
  }
  #----------------------------------------------------------------------------
  # Interpolation over master grid based on a non-linear vertical profile
  if ( argv$latte | 
       argv$latte_express) {# LATTE-interpoLATion verTical profilE
    if ( argv$latte_express) {
      res <- latte_express( box_o_nearest_halfwidth = argv$latte_halfbox, 
                            pmax            = argv$latte_pmax,
                            gamma           = argv$latte_gamma,
                            agg_fact        = argv$latte_agg_fact, 
                            weight_dh_scale = argv$latte_weight_dh_scale)
    } else {
      res <- latte()
    }
    if ( any( is.na( res))) 
      print(paste0("@@ warning: problems in regridding over ",
                   length( which( is.na( res))),
                   " points"))
    # save results in r
    r  <- rmaster; r[]<-NA
    ix <- which( !is.na( getValues(rmaster)) & 
                 !is.na( getValues(rmaster_dem)))
#    res1<-res; res<-ix_ma; res[]<-NA; res[1:length(res1)]<-res1; rm(res1)
    r[ix]<-res
    if ( !any( !is.na( values <- getValues(r)))) {
      print("warning: all NAs after latte")
      next
    }
  }
  #----------------------------------------------------------------------------
  # Estimation of vertical profile
  if ( argv$estvertprof) {
    rmaster <- r
    rmaster_dem <- r_dem
    argv$ffmaster_proj4 <- argv$ffin_proj4
    res_evp <- spider_estvertprof( box_o_nearest_halfwidth = argv$evp_halfbox, 
                                   pmax            = argv$evp_pmax,
                                   gamma           = argv$evp_gamma,
                                   gamma_min       = argv$evp_gamma_min,
                                   gamma_max       = argv$evp_gamma_max,
                                   t0_min          = argv$evp_t0_min,
                                   t0_max          = argv$evp_t0_max,
                                   agg_fact        = argv$evp_agg_fact, 
                                   weight_dh_scale = argv$evp_weight_dh_scale)
    t0_evp    <- res_evp$res_out[1,]
    gamma_evp <- res_evp$res_out[2,]
    ix_evp    <- res_evp$ix
    rm( res_evp)
  } 
  #----------------------------------------------------------------------------
  # Use the mask(s) if needed
  # special case of downscaling
  if ( argv$master_mask & 
       !argv$latte & !argv$latte_express & !argv$downscale & !argv$upscale) {
    r <- spider_downscale() # mask is rmaster, so this is a downscaling for us
    if ( is.null(r)) next
    if ( !any( !is.na( values <- getValues(r)))) {
      print(paste("warning: all NAs after mask"))
      next
    }
  }
  #
  if (argv$polygon_mask) {
    r <- spider_polygon_mask()
    if ( is.null(r)) next
  }
  #----------------------------------------------------------------------------
  # Extract a list of points
  if (argv$point_mask) {
    values <- spider_point_mask()
    if ( is.null(r)) next
  }
  #----------------------------------------------------------------------------
  # convert from equivalent_reflectivity_factor to rain rate (mm/h) 
  if (argv$reflectivity_to_precip) { 
    r <- spider_reflectivity_to_precip()
    if ( is.null(r)) next
  } 
  #----------------------------------------------------------------------------
  # radar data quality control
  if (argv$metno_radar_dqc) {
    r <- spider_metno_radar_dqc( ffin=ffin, t_to_read=t_to_read_ffin)
    if ( is.null(r)) next
  } 
  #----------------------------------------------------------------------------
  # data quality control 
  if ( argv$gridded_dqc) {
    # range check_ check for unplausible values
    if ( !is.na( argv$gridded_dqc.min) & 
         !is.na( argv$gridded_dqc.max)) 
      r <- spider_griddqc_range_check()
    # check for holes in the field: remove small patches of connected cells
    if ( !any( is.na( argv$gridded_dqc.clump_r)) & 
         !any( is.na( argv$gridded_dqc.clump_n))) 
      r <- spider_griddqc_cool()
    # check for outliers
    if ( !is.na( argv$gridded_dqc.outlier_aggfact)) 
      r <- spider_griddqc_outliers()
  }
  #----------------------------------------------------------------------------
  # summary statistics 
  if (argv$summ_stat) {
    if (!exists("values")) values <- getValues( r)
    # some values not NAs
    if (length( ixvalid <- which(!is.na(values)) ) > 0) {
      ncells <- NA
      fcells <- NA
      if ( !is.na( argv$summ_stat_condition_threshold)) {
        ixtrue <- which( !is.na(values) & 
                         values > argv$summ_stat_condition_threshold )
        ncells <- length( ixtrue) 
        fcells <- length( ixtrue) / length( ixvalid)
        if ( !is.na( argv$summ_stat_condition_ncells) &
             ncells < argv$summ_stat_condition_ncells ) 
          next
        if ( !is.na( argv$summ_stat_condition_fcells) &
             fcells < argv$summ_stat_condition_fcells ) 
          next
      } 
    # values are all NAs
    } else {
      next
    }
    #--------------------------------------------------------------------------
    # - List values in a text file
    if ( argv$summ_stat_fun        == "list_values") {
      res <- spider_summ_stat_list_values( first=first, time=t_to_read_ffin) 
    # - Energy decomposition based on wavelet analysis
    } else if ( argv$summ_stat_fun == "wave_nrgx") {
      res <- spider_summ_stat_wave_nrgx()
    # - Summary statistics, standard set of parameters
    } else if ( argv$summ_stat_fun == "standard") {
      res <- spider_summ_stat_standard( first  = first, 
                                        ncells = ncells, fcells = fcells,
                                        time = t_to_read_ffin)
    # - Frequency distributions
    } else if ( argv$summ_stat_fun == "freqdist") {
      res <- spider_summ_stat_freqdist( first  = first)
    # - Ellipsis, object-based summary statistics 
    } else if ( argv$summ_stat_fun == "ellipsis") {
      if ( !exists( "ell_list")) ell_list <- list()
      aux <- spider_summ_stat_ellipsis()
      if (!is.null(ell_list))
        ell_list <- append( ell_list, aux)
    } else if ( argv$summ_stat_fun == "gamma_parest") {
      if ( !exists( "dat_to_gamma")) dat_to_gamma <- integer(0)
      aux <- getValues(r)[which(!is.na(getValues(r)))]
      dat_to_gamma <- c( dat_to_gamma, aux)
    # - what?
    } else {
      boom("ERROR: summ_stat_fun not defined")
    } 
  } # end if summary statistics
  #----------------------------------------------------------------------------
  # prepare for verification statistics 
  if ( argv$verif) {
    if ( gridded_output) {
      if ( !exists( "rmaster")) { rmaster<-r; rmaster[]<-NA}
      res <- spider_verif_grid_prepare()
      if ( is.null( res)) next
      # scores that are computed online
      #  dat_... dimension is the number of cells of raster "r"
      if ( res$online) {
        dat_mean <- res$dat_mean_up
        dat_cont <- res$dat_cont_up
        if ( !exists( "ix_dat")) ix_dat <- 1:ncell(r)
        print( paste("verif & gridded output: number of cells",res$n)) 
      # scores that require to store the whole dataset in memory
      #  mat... matrix (number of cells, subset not NAs) x (number of times)
      } else {
        if ( !exists("mat")) {
          mat    <- res$mat_col
          ix_dat <- res$ix
          if ( !is.na( argv$ffin_ref_template)) 
            mat_ref <- res$mat_ref_col
        } else {
          if ( any( !(res$ix %in% ix_dat))) {
            print("WARNING: verif & gridded output, wrong indexes: statistics is supposed to use always the same cells")
            next
          }
          mat <- cbind( mat, res$mat_col)
          if ( !is.na( argv$ffin_ref_template)) 
            mat_ref <- cbind(  mat_ref, res$mat_ref_col)
        }
      }
      rm( res)
    } # end if gridded output is TRUE
      # compute verif statistics on a step-by-step basis
      else {
      res <- spider_verif_online( first = first, 
                                  time  = t_to_read_ffin)
    } # end if gridded_output yes/no
  } # end if verif
  #----------------------------------------------------------------------------
  # prepare for gridded climate indices 
  if ( argv$gridclimind) {
    if ( !exists( "rmaster")) { rmaster<-r; rmaster[]<-NA}
    res <- spider_gridclimind_prepare()
    if ( is.null( res)) next
    # scores that are computed online
    #  dat_... dimension is the number of cells of raster "r"
    if ( res$online) {
      dat_aggr <- res$dat_aggr_up
      dat_cont <- res$dat_cont_up
      if ( !exists( "ix_dat")) ix_dat <- 1:ncell(r)
      print( paste("gridclimind & gridded output: number of cells",res$n)) 
    # scores that require to store the whole dataset in memory
    #  mat... matrix (number of cells, subset not NAs) x (number of times)
    } else {
      if ( !exists("mat")) {
        mat    <- res$mat_col
        ix_dat <- res$ix
        if ( !is.na( argv$ffin_ref_template)) 
          mat_ref <- res$mat_ref_col
      } else {
        if ( any( !(res$ix %in% ix_dat))) {
          print("WARNING: verif & gridded output, wrong indexes: statistics is supposed to use always the same cells")
          next
        }
        mat <- cbind( mat, res$mat_col)
        if ( !is.na( argv$ffin_ref_template)) 
          mat_ref <- cbind(  mat_ref, res$mat_ref_col)
      }
    }
    rm( res)
  } # end if gridclimind
  #----------------------------------------------------------------------------
  # prepare for temporal trend 
  if ( argv$temporal_trend) {
    if ( !exists( "rmaster")) { rmaster<-r; rmaster[]<-NA}
    res <- spider_temporal_trend_prepare()
    if ( is.null( res)) next
    # scores that are computed online
    #  dat_... dimension is the number of cells of raster "r"
    if ( res$online) {
      dat_aggr <- res$dat_aggr_up
      dat_cont <- res$dat_cont_up
      if ( !exists( "ix_dat")) ix_dat <- 1:ncell(r)
      print( paste("temporal trend & gridded output: number of cells",res$n)) 
    # scores that require to store the whole dataset in memory
    #  mat... matrix (number of cells, subset not NAs) x (number of times)
    } else {
      if ( !exists("mat")) {
        mat    <- res$mat_col
        ix_dat <- res$ix
        if ( !is.na( argv$ffin_ref_template)) 
          mat_ref <- res$mat_ref_col
      } else {
        if ( any( !(res$ix %in% ix_dat))) {
          print("WARNING: temporal trend & gridded output, wrong indexes: statistics is supposed to use always the same cells")
          next
        }
        mat <- cbind( mat, res$mat_col)
        if ( !is.na( argv$ffin_ref_template)) 
          mat_ref <- cbind(  mat_ref, res$mat_ref_col)
      }
    }
    rm( res)
  } # end if temporal trend
  #----------------------------------------------------------------------------
  # prepare for return level
  if ( argv$return_level) {
    if ( !exists( "rmaster")) { rmaster<-r; rmaster[]<-NA}
    res <- spider_return_level_prepare()
    if ( is.null( res)) next
    if ( !exists("mat")) {
      mat    <- res$mat_col
      ix_dat <- res$ix
    } else {
      if ( any( !(res$ix %in% ix_dat))) {
        print("WARNING: return & gridded output, wrong indexes: statistics is supposed to use always the same cells")
        next
      }
      mat <- cbind( mat, res$mat_col)
    }
    rm( res)
  } # end if return level
  #----------------------------------------------------------------------------
  # store in a raster stack 
  if ( gridded_output & !argv$verif & !argv$gridclimind) {
    if ( !exists( "s")) {
      s <- r 
    # online time aggregation
    } else if ( argv$time_aggregation_online) {
      if ( argv$time_fun == "sum")   {
        s <- s + r
      } else if ( argv$time_fun == "mean")  {
        s <- s + 1/(n+1) * (r-s)
      } else if ( (argv$time_fun == "min") | (argv$time_fun == "max"))  {
        if ( argv$time_fun == "min" ) aux <- pmin( getValues(r), getValues(s))
        if ( argv$time_fun == "max" ) aux <- pmax( getValues(r), getValues(s))
        s[] <- aux
        rm(aux)
      } else {
        rip( str=paste("time_aggregation_online not defined for fun=",argv$time_fun), code=1, t0=t0)
      }
    # save r in a rasterStack
    } else {
      s <- stack( s, r)
    }
  }
  #----------------------------------------------------------------------------
  # plot a map
  if (argv$pam) aux <- spider_pam( time=tseq[t])
  #----------------------------------------------------------------------------
  # update counters of valid timesteps
#  if (exists("values")) rm(values)
  if (exists("r"))      rm(r)
  if (exists("r_ref"))  rm(r_ref)
  n       <- n+1
  t_ok[n] <- t
  first   <- F
} # MAIN LOOP @@END@@
#
if (argv$debug) {
  print("time steps \"ok\":")
  print(t_ok)
  print(paste("number of time steps \"ok\" =",n))
}
#------------------------------------------------------------------------------
# RData output file
if ( argv$summ_stat_fun == "ellipsis") 
  save( file=argv$ffout_summ_stat, ell_list,tseq, t_ok, n)
if ( argv$summ_stat_fun == "gamma_parest") {
  dat_to_gamma[which(dat_to_gamma<0)] <- 0
  res <- gamma_get_shape_rate_from_dataset_constrOptim( dat_to_gamma)
  save( file=argv$ffout_summ_stat, dat_to_gamma, res)
}
#----------------------------------------------------------------------------
# return level
if (argv$return_level) {
  cat(paste("-- Return Levels --\n"))
  if ( argv$return_level_elab %in% c("fitGEV_bayesian")) {
    ntime  <- dim(mat)[2]
    nyears <- length(argv$return_level_year4retlev)
    # define input and output grids
    r_in <- rmaster
    ix_in <- which( !is.na( getValues(r_in)))
    if (!is.na(argv$return_level_aggfact) & argv$return_level_aggfact > 1) {
      r_out <- aggregate( rmaster, fact=argv$return_level_aggfact,
                         fun=mean, na.rm=T, expand=T)
    } else {
      r_out <- r_in
    }
    ix_out <- which( !is.na( getValues(r_out)))
    # get x,y coordinates
    # ..._in vectors are aligned with mat[,]
    xy_in <- xyFromCell( r_in, 1:ncell(r_in))
    x_in  <- xy_in[ix_in,1]
    y_in  <- xy_in[ix_in,2]
    rm(xy_in)
    n_in <- length(x_in)
    xy_out <- xyFromCell( r_out, 1:ncell(r_out))
    x_out  <- xy_out[ix_out,1]
    y_out  <- xy_out[ix_out,2]
    rm(xy_out)
    n_out_tot <- length(x_out)
    cat(paste("total number of time instants =", ntime, "\n"))
    cat(paste("total number of input points  =", n_in, "\n"))
    cat(paste("total number of output points =", n_out_tot, "\n"))
    # define the number of points used to compute return levels
    if ( !is.na(argv$return_level_m1) & !is.na(argv$return_level_m2)) {
      if ( (argv$return_level_m1 > n_out_tot) &
           (argv$return_level_m2 > n_out_tot))
        boom(str=paste("Check input arguments, max points is",n_out_tot))
      m1_def <- argv$return_level_m1 
      m2_def <- min( c(n_out_tot,argv$return_level_m2)) 
      n_out <- m2_def - argv$return_level_m1 + 1
    } else {
      m1_def <- 1
      m2_def <- n_out_tot
      n_out <- length(x_out)
    }
    cat(paste("requested elaboration from point ", m1_def, "to", m2_def,"\n"))
    cat(paste("# output points  =", n_out,"\n"))
    # loop over output points
    m <- m1_def - 1
#    retlev <- array( data=NA, dim=c(n_out,3))
    while (m <= m2_def) {
      m1 <- m + 1
      m2 <- min( c( m + argv$return_level_loop_deltam, m2_def))
      m1_out <- m1 - m1_def + 1
      m2_out <- m2 - m1_def + 1
      m_dim <- m2 - m1 + 1
      cat(paste("loop over output points ",m1,m2,"(",m1_out,m2_out,")","\n"))
      x_m <- x_out[m1:m2]
      y_m <- y_out[m1:m2]
      # find input points nearest to output points
      nn2 <- nn2( cbind( x_in, y_in), 
                  query = cbind( x_m, y_m), 
                  k = argv$return_level_nn2k, 
                  searchtype = "radius", 
                  radius = argv$return_level_nn2radius)
      # multicores
      if ( !is.na( argv$cores)) {
        res <- t( mcmapply( return_level_fun,
                            1:m_dim,
                            mc.cores   = argv$cores,
                            SIMPLIFY   = T, 
                            MoreArgs   = list( 
                                  lab = argv$return_level_elab,
                                  iter_reg = argv$return_level_iter_reg,
                                  year4retlev = argv$return_level_year4retlev,
                                  randomseed = argv$return_level_randomseed,
                                  iter_bay = argv$return_level_iter_bay,
                                  burn = argv$return_level_burn)))
      # no-multicores
      } else {
        res <- t( mapply( return_level_fun,
                          1:m_dim,
                          SIMPLIFY   = T, 
                          MoreArgs   = list( 
                                  lab = argv$return_level_elab,
                                  iter_reg = argv$return_level_iter_reg,
                                  year4retlev = argv$return_level_year4retlev,
                                  randomseed = argv$return_level_randomseed,
                                  iter_bay = argv$return_level_iter_bay,
                                  burn = argv$return_level_burn)))
      }
      # save results in background data structure
      if (m1_out == 1) retlev <- array( data=NA, dim=c(n_out,dim(res)[2]))
      retlev[m1_out:m2_out,] <- res[1:m_dim,]
      # next bunch of gridpoints
      m <- m + argv$return_level_loop_deltam
    } # end loop over gridpoints
    save( file=argv$ffout, argv, retlev, r_in, r_out, mat, m1_def, m2_def)
  }
  gridded_output <- FALSE
}
#------------------------------------------------------------------------------
# Aggregate gridpoint-by-gridpoint over time
if (gridded_output)  {
  #----------------------------------------------------------------------------
  # fill the gaps
  if (argv$fill_gaps) {
    imiss <- which(!(1:n_tseq %in% t_ok))
    nmiss <- length(imiss)
    if ( nmiss > 0) {
      if ( any( diff( imiss)==1) & argv$stop_if_two_gaps) 
        boom( paste( "ERROR two consecutive gaps found"))
      r <- raster( s, layer=1)
      for (i in imiss) {
        r[] <- NA
        iprev <- ifelse(      i==1,          2, (i-1))
        inext <- ifelse( i==n_tseq, (n_tseq-1), (i+1))
        r[]  <- ( getValues( raster( s, layer=iprev)) + 
                  getValues( raster( s, layer=inext))) / 2
        if ( !any( !is.na(getValues( r))) & argv$stop_if_two_gaps )
          boom( paste( "ERROR two consecutive gaps found"))
        s <- stack( s, r)
        n <- n + 1
        t_ok[n] <- i
      }
    }
    if ( exists( "r")) rm( r, i, iprev, inext)
    rm( imiss, nmiss)
  } # end fill the gaps
  #----------------------------------------------------------------------------
  # time aggregation
  if (  argv$time_aggregation & 
       !argv$time_aggregation_online) {
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
        #---------------------------------------------------------------------
        if (argv$time_fun=="sum")  { r<-sum(s,na.rm=argv$fun_narm) } 
        else if (argv$time_fun=="mean") { r<-mean(s,na.rm=argv$fun_narm) }
        else if (argv$time_fun=="max")  { r<-max(s,na.rm=argv$fun_narm) }
        else if (argv$time_fun=="min")  { r<-min(s,na.rm=argv$fun_narm) }
        else if (argv$time_fun=="count" | argv$time_fun=="freq")  {
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
            dat <- mcmapply( score_fun,
                             1:npoints,
                             mc.cores   = argv$cores,
                             SIMPLIFY   = T, 
                             lab        = "count_x",
                             threshold  = threshold,
                             threshold1 = threshold1,
                             type       = type)
          # no-multicores
          } else {
            dat <- mapply( score_fun,
                           1:npoints,
                           SIMPLIFY   = T,
                           lab        = "count_x",
                           threshold  = threshold,
                           threshold1 = threshold1,
                           type       = type)
          }
          r <- subset(s,subset=1)
          r[] <- NA
          if ( argv$time_fun=="count") {r[ix]<-dat} else {r[ix]<-dat/nlayers(s)}
          rm( mat, dat, ix)
        }
        #---------------------------------------------------------------------
        else if (argv$time_fun=="radar_mean")  {
          first<-T
          for (t in 1:n) {
#            works only for hourly aggregation
#            weight<-ifelse(format(tseq[t_ok[t]],format="%M%S",tz="GMT")=="0000",
#                           0.5,1)
            weight<-ifelse((t_ok[t]==1 | t_ok[t]==n_tseq), 0.5, 1)
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
            if (argv$debug) {
              if (!exists("ix_deb")) ix_deb<-ix_nona[which(dat[ix_nona]>0)][1]
              print(paste("t i w dat_cont dat dat_mean w_sum :",
                           t,ix_deb,weight,
                           dat_cont[ix_deb],
                           round(dat[ix_deb],4),
                           round(dat_mean[ix_deb],4),
                           weight_sum[ix_deb]))
            }
            rm(ix_nona,dat,weight)
          }
          r<-subset(s,subset=1)
          r[]<-NA
          ix<-which(dat_cont>0 & (dat_cont/n_tseq)>=argv$frac)
          if (length(ix)>0) r[ix]<-argv$radar_mean_rescale * dat_mean[ix] / weight_sum[ix]
          if (argv$debug) {
            print(paste("t i dat_mean w_sum mean:",
                         t,ix_deb,
                         round(dat_mean[ix_deb],4),
                         weight_sum[ix_deb],
                         round(dat_mean[ix_deb]/weight_sum[ix_deb],4)))
          }
          rm(ndat,dat_mean,dat_cont,first,ix,weight_sum)
        }
        #---------------------------------------------------------------------
        else if (argv$time_fun=="precise_mean")  {
          t00 <- Sys.time()
          ndat <- length( getValues( subset( s, subset=1)))
          dat  <- array(  data=0, dim=c(ndat,n))
          for (t in 1:n) dat[,t] <- getValues( subset( s, subset=t))
          tprec_in <- as.numeric( tseq)
          tprec0 <- as.numeric( seq( tseq_out, by=paste0("-",argv$date_out_time_step," ",argv$date_out_time_unit),length=2)[2])
          tprec_out <- c( tprec0, as.numeric( tseq_out))
          if (!is.na(argv$cores)) {
            res <- mcmapply( precise_fun,
                             1:ndat,
                             mc.cores = argv$cores,
                             SIMPLIFY = T,
                             fun = "mean")
          # no-multicores
          } else {
            res <- mapply( precise_fun,
                           1:ndat,
                           SIMPLIFY = T,
                           fun = "mean")
          }
          r<-subset(s,subset=1)
          if ( n_tseq_out == 1) {
            r[] <- argv$precise_mean_rescale * res
          } else {
            r1 <- subset( s, subset=1)
            r[] <- argv$precise_mean_rescale * res[1,]
            for (t in 1:n_tseq_out) { 
              r1[] <- argv$precise_mean_rescale * res[t,]
              r <- stack( r, r1)
            }
            rm(r1)
          }
          rm( ndat, dat, s, res)
          t11 <- Sys.time()
          print( paste( " precise mean - time", round(t11-t00,1),
                                                attr( t11-t00,"unit")))

        } # end if over time_fun
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
          if (argv$time_fun=="sum")  {
            dat_res[ix_nona]<-dat_res[ix_nona]+aux
          } else if (argv$time_fun=="max") {
            dat_res[ix_nona]<-pmax(dat_res[ix_nona],aux,na.rm=argv$fun_narm)
          } else if (argv$time_fun=="min") {
            dat_res[ix_nona]<-pmin(dat_res[ix_nona],aux,na.rm=argv$fun_narm)
          } else if (argv$time_fun=="mean") {
            dat_res[ix_nona]<-dat_res[ix_nona]+aux
          }
          rm(ix_nona,aux)
        }
        r<-subset(s,subset=t_ok[1])
        r[]<-NA
        ix<-which(dat_cont>0 & (dat_cont/n_tseq)>=argv$frac)
        if (length(ix)>0) {
          if (argv$time_fun=="mean") {
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
  } # end time aggregation
  #----------------------------------------------------------------------------
  # verification
  if (argv$verif) {
    if (argv$verif_metric %in% c("corr","msess","ets","a","b","c","d",
                                 "seeps","roblinreg","quantile")) {
      if (argv$verif_metric=="corr") {
        threshold  <- NA
        threshold1 <- NA
        type       <- argv$verif_corr_method
      } else if (argv$verif_metric=="msess") {
        threshold  <- NA
        threshold1 <- NA
        type       <- NA
      } else if (argv$verif_metric=="seeps") {
        threshold  <- argv$verif_seeps_threshold
        threshold1 <- argv$verif_seeps_threshold
        type       <- argv$verif_seeps_type
      } else if (argv$verif_metric=="roblinreg") {
        threshold  <- argv$verif_roblinreg_threshold
        threshold1 <- argv$verif_roblinreg_threshold
        type       <- argv$verif_roblinreg_type
      } else if (argv$verif_metric=="quantile") {
        threshold  <- argv$which_quantile
        threshold1 <- NA
        type       <- NA
      } else if (argv$verif_metric %in% c("a","b","c","d","ets")) {
        threshold  <- argv$verif_contab_threshold
        threshold1 <- argv$verif_contab_threshold1
        type       <- argv$verif_contab_type
      }
      npoints <- dim(mat)[1]
      if ( !is.na( argv$cores)) {
        dat_mean <- mcmapply( score_fun,
                              1:npoints,
                              mc.cores   = argv$cores,
                              SIMPLIFY   = T, 
                              lab        = argv$verif_metric,
                              threshold  = threshold,
                              threshold1 = threshold1,
                              type       = type)
      # no-multicores
      } else {
        dat_mean <- mapply( score_fun,
                            1:npoints,
                            SIMPLIFY   = T,
                            lab        = argv$verif_metric,
                            threshold  = threshold,
                            threshold1 = threshold1,
                            type       = type)
      }
      if (exists("mat")) rm(mat)
      if (exists("mat_ref")) rm(mat_ref)
      dat_cont   <- dat_mean
      dat_cont[] <- n
    } else if (argv$verif_metric %in% c("rmse","rmsf")) {
      dat_mean <- sqrt(dat_mean)
    }
    # define r again
    r <- rmaster
    rm( rmaster)
    r[]<-NA
    ix <- which( !is.na(dat_cont) & (dat_cont/n_tseq)>=argv$frac)
    if ( length(ix)>0) r[ix_dat[ix]] <- dat_mean[ix]
    if ( exists( "dat_mean")) rm(dat_mean)
    if ( exists( "dat_cont")) rm(dat_cont)
    if ( exists( "ix_dat"))   rm(ix_dat)
    if ( exists( "ix"))       rm(ix)
  } # end verification
  #----------------------------------------------------------------------------
  # gridded climate indices 
  if ( argv$gridclimind) {
    # indices all dataset in memory
    if ( argv$gridclimind_index %in% c("quantile", "metnoheatwave", "rx5day", "rx3day")) { 
      if (argv$gridclimind_index == "quantile") {
          threshold  <- argv$which_quantile
          threshold1 <- argv$quantile_geq_threshold
          type       <- "above="
      }
      if (argv$gridclimind_index == "metnoheatwave") {
          threshold  <- argv$metnohw_tmin_threshold
          threshold1 <- argv$metnohw_tmax_threshold
          type       <- "above="
      }
      if (argv$gridclimind_index %in% c("rx5day","rx3day")) {
          threshold  <- NA
          threshold1 <- NA
          type       <- ""
      }
      npoints <- dim(mat)[1]
      if ( !is.na( argv$cores)) {
        dat <- mcmapply( score_fun,
                         1:npoints,
                         mc.cores   = argv$cores,
                         SIMPLIFY   = T, 
                         lab        = argv$gridclimind_index,
                         threshold  = threshold,
                         threshold1 = threshold1,
                         type       = type)
      # no-multicores
      } else {
        dat <- mapply( score_fun,
                       1:npoints,
                       SIMPLIFY   = T,
                       lab        = argv$gridclimind_index,
                       threshold  = threshold,
                       threshold1 = threshold1,
                       type       = type)
      }
      # define r again
      r <- rmaster
      rm( rmaster)
      r[] <- NA
      r[ix_dat] <- dat
      if ( exists( "dat")) rm(dat)
    } else {
      # indices computed online
      # define r again
      r <- rmaster
      rm( rmaster)
      r[] <- NA
      ix <- which( !is.na(dat_cont) & (dat_cont/n_tseq)>=argv$frac)
      if ( argv$gridclimind_index == "freq" & argv$freq_as_perc) {
        if ( length(ix)>0) r[ix_dat[ix]] <- dat_aggr[ix] / dat_cont[ix] * 100
      } else {
        if ( length(ix)>0) r[ix_dat[ix]] <- dat_aggr[ix]
      }
      if ( exists( "dat_aggr")) rm(dat_aggr)
      if ( exists( "dat_cont")) rm(dat_cont)
      if ( exists( "ix_dat"))   rm(ix_dat)
      if ( exists( "ix"))       rm(ix)
    }
  } # end gridclimind
  #----------------------------------------------------------------------------
  # temporal trends 
  if (argv$temporal_trend) {
    if ( argv$temporal_trend_elab %in% c( "Theil_Sen_regression", 
                                          "Mann_Kendall_trend_test")) {
      npoints <- dim(mat)[1]
      if ( !is.na( argv$cores)) {
        res <- mcmapply( temporal_trends_fun,
                         1:npoints,
                         mc.cores   = argv$cores,
                         SIMPLIFY   = T, 
                         MoreArgs   = list( lab = argv$temporal_trend_elab))
      # no-multicores
      } else {
        res <- mapply( temporal_trends_fun,
                       1:npoints,
                       SIMPLIFY   = T, 
                       MoreArgs   = list( lab = argv$temporal_trend_elab))
      }
      if (exists("mat")) rm(mat)
      if (exists("mat_ref")) rm(mat_ref)
      dat_cont <- res[3,]
      if ( argv$temporal_trend_elab == "Mann_Kendall_trend_test") {
        # Benjamini‐Hochberg meta test of the p-values used to assess statistical significance
        # REF: Wilks (2019) p. 195
        # Hypothesis testing (problem of test multiplicity, assessing statistical significance). Simplyfing a bit: H0(j)=no-trend at the j-th point; return TRUE(1) if H0(j) can be rejected with the preset global false discovery rate; return FALSE(0) if H0(j) cannot be rejected. The threshold used to assess significance varies from point to point. Results of individual tests are regarded as significant if the corresponding H0 is TRUE(1).
        p_values <- res[2,]
        trend_significance <- p_values; trend_significance[] <- NA
        p_values_adj       <- p_values; p_values_adj[]       <- NA
        if ( (np <- length( ixp <- which( !is.na( p_values)))) > 0) {
          p_values_adj[ixp] <- p.adjust( p_values[ixp], method="BH")
          trend_significance[ixp] <- p_values_adj[ixp] <= argv$temporal_trend_FDR
        }
      }
    }
    # prepare for output
    r <- rmaster
    rm( rmaster)
    r[]<-NA
    ix <- which( !is.na( dat_cont) & ( dat_cont / n_tseq) >= argv$frac)
    if ( length(ix)>0) {
      r1 <- r
      r1[ix_dat[ix]] <- res[1,ix]; r <- r1; r1[] <- NA
      if ( argv$temporal_trend_elab == "Mann_Kendall_trend_test") {
        r1[ix_dat[ix]] <- p_values_adj[ix]; r <- stack( r, r1); r1[] <- NA
      } else {
        r1[ix_dat[ix]] <- res[2,ix]; r <- stack( r, r1); r1[] <- NA
      }
      r1[ix_dat[ix]] <- res[3,ix]; r <- stack( r, r1); r1[] <- NA
      if ( argv$temporal_trend_elab == "Mann_Kendall_trend_test") {
        r1[ix_dat[ix]] <- as.numeric(trend_significance[ix]); r <- stack( r, r1)
      }
    } else {
      r1 <- r
      r <- stack( r, r1); r <- stack( r, r1); r <- stack( r, r1)
      if ( argv$temporal_trend_elab == "Mann_Kendall_trend_test")
        r <- stack( r, r1)
    }
    rm( r1)
    if ( exists( "res"))      rm(res)
    if ( exists( "dat_cont")) rm(dat_cont)
    if ( exists( "ix_dat"))   rm(ix_dat)
    if ( exists( "ix"))       rm(ix)
    if ( exists( "p_values_adj")) rm(p_values_adj)
    if ( exists( "p_values"))     rm(p_values)
    if ( exists( "trend_significance")) rm(trend_significance)
  } # end temporal_trend
  #----------------------------------------------------------------------------
  # data quality control 
  if ( argv$gridded_dqc_afterAgg) {
    # range check_ check for unplausible values
    if ( !is.na( argv$gridded_dqc.min) & 
         !is.na( argv$gridded_dqc.max)) 
      r <- spider_griddqc_range_check()
    # check for holes in the field: remove small patches of connected cells
    if ( !any( is.na( argv$gridded_dqc.clump_r)) & 
         !any( is.na( argv$gridded_dqc.clump_n))) 
      r <- spider_griddqc_cool()
    # check for outliers
    if ( !is.na( argv$gridded_dqc.outlier_aggfact)) 
      r <- spider_griddqc_outliers()
  }
  #----------------------------------------------------------------------------
  # Estimation of verical profile 
  if ( argv$estvertprof) {
    # prepare for output
    r <- rmaster
    rm( rmaster)
    r[]<-NA
    r1 <- r
    r[ix_evp] <- t0_evp
    r1[ix_evp] <- gamma_evp
    r <- stack( r, r1)
    rm (r1, t0_evp, gamma_evp, ix_evp)
  }
  #----------------------------------------------------------------------------
  #-----------------------------------------------------------------------------
  # Gridded output (we want to reduce memory usage, then this is not in a function)
  # adjust
  if ( !exists("r")) r <- s
  if (  exists("s")) rm(s)
  if (argv$temporal_trend) {
    if ( argv$temporal_trend_elab == "Theil_Sen_regression") {
      argv$ffout_varname         <- c( "a", "b", "n")
      argv$ffout_varlongname     <- c( "estimate of the intercept of the temporal trend using Theil-Sen regression",
                                       "estimate of the slope of the temporal trend using Theil-Sen regression",
                                       "number of points used in the elaboration")
      argv$ffout_varstandardname <- c( "intercept", "slope", "counter")
      argv$ffout_varversion      <- c( "1.0", "1.0", "1.0")
      argv$ffout_diground        <- 4
    } else if ( argv$temporal_trend_elab ==  "Mann_Kendall_trend_test") {
      argv$ffout_varname         <- c( "z", "p_value", "n", "trend")
      argv$ffout_varlongname     <- c( "standard Gaussian value Mann-Kendall trend test",
                                       "p-value Mann-Kendall trend test adjusted using Benjamini‐Hochberg method",
                                       "number of points used in the elaboration",
                                       paste0("trend significance with FDR=",
                                              round( argv$temporal_trend_FDR,3)))
      argv$ffout_varstandardname <- c( "standard_Gaussian_value", "p_value", "counter","hypothesis_testing")
      argv$ffout_varversion      <- c( "1.0", "1.0", "1.0", "1.0")
      argv$ffout_diground        <- 6
    }
  }
  if ( argv$estvertprof) {
    argv$ffout_varname         <- c( "t0", "gamma")
    argv$ffout_varlongname     <- c( "estimate of the intercept of the linear vertical profile with z=0m",
                                     "near-surface lapse rate (degC/m)")
    argv$ffout_varstandardname <- c( "intercept", "slope")
    argv$ffout_varversion      <- c( "1.0", "1.0")
    argv$ffout_varunit         <- c( "Celsius", "Celsius/m")
    argv$ffout_diground        <- 4
  }
  # write
  xy <- xyFromCell( r, 1:ncell(r))
  x  <- sort( unique( xy[,1]))
  y  <- sort( unique( xy[,2]), decreasing=T)
  r.list<-list()
  if (nlayers(r)>1) {
    grid <- array( data=NA, dim=c( length(x), length(y), nlayers(r)))
    for (i in 1:nlayers(r)) 
      grid[,,i] <- matrix( data=subset( r, subset=i), 
                           ncol=length(y), nrow=length(x))
    if ( argv$temporal_trend || 
         (argv$gridclimind &  argv$gridclimind_index == "quantile")) {
      date_out <- format( strptime( date_out,
                                    date_out.format,tz="UTC"),
                                    "%Y%m%d%H%M", tz="UTC")
    } else {
      if ( any(is.na(tseq_out))) {
        date_out <- format( tseq[t_ok], "%Y%m%d%H%M", tz="UTC")
      } else {
        date_out <- format( tseq_out[2:n_tseq_out], "%Y%m%d%H%M", tz="UTC")
      }
    }
  } else {
    grid     <- array(  data=NA, dim=c(length(x), length(y)))
    grid[,]  <- matrix( data=r,  ncol=length(y),  nrow=length(x))
    date_out <- format( strptime( date_out,
                                  date_out.format,tz="UTC"),
                                  "%Y%m%d%H%M", tz="UTC")
  }
  if (argv$temporal_trend | argv$estvertprof) {
    for (i in 1:nlayers(r))  r.list[[i]] <- grid[,,i]
  } else {
    r.list[[1]] <- grid
  }
  rm( grid, r)
  if ( any( is.na( argv$time_bnds_string_as_two_dates))) {
    if ( length( date_out) == 1) {
      time_bnds <- array( format( rev( seq(
                    strptime( date_out, "%Y%m%d%H%M", tz="UTC"),
                              length=2, by=argv$time_bnds_string)),
                    format="%Y%m%d%H%M", tz="UTC"), dim=c(1,2))
    } else {
      time_bnds <- array( format( 
       strptime( date_out, "%Y%m%d%H%M", tz="UTC"),
                 format="%Y%m%d%H%M", tz="UTC"), dim=c(1,2))
    }
  } else {
    time_bnds <- array( format( 
       strptime( argv$time_bnds_string_as_two_dates, "%Y%m%d%H%M", tz="UTC"),
                 format="%Y%m%d%H%M", tz="UTC"), dim=c(1,2))
    print(time_bnds)
  }
  if (argv$ffout_revy) {
    y <- rev(y)
    for (i in 1:length(r.list)) {
      if ( length(dim(r.list[[i]])) == 2) {
        for (j in 1:length(x)) 
          r.list[[i]][j,1:length(y)] <- r.list[[i]][j,length(y):1]
      } else if ( length(dim(r.list[[i]])) == 3) {
        for (k in 1:dim(r.list[[i]])[3]) 
          for (j in 1:length(x)) 
            r.list[[i]][j,1:length(y),k] <- r.list[[i]][j,length(y):1,k]
      }
    }
  }
  out <- write_dotnc(grid.list = r.list,
                     times     = date_out,
                     file.name = argv$ffout,
                     grid.type = argv$ffout_gridtype,
                     x = x,
                     y = y,
                     x_round.dig = argv$ffout_x_rounddig,
                     y_round.dig = argv$ffout_y_rounddig,
                     var.name          = argv$ffout_varname,
                     var.longname      = argv$ffout_varlongname,
                     var.standardname  = argv$ffout_varstandardname,
                     var.version       = argv$ffout_varversion,
                     var.cell_methods  = argv$ffout_cell_methods,
                     times.ref         = argv$ffout_times_ref,
                     times.unit        = argv$ffout_times_unit,
                     reference         = argv$ffout_reference,
                     license           = argv$ffout_license,
                     proj4.string      = argv$ffout_proj4,
                     var.unit          = argv$ffout_varunit,
                     lonlat.out        = argv$ffout_lonlat,
                     lonlat_minmax.out = argv$ffout_lonlat_minmax,
                     round.dig         = argv$ffout_diground,
                     summary           = argv$ffout_summary,
                     source.string     = argv$ffout_sourcestring,
                     title             = argv$ffout_title,
                     comment           = argv$ffout_comment,
                     atts.var.add      = NULL,
                     time_bnds         = time_bnds,
                     cf_1.7            = argv$ffout_cf_1.7,
                     cf_1.0            = argv$ffout_cf_1.0)
  t1 <- Sys.time()
  print( paste( "writing output file", argv$ffout,
                " / time", round(t1-t0,1), attr(t1-t0,"unit")))
} # endif gridded_output
#------------------------------------------------------------------------------
# Normal exit
rip( str="Normal Exit", code=0, t0=t0)
