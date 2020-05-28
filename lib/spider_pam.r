#+ pam, plot a map
spider_pam <- function( argv=NA,
                        time=NA,
                        r=NA) {
#------------------------------------------------------------------------------
  if (is.na(argv)) 
    if ("argv" %in% ls(envir = .GlobalEnv)) get("argv", envir = .GlobalEnv)
  if (is.na(r)) 
    if ("r" %in% ls(envir = .GlobalEnv)) get("r", envir = .GlobalEnv)
  #
  ffout_png <- replaceDate( string   = argv$ffout_pam_template,
                            date.str = format(time,
                                       format=argv$ffin_date.format,tz="GMT"),
                            year_string  = argv$year_string,
                            month_string = argv$month_string,
                            day_string   = argv$day_string,
                            hour_string  = argv$hour_string,
                            sec_string   = argv$sec_string,
                            format       = argv$ffin_date.format)
 pam( ffout = list( name   = ffout_png,
                    width  = argv$pam_width,
                    height = argv$pam_height),
     borders_par = list(name  = argv$pam_ffshp_borders,
                        layer = argv$pam_ffshp_borders_layer,
                        lwd   = argv$pam_ffshp_borders_lwd),
     fig_par = list( mar  = c(.5,.5,.5,.5),
                     xlim = argv$pam_xlim,
                     ylim = argv$pam_ylim,
                     fool_coltab     = argv$pam_fool_coltab,
                     fool_coltab_rev = argv$pam_fool_coltab_rev,
                     fool_path       = argv$pam_fool_path,
                     fool_breaks     = argv$pam_fool_breaks,
                     breaks          = argv$pam_breaks,
                     disagg_fact     = argv$pam_disagg_fact,
                     colors          = argv$pam_colors),
     leg_par = list( type   = argv$pam_leg_type,
                     nticks = argv$pam_leg_cbar_nticks,
                     height = argv$pam_leg_height,
                     dig    = argv$pam_leg_dig),
     raster_to_plot = r
    ) 
}
