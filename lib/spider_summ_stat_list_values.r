#+
spider_summ_stat_list_values <- function( argv  = NULL, 
                                          r     = NULL,
                                          first = F,
                                          time  = NA) {
#------------------------------------------------------------------------------
  if ( is.null(argv))
    if ( "argv" %in% ls(envir = .GlobalEnv)) 
      get( "argv", envir = .GlobalEnv)
  if ( is.null(r))
    if ( "r" %in% ls(envir = .GlobalEnv)) 
      get( "r", envir = .GlobalEnv)
  # write header, if needed 
  if ( !file.exists(argv$ffout_summ_stat) | 
      (!argv$ffout_summ_stat_append & first)) 
    cat( file = argv$ffout_summ_stat, append=F, 
         paste0("time;label;x;y;value\n"))
  #
  if ( any( is.na( argv$point_mask_x))) {
    labels <- 1:ncell(r)
    xy <- xyFromCell(r,1:ncell(r))
    x <- xy[,1]
    y <- xy[,2]
  } else {
    labels <- argv$point_mask_labels
    x <- round( argv$point_mask_x, 6)
    y <- round( argv$point_mask_y, 6)
  }
  #
  if ( !is.na(argv$ffout_summ_stat_proj4)) {
    if (!compareCRS( crs(argv$ffout_summ_stat_proj4),
                     crs(argv$ffin_proj4))) {
      coord.new <- attr( spTransform( 
                         SpatialPoints(
                          cbind(x,y),
                          proj4string=CRS(argv$ffin_proj4)) 
                                     ,CRS(argv$ffout_summ_stat_proj4)),
                 "coords")
      x<-coord.new[,1]
      y<-coord.new[,2]
    }
  }
  #
  if ( !is.na( argv$list_values_min)) 
    values[ which( values < argv$list_values_min)] <- argv$list_values_min_replace
  if ( !is.na( argv$list_values_max)) 
    values[ which( values > argv$list_values_max)] <- argv$list_values_max_replace
  cat(file=argv$ffout_summ_stat,append=T,
      paste0( t_to_read,";",
              labels,";",
              round(      x, argv$list_values_coord_rounddig),";",
              round(      y, argv$list_values_coord_rounddig),";",
              round( values, argv$list_values_rounddig),"\n",collapse=""))
}

