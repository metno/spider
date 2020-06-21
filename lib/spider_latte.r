#+
latte <- function( box_o_nearest_halfwidth = 100000,  # m
                   pmax                    = 100000,  # mx num of point
                   gamma                   = -0.0065  # degC / m
                 ) {
#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------
  # -- Initialization --
  #
  # master grid - target selection
  if ( length( ix_ma <- which( 
                    !is.na( getValues(rmaster)) & 
                    !is.na( zma_tot <- getValues(rmaster_dem))))==0) {
    print("all NAs for intersection of master & master_dem")
    return( NULL)
  }
  xy_ma   <- xyFromCell( rmaster, ix_ma) #dim nma_tar 2
  nma_tar <- length( ix_ma)
  xma_tar <- xy_ma[,1]
  yma_tar <- xy_ma[,2]
  zma_tar <- zma_tot[ix_ma]
  #
  # original grid
  if ( length( ix_or <- which( 
                    !is.na( vor_tot <- getValues(r)) & 
                    !is.na( zor_tot <- getValues(r_dem))))==0) {
    print("all NAs for intersection of data & data_dem")
    return( NULL)
  }
  if (argv$ffin_proj4 == argv$ffmaster_proj4) {
    coord.new <- xyFromCell( r, ix_or) 
  } else {
    cat("coordinate conversion...")
    coord.new <- spTransform( 
                  SpatialPoints(xyFromCell( r, ix_or),
                                 proj4string = CRS(argv$ffin_proj4)) 
                                              ,CRS(argv$ffmaster_proj4))
    coord.new <- attr( coord.new, "coords")
    cat( "ok!\n")
  }
  nor_tar <- length( ix_or)
  xor_tar <- coord.new[,1]
  yor_tar <- coord.new[,2]
  zor_tar <- zor_tot[ix_or]
  vor_tar <- vor_tot[ix_or]
  vmin <- min( vor_tar) - as.numeric( diff( range( vor_tar)))
  vmax <- max( vor_tar) + as.numeric( diff( range( vor_tar)))
  if (!is.na(argv$cores)) {
    res <- mcmapply( oi_var_gridpoint_by_gridpoint,
                     1:nma_tar,
                     mc.cores                = argv$cores,
                     SIMPLIFY                = T,
                     MoreArgs = list(
                       box_o_nearest_halfwidth = argv$latte_halfbox,
                       pmax                    = argv$latte_pmax,
                       fg                      = argv$latte_fglab,
                       fg_gamma                = argv$latte_gamma,
                       fg_min                  = vmin,
                       fg_max                  = vmax,
                       return_fg_only          = T,
                       xgrid_spint = xma_tar,
                       ygrid_spint = yma_tar,
                       zgrid_spint = zma_tar,
                       xobs_spint  = xor_tar,
                       yobs_spint  = yor_tar,
                       zobs_spint  = zor_tar,
                       yo_spint    = vor_tar))
  # no-multicores
  } else {
    res <- mapply(   oi_var_gridpoint_by_gridpoint,
                     1:nma_tar,
                     SIMPLIFY                = T,
                     MoreArgs = list(
                       box_o_nearest_halfwidth = argv$latte_halfbox,
                       pmax                    = argv$latte_pmax,
                       fg                      = argv$latte_fglab,
                       fg_gamma                = argv$latte_gamma,
                       fg_min                  = vmin,
                       fg_max                  = vmax,
                       return_fg_only          = T,
                       xgrid_spint = xma_tar,
                       ygrid_spint = yma_tar,
                       zgrid_spint = zma_tar,
                       xobs_spint  = xor_tar,
                       yobs_spint  = yor_tar,
                       zobs_spint  = zor_tar,
                       yo_spint    = vor_tar))
  }
  cat(paste("done!",round(Sys.time()-t0,1), attr(Sys.time()-t0,"unit"),"\n"))
  res
}
