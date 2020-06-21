# LATTE-interpoLATion based on a non-linear verTical profilE 
#------------------------------------------------------------------------------
latte_express <- function( box_o_nearest_halfwidth = 100000,  # m
                           pmax                    = 100000,  # mx num of point
                           gamma                   = -0.0065, # degC / m
                           agg_fact                = 50, # number of grid points
                           weight_dh_scale         = 25000 # m
                         ) {
#------------------------------------------------------------------------------
#
# --> Input  <--
# --> Output <--
#
# variable_abbreviation/grid_abbrevation/_/selection_abbreviation
# - grid abbrevations
#  ma = master grid, target (finer resolution)
#  or = original grid, initial (coarser resolution)
# - selection abbreviations
#  tot = all points
#  tar = target points
# - variable abbreviations
#  a = aux
#  x = easting coord
#  y = northing coord
#  z = elevation
#  n = number
#  v = value (e.g. temperature)
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
  #
  # aggregated grid - parameters for sub-regional vertical profiles
  s <- aggregate( r, fact=agg_fact)
  if ( length( ix_ag <- which( !is.na( getValues(s))))==0) {
    print("all NAs for intersection of agg")
    return( NULL)
  }
  xy_ag   <- xyFromCell( s, ix_ag)
  ix_ag1  <- which( !is.na(extract( rmaster, xy_ag)))
  ix_ag   <- ix_ag[ix_ag1]
  nag_tar <- length( ix_ag)
  xag_tar <- xy_ag[ix_ag1,1]
  yag_tar <- xy_ag[ix_ag1,2]
  #
  # 
  cat( "LATTE-xpress - step 1, start ...")
  cat( paste( "#or/ #tar/ #agg:", nor_tar, "/", nma_tar, "/", nag_tar))
  #
  if ( !is.na( argv$cores)) {
    res_step1 <- t( mcmapply( latte_express_step1,
                     1:nag_tar,
                     mc.cores                = argv$cores,
                     SIMPLIFY                = T,
                     MoreArgs = list(
                       box_o_nearest_halfwidth = argv$latte_halfbox,
                       pmax    = argv$latte_pmax,
                       gamma   = argv$latte_gamma,
                       xag_tar = xag_tar,
                       yag_tar = yag_tar,
                       yor_tar = yor_tar,
                       xor_tar = xor_tar,
                       zor_tar = zor_tar,
                       vor_tar = vor_tar,
                       vmin    = vmin,
                       vmax    = vmax)))
  # no-multicores
  } else {
    res_step1 <- t(mapply(   latte_express_step1,
                     1:nag_tar,
                     SIMPLIFY                = T,
                     MoreArgs = list(
                       box_o_nearest_halfwidth = argv$latte_halfbox,
                       pmax    = argv$latte_pmax,
                       gamma   = argv$latte_gamma,
                       xag_tar = xag_tar,
                       yag_tar = yag_tar,
                       yor_tar = yor_tar,
                       xor_tar = xor_tar,
                       zor_tar = zor_tar,
                       vor_tar = vor_tar,
                       vmin    = vmin,
                       vmax    = vmax)))
  }
  cat(paste(" done!",round(Sys.time()-t0,1), attr(Sys.time()-t0,"unit"),"\n"))
  if ( any( is.na( res_step1[1,]))) 
    print(paste0("@@ warning: problems in regridding over ",
                 length( which( is.na( res_step1[1,]))), " points"))
  #
  cat( "LATTE-xpress - step2, start ...")
  if ( !is.na( argv$cores)) {
    res <- mcmapply( latte_express_step2,
                     1:nma_tar,
                     mc.cores = argv$cores,
                     SIMPLIFY = T,
                     MoreArgs = list(
                       xma_tar    = xma_tar,
                       yma_tar    = yma_tar,
                       zma_tar    = zma_tar,
                       xag_tar    = xag_tar,
                       yag_tar    = yag_tar,
                       par        = res_step1,
                       dh_ref     = weight_dh_scale,
                       zbilma_tar = getValues( resample( r_dem, rmaster))[ix_ma],
                       vbilma_tar = getValues( resample( r, rmaster))[ix_ma]))
  # no-multicores
  } else {
     res <- mapply( latte_express_step2,
                     1:nma_tar,
                     SIMPLIFY = T,
                     MoreArgs = list(
                       xma_tar    = xma_tar,
                       yma_tar    = yma_tar,
                       zma_tar    = zma_tar,
                       xag_tar    = xag_tar,
                       yag_tar    = yag_tar,
                       par        = res_step1,
                       dh_ref     = weight_dh_scale,
                       zbilma_tar = getValues( resample( r_dem, rmaster))[ix_ma],
                       vbilma_tar = getValues( resample( r, rmaster))[ix_ma]))
  }
  cat(paste("done!",round(Sys.time()-t0,1), attr(Sys.time()-t0,"unit"),"\n"))
#  if ( any( is.na( res))) 
#    print(paste0("@@ warning: problems in regridding over ",
#                 length( which( is.na( res))), " points"))
#rsam<-resample( r, rmaster)
#rsam_dem<-resample( r_dem, rmaster)
#save( file="tmp1.rdata", res,r,rmaster,r_dem,rsam,rsam_dem,ix_ma,nag_tar,argv,nma_tar,xma_tar,yma_tar,zma_tar,xag_tar,yag_tar,par)
#for (j in 1:nag_tar) {
#  print(j)
#  da<- sqrt( (xag_tar[j]-xma_tar)**2 + (yag_tar[j]-yma_tar)**2)
#  ix<- which( da<50000 )
#xr<-range(c(getValues(rsam)[ix_ma][ix],res[ix]),na.rm=T)
#yf<-tvertprof_Frei_2014( z=zma_tar[ix], t0=res_step1[j,1], gamma=res_step1[j,2], 
#               a=res_step1[j,3], h0=res_step1[j,4], h1i=res_step1[j,5]) 
#png(file=paste0("compare/fig_",formatC(j,width=7,flag="0",format="d"),".png"),width=600,height=800)
#par(mar=c(4,5,3,1),cex.axis=1.5)
#plot( getValues(rsam)[ix_ma][ix], getValues(rsam_dem)[ix_ma][ix],cex=1, ylim=c(0,2000),
#      xlim=xr,
#      xlab="Temperature (degC)",ylab="Elevation (m amsl)",
#      main="",pch=21,bg="gray",axes=F)
#points( res[ix], zma_tar[ix],cex=1,pch=21,bg="cornflowerblue")
#points( yf, zma_tar[ix],cex=1,pch=21,bg="gold")
#abline(h=seq(0,3000,by=50),lwd=1,lty=2,col="gray")
#abline(h=seq(0,3000,by=100),lwd=1,lty=2,col="gray")
#abline(h=c(0,1000,2000),lwd=3,col="gray")
#ts<--50:50
#for (t0 in seq(-50,50,by=1)) {
#  lines(ts,1/gamma*(ts-t0),lty=2,lwd=2,col="gray")
#}
#
#
#box()
#dev.off()
#}
#save(file="tmp1.rdata",res,r,rmaster,ix_ma)
#,r_dem,
#     xma_tar,yma_tar,zma_tar,
#     vor,xor,yor,zor,
#     res,par,ixok,ixno, resl)
  return( res)
}
