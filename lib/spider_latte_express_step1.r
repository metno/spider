
# 
#------------------------------------------------------------------------------
latte_express_step1 <- function( i,
                                 box_o_nearest_halfwidth=100000, #m
                                 pmax,
                                 gamma=NA,
                                 xag_tar,
                                 yag_tar,
                                 xor_tar,
                                 yor_tar,
                                 zor_tar,
                                 vor_tar,
                                 zseq_min=100,
                                 zseq_max=3000,
                                 zseq_dz=100,
                                 vmin=NA,
                                 vmax=NA) {
#------------------------------------------------------------------------------
# select the p_i observations nearest to the i-th gridpoint
  if (i%%1000==0) print(paste(i,Sys.time()-t0))
  deltax <- abs( xag_tar[i] - xor_tar)
  if ( !any( flagx <- ( deltax < box_o_nearest_halfwidth))) return( rep( NA, 5)) 
  deltay <- abs( yag_tar[i]-yor_tar[flagx])
  if ( !any( flagy <- ( deltay < box_o_nearest_halfwidth))) return( rep( NA, 5)) 
  ixa <- which( flagx)[flagy]
  if ( length( ixa) == 0) return( rep( NA, 5)) 
  #
  if ( length(ixa) > pmax) {
    disth2 <- deltax[ixa]*deltax[ixa]+deltay[flagy]*deltay[flagy]
    ixb    <- order(disth2, decreasing=F)[1:pmax]
    ixa    <- ixa[ixb]
    disth2 <- disth2[ixb]
    rm( ixb)
  }
  p_i <- length( ixa)
  lopt <- optimize( f          = opt_vertprof_basic,
                    interval   = c(vmin,vmax),
                    vert_coord = zor_tar[ixa],
                    gamma      = gamma,
                    obs        = vor_tar[ixa])
  yb_lin <- tvertprof_basic( z     = zor_tar[ixa], 
                             t0    = lopt$minimum,
                             gamma = gamma)
  
  zq <- as.numeric( quantile( zor_tar[ixa], probs = c( 0.1, 0.25, 0.75, 0.9)))
  sd <- zor_tar[ixa]; sd[] <- NA
  for (j in seq(zseq_min,zseq_max,by=zseq_dz)) {
    if (j==100) { ix <- which( zor_tar[ixa] < zseq_min) }
    else if (j==3000) { ix <- which( zor_tar[ixa] >= zseq_max) }
    else { ix <- which( zor_tar[ixa] >= j & zor_tar[ixa] < (j+zseq_dz) ) }
    if (length(ix)<10) next 
    sd[ix] <- max( c( .1, sd( vor_tar[ixa][ix] - yb_lin[ix])))
  }
  sd[is.na(sd)] <- max( sd, na.rm=T)
  opt <- constrOptim( theta = c( lopt$minimum, gamma, sd( (vor_tar[ixa]-yb_lin)), zq[2], max(c(10,zq[3]-zq[2]))),
                      grad  = NULL,
                      f     = opt_vertprof_Frei_2014,
                      ui    = rbind( c( 1, 0, 0, 0, 0), c( -1,  0,  0,  0,  0), 
                                     c( 0, 1, 0, 0, 0), c(  0, -1,  0,  0,  0),
                                     c( 0, 0, 1, 0, 0), c(  0,  0, -1,  0,  0),
                                     c( 0, 0, 0, 1, 0), c(  0,  0,  0, -1,  0),
                                     c( 0, 0, 0, 0, 1), c(  0,  0,  0,  0, -1)),
                      ci= c(  -60, -60,
                             -0.1, 0.001,
                              -50, -50,
                             -100, -3000,
                             -100, -3000),
                      sd = sd,
                      vert_coord = zor_tar[ixa],
                      obs        = vor_tar[ixa])
  res <- c( lopt$minimum, opt$par, range(zor_tar[ixa]), range(vor_tar[ixa]))
# plot for debug
yb_frei <- tvertprof_Frei_2014( z    = zor_tar[ixa],
                     t0    = opt$par[1],
                     gamma = opt$par[2],
                     a     = opt$par[3],
                     h0    = opt$par[4],
                     h1i   = opt$par[5])
xr<-range(c(vor_tar[ixa],yb_frei,yb_lin))
yr<-range(zor_tar[ixa])
if ( (i %%1) ==0) {
png(file=paste0("bkg/fig_",formatC(i,width=7,flag="0",format="d"),".png"),width=600,height=800)
par(mar=c(4,5,3,1),cex.axis=1.5)
plot(vor_tar[ixa], zor_tar[ixa],cex=2,xlim=xr,ylim=c(0,1550),
     xlab="Temperature (degC)",ylab="Elevation (m amsl)",main="",pch=21,bg="gray")
points(yb_lin,  zor_tar[ixa],pch=21,bg="gold",cex=2)
points(yb_frei, zor_tar[ixa],pch=21,bg="red",cex=3 )
abline(h=seq(0,3000,by=50),lwd=1,lty=2,col="gray")
abline(h=seq(0,3000,by=100),lwd=1,lty=2,col="gray")
abline(h=c(0,1000,2000),lwd=3,col="gray")
ts<--50:50
for (t0 in seq(-50,50,by=1)) {
  lines(ts,1/gamma*(ts-t0),lty=2,lwd=2,col="gray")
}
par(new=T)
plot(sd,zor_tar[ixa],xlim=c(0,max(sd)),ylim=c(0,1550),axes=F)
axis(3)
dev.off()
png(file=paste0("map/fig_",formatC(i,width=7,flag="0",format="d"),".png"),width=600,height=800)
par(mar=c(1,1,1,1))
plot(xag_tar, yag_tar,cex=.1,
     xlab="",ylab="",main="",col="gray",axes=F)
points(xor_tar, yor_tar,cex=.1,col="lightgray")
rect( min(xor_tar[ixa]), min(yor_tar[ixa]),
      max(xor_tar[ixa]), max(yor_tar[ixa]),lwd=2 )
points(xag_tar[i], yag_tar[i],pch=21,bg="red",cex=.5 )
box()
dev.off()
print(i)
}


  res <- opt$par
  return( res)
}


