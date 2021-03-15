#+
spider_upscale <- function() {
#------------------------------------------------------------------------------
  values<-getValues(r)
  ix<-which(!is.na(values))
  if (argv$ffin_proj4==argv$ffmaster_proj4) {
    coord.new<-xyFromCell(r,ix)
  } else {
    coord.new<-spTransform( 
                SpatialPoints(xyFromCell(r,ix),
                               proj4string=CRS(argv$ffin_proj4)) 
                                          ,CRS(argv$ffmaster_proj4))
  }
  if (argv$space_fun=="mean") {
    r1<-rasterize(x=coord.new, y=rmaster, field=values[ix], fun=mean)
  } else if (argv$space_fun=="max") {
    r1<-rasterize(x=coord.new, y=rmaster, field=values[ix], fun=max)
  } else if (argv$space_fun=="min") {
    r1<-rasterize(x=coord.new, y=rmaster, field=values[ix], fun=min)
  }
  if (argv$master_mask) r1<-mask(r1,mask=rmaster)
  if (argv$master_trim) r1<-trim(r1)
  r<-r1
  if (!any(!is.na(values<-getValues(r)))) {
    print(paste("warning: all NAs after upscale"))
    return(NULL)
  }
  r
}
