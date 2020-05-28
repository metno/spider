#+
spider_summ_stat_wave_nrgx <- function( argv  = NULL, 
                                        r     = NULL) {
#------------------------------------------------------------------------------
  if ( is.null(argv))
    if ( "argv" %in% ls(envir = .GlobalEnv)) 
      get( "argv", envir = .GlobalEnv)
  if ( is.null(r))
    if ( "r" %in% ls(envir = .GlobalEnv)) 
      get( "r", envir = .GlobalEnv)
  #
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
    }
    # energies of mother wavelets
    En2o_boot[1:N,boot]<-En2o[1:N]
    # nnscales = N+1, energy of the father wavelet = mean(obs)**2
    En2o_boot[(N+1),boot]<-cellStats(rboot,stat="mean",na.rm=T)**2
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
  }
  cat(file=argv$ffout_summ_stat,append=T,
      paste(t_to_read,
            gsub(",",";",toString(round(En2o_t,9))),"\n",sep=";"))
}
