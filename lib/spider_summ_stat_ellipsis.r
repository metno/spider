#+
spider_summ_stat_ellipsis <- function( argv  = NULL, 
                                       r     = NULL,
                                       first = F,
                                       time  = NA) {
#------------------------------------------------------------------------------
  if ( is.null(argv))
    if ( "argv" %in% ls(envir = .GlobalEnv)) 
      argv <- get( "argv", envir = .GlobalEnv)
  if ( is.null(r))
    if ( "r" %in% ls(envir = .GlobalEnv)) 
      r <- get( "r", envir = .GlobalEnv)
  #
  dat <- getValues( r)
  ix  <- score_fun( x=dat,
                    lab="index_x",
                    threshold=argv$summ_stat_r,
                    type=argv$summ_stat_b) 
  dat[]   <- NA
  dat[ix] <- 1
  r[]     <- dat
  r       <- clump(r,directions=8,gaps=F)
  clump_lab <- freq(r)
  ix_clump_big  <- which(!is.na(clump_lab[,1]) & clump_lab[,2]>100)
  clump_lab_val <- clump_lab[ix_clump_big,1]
  clump_lab_n   <- clump_lab[ix_clump_big,2]
  ix_r_clump_small <- which(!(getValues(r) %in% clump_lab_val))
  r[ix_r_clump_small] <- NA
  dat <- getValues(r)
  xy  <- xyFromCell(r,1:ncell(r))
  x <- xy[,1]
  y <- xy[,2]
  n_clump<-length(clump_lab_val)
  if ( n_clump > 0 ) { 
    ell_x <- vector(mode="numeric",length=n_clump); ell_x[]<-NA
    ell_y <- vector(mode="numeric",length=n_clump); ell_y[]<-NA
    ell_smajor <- vector(mode="numeric",length=n_clump); ell_smajor[]<-NA
    ell_sminor <- vector(mode="numeric",length=n_clump); ell_sminor[]<-NA
    ell_smadir_eve <- vector(mode="numeric",length=n_clump); ell_smadir_eve[]<-NA
#png(file=paste0("ellipsis.png"),width=800,height=800)
#image(r)
    ell_list_t <- list()
    for (i in 1:n_clump) {
      ixi <- which(dat==clump_lab_val[i])
      xy  <- cbind(x[ixi],y[ixi])
      ell <- ellipsoidhull(xy)
#print(class(ell))
      ell_list_t[[i]] <- ell
      ell_x[i] <- ell$loc[1]
      ell_y[i] <- ell$loc[2]
      eigenval <- eigen(ell$cov)$values
      eigenvec <- eigen(ell$cov)$vectors
      e <- sqrt(eigenval)
      ell_smajor[i] <- sqrt(ell$d2) * e[1] /1000  # semi-major axis [Km]
      ell_sminor[i] <- sqrt(ell$d2) * e[2] /1000  # semi-minor axis [Km]
      # orientation of the ellipse: angle between the major axis and the y-axis
      # dir=0 N-S orientation; dir=45 NE-SW; dir=90 E-W; dir=135 NW-SE; dir=180 N-S
      ell_smadir_eve[i] <- atan2(eigenvec[1,1],eigenvec[2,1])
      if (ell_smadir_eve[i]<0) ell_smadir_eve[i] <- ell_smadir_eve[i]+pi
      ell_smadir_eve[i] <- ell_smadir_eve[i]/pi*180.
      ell_list_t[[i]]$add_el <- c( ell_smajor[i], ell_sminor[i], ell_smadir_eve[i])
#lines(predict(ell))
    }
  } else {
    ell_list_t <- NULL
  }
#dev.off()
#next
  ell_list_t
}
