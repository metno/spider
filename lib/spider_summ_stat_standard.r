#+
spider_summ_stat_standard <- function( argv   = NULL, 
                                       r      = NULL,
                                       first  = F,
                                       ncells = NA,
                                       fcells = NA,
                                       time   = NA) {
#------------------------------------------------------------------------------
  if ( is.null(argv))
    if ( "argv" %in% ls(envir = .GlobalEnv)) 
      get( "argv", envir = .GlobalEnv)
  if ( is.null(r))
    if ( "r" %in% ls(envir = .GlobalEnv)) 
      get( "r", envir = .GlobalEnv)
  #
  values <- getValues(r)
  if (!file.exists(argv$ffout_summ_stat) | 
      (!argv$ffout_summ_stat_append & first)) {
    cat(file=argv$ffout_summ_stat,append=F,
        paste0("time;mean;stdev;min;",
               "q01;q05;q10;q20;q25;q50;q75;q80;q90;q95;q99;",
               "max;thr_gt;ncell;fcell\n"))
  }
  qvec<-as.vector(quantile(values[ixvalid],
        probs=c(0,0.01,0.05,0.1,0.2,0.25,0.5,0.75,0.8,0.9,0.95,0.99,1)))
  cat(file=argv$ffout_summ_stat,append=T,
      paste0(time,";",
             round(mean(values[ixvalid]),3),";",
             round(sd(values[ixvalid]),3),";",
             round(qvec[1],3),";",
             round(qvec[2],3),";",
             round(qvec[3],3),";",
             round(qvec[4],3),";",
             round(qvec[5],3),";",
             round(qvec[6],3),";",
             round(qvec[7],3),";",
             round(qvec[8],3),";",
             round(qvec[9],3),";",
             round(qvec[10],3),";",
             round(qvec[11],3),";",
             round(qvec[12],3),";",
             round(qvec[13],3),";",
             round(argv$summ_stat_condition_threshold,4),";",
             round(ncells,0),";",
             round(fcells,4),"\n"))
}
