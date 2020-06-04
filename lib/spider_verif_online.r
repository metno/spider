#+
spider_verif_online <- function( argv   = NULL, 
                                 r      = NULL,
                                 r_ref  = NULL,
                                 first  = F,
                                 time   = NA) {
#------------------------------------------------------------------------------
  if ( is.null(argv))
    if ( "argv" %in% ls(envir = .GlobalEnv)) 
      get( "argv", envir = .GlobalEnv)
  if ( is.null(r))
    if ( "r" %in% ls(envir = .GlobalEnv)) 
      get( "r", envir = .GlobalEnv)
  if ( is.null(r_ref))
    if ( "r_ref" %in% ls(envir = .GlobalEnv)) 
      get( "r_ref", envir = .GlobalEnv)

  nr <- length(argv$verif_r)
  if ( nr > 0) {
    if (argv$verif_b=="=within" | argv$verif_b=="within" |
        argv$verif_b=="within=" | argv$verif_b=="=within=") {
      score<-vector(mode="numeric",length=(nr-1))
    } else {
      score<-vector(mode="numeric",length=nr)
    }
    score[]<-NA
  } else {
    score<-NA
  }
  #
  # first time in, define variables
  # NOTE: case of "within", num is a vector with dimension nr-1
  #       otherwise, num is a vector with dimension nr
  if ( first) {
    header_string <- integer(0)
    if ( nr > 0) {
      for (i in 1:nr) {
        if (argv$verif_b=="=within" | argv$verif_b=="within" |
            argv$verif_b=="within=" | argv$verif_b=="=within=") {
          if ( i< nr) header_string<-paste0(header_string,
                                            paste0(";thr_",argv$verif_b,"_",
                                                   argv$verif_r[i],"_",
                                                   argv$verif_r[(i+1)]))
        } else {
          header_string<-paste0(header_string,
                                paste0(";thr_",argv$verif_b,"_",
                                       argv$verif_r[i]))
        }
      }
      header_string<-paste0(header_string,";numtot")
    # no thresolds
    } else {
      header_string<-paste0(";score;numtot")
    }
  }
  # if required, write the file header
  if (!file.exists(argv$ffout_verif) | 
       (!argv$ffout_verif_append & first)) {
     cat(file=argv$ffout_verif,append=F,
         paste0("time",header_string,"\n"))
  }
  # compute score
  val<-getValues(r)
  ref<-getValues(r_ref)
  numtot<-length(aux<-which(!is.na(val) & !is.na(ref)))
  val<-val[aux]
  ref<-ref[aux]; rm(aux)
  # use thresholds if needed
  if (nr>0) {
    score[]<-NA
    for (i in 1:nr) {
      if ( argv$verif_b %in% c("within","=within","within=","=within=") ) {
        if (i<nr) score[i]<-score_fun(x=val,
                                      x_ref=ref,
                                      lab=argv$verif_metric,
                                      threshold=argv$verif_r[i],
                                      threshold1=argv$verif_r[(i+1)],
                                      type=argv$verif_b)
      } else { 
        score[i]<-score_fun(x=val,
                            x_ref=ref,
                            lab=argv$verif_metric,
                            threshold=argv$verif_r[i],
                            type=argv$verif_b)
      }
    }
  } else {
    score<-score_fun(x=val,
                     x_ref=ref,
                     lab=argv$verif_metric)
  } # end compute score
  rm(val,ref)
  # output
  if (nr>0) {
    data_string<-integer(0)
    for (i in 1:nr) {
      if ( argv$verif_b %in% c("within","=within","within=","=within=") ) {
        if (i<nr) data_string<-paste0(data_string,";",score[i])
      } else {
        data_string<-paste0(data_string,";",score[i])
      }
    }
    data_string<-paste0(data_string,";",numtot)
  } else {
    data_string<-paste0(score,";",numtot)
  }
  cat(file=argv$ffout_verif,append=T,
      paste0(time,";",data_string))
}
