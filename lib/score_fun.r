#+ verification scores and summary statistics 
score_fun<-function(i=NA,
                    x=NA,
                    x_ref=NA,
                    lab="msess",
                    threshold=NA,
                    threshold1=NA,
                    type=NA) {
#------------------------------------------------------------------------------
# mandatory input is one dataset, that is the one that is either considered (i)
# for the calculation of summary statistics or (ii) as the datasets to 
# evaluate for verification. In the case of verification, then a second dataset
# must be given as input, that is the "reference" dataset.
# input dataset can be passed in two ways:
#  - as the i-th row of a matrix mat[1:nrows,1:ncols]
#  - as the vector x
# reference dataset can be passed also in two ways:
#  - as the i-th row of a matrix mat_ref[1:nrows,1:ncols]
#  - as the vector x_ref
# NOTES:
#  - lab in "count_x","a","b","c","d","msess","bias","mae","rmse","mbias",
#           "rmsf","seeps","corr", "index_x"
#  - NAs are allowed in the input datasets
#  - scores "a"..."d", the order does not matter, simply count occurrences
#  - for other scores:
#    - it is assumed that input and reference datasets have the same length
#    - use all not-NA pairs
#    - thresholding (if needed) is done on the reference values
#  - cor, type defines the method "pearson" (default), "kendall", "spearman" 
#  - SEEPS:
#    - thresholding is not allowed here
#    - threshold specify the precip yes/no thresholds (default 0.25 mm)
#    - if dataset to evaluate and reference have different prec y/n thresholds
#      then use threshold for the dataset and threshold1 for the reference 
#    - as in the references, 1=dry, 2=light, 3=heavy
#    - based on p3=p2/2 (and p1+p2+p3=1)
#    - not applicable if p1<0.1 or p1>0.85
# SEEPS References: 
#  Rodwell MJ, Richardson DS, Hewson TD, Haiden T. 2010. A new equitable 
#   score suitable for verifying precipitation in numerical weather 
#   prediction. Q. J. R. Meteorol. Soc. 136: 1344–1363. DOI:10.1002/qj.656
#  ECMWF Technical Memorandum n.665
#  F.A. Isotta et al.: Evaluation of European regional reanalyses and 
#   downscalings. Meteorol. Z., 24, 2015
# WRT Rodwell/ECMWF: mat_ref = obs (v, reference dataset) 
#                        mat = forecast (f, dataset to evaluate)
# threshold1 defines the prec yes/no threshold for v
#  threshold defines the prec yes/no threshold for f
# default to the values of hourly precipitation (see ECMWF memo)
#------------------------------------------------------------------------------
  # transform x and x_ref into 1D mat and mat_ref
  if (is.na(i)) {
    i<-1
    if (exists("x")) mat<-array(data=x,dim=c(1,length(x)))
    if (!any(!is.na(mat))) return(NA)
    if (exists("xref")) {
      mat_ref<-array(data=x_ref,dim=c(1,length(x_ref)))
      if (!any(!is.na(mat_ref))) return(NA)
    }
  }
  # count_x is the only score that do not use mat_ref
  if (lab=="count_x" | lab=="index_x") {
    if (is.na(type)) return(NA)
    if (type=="below") {
      score<-which(mat[i,]<threshold)
    } else if (type=="below=") {
      score<-which(mat[i,]<=threshold)
    } else if (type=="=within") {
      score<-which(mat[i,]>=threshold & mat[i,]<threshold) 
    } else if (type=="within") {
      score<-which(mat[i,]>threshold & mat[i,]<threshold) 
    } else if (type=="within=") {
      score<-which(mat[i,]>threshold & mat[i,]<=threshold) 
    } else if (type=="=within=") {
      score<-which(mat[i,]>=threshold & mat[i,]<=threshold) 
    } else if (type=="above") {
      score<-which(mat[i,]>threshold) 
    } else if (type=="above=") {
      score<-which(mat[i,]>=threshold) 
    }
    if (lab=="count_x") score<-length(score)
  # use the reference data. not assume temporal allignment mat and mat_ref
  } else if (lab %in% c("a","b","c","d","ets")) {
    if (is.na(type)) return(NA)
    n_val<-length( ix_val <- which(!is.na(mat[i,])) )
    n_ref<-length( ix_ref <- which(!is.na(mat_ref[i,])) )
    if (n_val==0 | n_ref==0) return(NA)
    if ( is.na(threshold))  threshold<-0.25 # mm
    if (is.na(threshold1)) threshold1<-threshold
    if (type %in% c("wet","dry","light","heavy")) {
      if (type %in% c("light","heavy")) {
        prob_light_to_heavy<-2 # light is twice as much likely than heavy
        v_dry<- mat_ref[i,ix_ref] < threshold1
        v_p<-vector(mode="numeric",length=2)
        v_p[1]<- length(which(v_dry)) / n_ref
        v_p[2]<- prob_light_to_heavy/(prob_light_to_heavy+1) * (1-v_p[1])
        # define thresholds (quantiles)
        v_q<-vector(mode="numeric",length=3)
        v_q<- c( threshold1, 
                 as.numeric(quantile( mat_ref[i,ix_ref[which(!v_dry)]], 
                            probs=(v_p[1]+v_p[2]), type=4)) )
        if (type == "light" ) {
          threshold<-v_q[1]
          threshold1<-v_q[2]
          type<-"=within"
        } else { # heavy
          threshold<-v_q[2]
          threshold1<-v_q[2]
          type<-"above="
        }
      } else if (type == "dry") {
        type<-"below"
      } else if (type == "wet") {
        type<-"above="
      }
    }
    if (type=="below") {
      yes_val<-which(mat[i,ix_val]<threshold)
      yes_ref<-which(mat_ref[i,ix_ref]<threshold1)
    } else if (type=="below=") {
      yes_val<-which(mat[i,ix_val]<=threshold)
      yes_ref<-which(mat_ref[i,ix_ref]<=threshold1)
    } else if (type=="=within") {
      yes_val<-which(mat[i,ix_val]>=threshold & mat[i,ix_val]<threshold) 
      yes_ref<-which(mat_ref[i,ix_ref]>=threshold1 & mat_ref[i,ix_ref]<threshold1) 
    } else if (type=="within") {
      yes_val<-which(mat[i,ix_val]>threshold & mat[i,ix_val]<threshold) 
      yes_ref<-which(mat_ref[i,ix_ref]>threshold1 & mat_ref[i,ix_ref]<threshold1) 
    } else if (type=="within=") {
      yes_val<-which(mat[i,ix_val]>threshold & mat[i,ix_val]<=threshold) 
      yes_ref<-which(mat_ref[i,ix_ref]>threshold1 & mat_ref[i,ix_ref]<=threshold1) 
    } else if (type=="=within=") {
      yes_val<-which(mat[i,ix_val]>=threshold & mat[i,ix_val]<=threshold) 
      yes_ref<-which(mat_ref[i,ix_ref]>=threshold1 & mat_ref[i,ix_ref]<=threshold1) 
    } else if (type=="above") {
      yes_val<-which(mat[i,ix_val]>threshold) 
      yes_ref<-which(mat_ref[i,ix_ref]>threshold1) 
    } else if (type=="above=") {
      yes_val<-which(mat[i,ix_val]>=threshold) 
      yes_ref<-which(mat_ref[i,ix_ref]>=threshold1) 
    }
    if (length(yes_val)==0) {no_val<-ix_val} else {no_val<-ix_val[-yes_val]}
    if (length(yes_ref)==0) {no_ref<-ix_ref} else {no_ref<-ix_ref[-yes_ref]}
    # kind of ad-hoc "table" Rcommand
    if (lab=="a") { # hit
      score<-length(which(yes_val %in% yes_ref))
    } else if (lab=="b") { # false allarm
      score<-length(which(yes_val %in% no_ref))
    } else if (lab=="c") { # miss
      score<-length(which(no_val %in% yes_ref))
    } else if (lab=="d") { # correct rejection
      score<-length(which(no_val %in% no_ref))
    } else if (lab=="ets") {
      a<-length(which(yes_val %in% yes_ref))
      b<-length(which(yes_val %in% no_ref))
      c<-length(which(no_val %in% yes_ref))
      d<-length(which(no_val %in% no_ref))
      a_random<-(a+c)*(a+b)/(a+b+c+d)
      score<-(a-a_random)/(a+c+b-a_random)
    }
  # use the reference dataset. temporal allignment mat and mat_ref
  } else {
    # no thresholding, then use all not-NA pairs 
    if (is.na(threshold) | lab=="seeps") {
      ix<-which( !is.na(mat[i,]) & !is.na(mat_ref[i,]) )
    # thresholding is done both on the reference and evaluated values
    } else if (lab=="roblinreg") {
      if (is.na(type)) return(NA)
      if (type=="below") {
        ix<-which(mat_ref[i,]<threshold & mat[i,]<threshold)
      } else if (type=="below=") {
        ix<-which(mat_ref[i,]<=threshold & mat[i,]<=threshold)
      } else if (type=="=within") {
        ix<-which(mat_ref[i,]>=threshold & mat_ref[i,]<threshold1 &
                      mat[i,]>=threshold &     mat[i,]<threshold1) 
      } else if (type=="within") {
        ix<-which(mat_ref[i,]>threshold & mat_ref[i,]<threshold1 & 
                      mat[i,]>threshold &     mat[i,]<threshold1 ) 
      } else if (type=="within=") {
        ix<-which(mat_ref[i,]>threshold & mat_ref[i,]<=threshold1 & 
                      mat[i,]>threshold &     mat[i,]<=threshold1) 
      } else if (type=="=within=") {
        ix<-which(mat_ref[i,]>=threshold & mat_ref[i,]<=threshold1 & 
                      mat[i,]>=threshold &     mat[i,]<=threshold1) 
      } else if (type=="above") {
        ix<-which(mat_ref[i,]>threshold & mat[i,]>threshold) 
      } else if (type=="above=") {
        ix<-which(mat_ref[i,]>=threshold & mat[i,]>=threshold) 
      }
    # thresholding is done on the reference values, then use all not-NA pairs
    } else {
      if (is.na(type)) return(NA)
      if (type=="below") {
        ix<-which(mat_ref[i,]<threshold & !is.na(mat[i,]))
      } else if (type=="below=") {
        ix<-which(mat_ref[i,]<=threshold & !is.na(mat[i,]))
      } else if (type=="=within") {
        ix<-which(mat_ref[i,]>=threshold & mat_ref[i,]<threshold1 & !is.na(mat[i,])) 
      } else if (type=="within") {
        ix<-which(mat_ref[i,]>threshold & mat_ref[i,]<threshold1 & !is.na(mat[i,])) 
      } else if (type=="within=") {
        ix<-which(mat_ref[i,]>threshold & mat_ref[i,]<=threshold1 & !is.na(mat[i,])) 
      } else if (type=="=within=") {
        ix<-which(mat_ref[i,]>=threshold & mat_ref[i,]<=threshold1 & !is.na(mat[i,])) 
      } else if (type=="above") {
        ix<-which(mat_ref[i,]>threshold & !is.na(mat[i,])) 
      } else if (type=="above=") {
        ix<-which(mat_ref[i,]>=threshold & !is.na(mat[i,])) 
      }
    }
    if (n<-length(ix)==0) return(NA)
    if (lab=="msess") {
      score<- 1 - mean( (    mat[i,ix] -       mat_ref[i,ix])**2) /
                  mean( (mat_ref[i,ix] - mean(mat_ref[i,ix]))**2)
    } else if (lab=="corr") {
      score<-cor(mat[i,ix],mat_ref[i,ix],method=ifelse(is.na(type),"pearson",type))
    } else if (lab=="bias") {
      score<- mean(    (mat[i,ix] - mat_ref[i,ix]))
    } else if (lab=="mae") {
      score<- mean( abs(mat[i,ix] - mat_ref[i,ix]))
    } else if (lab=="mbias") {
      score<- mean(    (mat[i,ix] / mat_ref[i,ix]))
    } else if (lab=="rmse") {
      score<- mean(    (mat[i,ix] - mat_ref[i,ix])**2)
    } else if (lab=="rmsf") {
      score<- mean(    (mat[i,ix] / mat_ref[i,ix])**2)
    } else if (lab=="roblinreg") { # robust linear regression
      bxp<-boxplot(mat[i,ix]~round(mat_ref[i,ix],0),plot=F)
      # "stats" "n"     "conf"  "out"   "group" "names"
      iy<-which(bxp$n>10)
      if (length(iy<-which(bxp$n>10))<3) {
        score<-NA
      } else {
        score<-as.numeric(lm(bxp$stats[3,iy]~as.numeric(bxp$names[iy])+0)$coefficients[1])
      }
    } else if (lab=="seeps") {
      if ( is.na(threshold))  threshold<-0.25 # mm
      if (is.na(threshold1)) threshold1<-threshold
      if (is.na(type)) type<-"error" # "skillscore"
      prob_light_to_heavy<-2 # light is twice as much likely than heavy
      # prec yes/no
      v_dry<- mat_ref[i,ix] < threshold1
      # define probabilities (1=dry, 2=light, 3=heavy)
      # based on p3=p2/2 (and p1+p2+p3=1)
      v_p<-vector(mode="numeric",length=2)
      v_p[1]<- length(which(v_dry)) / n
      # check if SEEPS is applicable
      if ( v_p[1]<0.1 | v_p[1]>0.85 ) return(NA)
      v_p[2]<- prob_light_to_heavy/(prob_light_to_heavy+1) * (1-v_p[1])
      # define thresholds (quantiles), v_p[1]+v_p[2] == 1-1/(prob_light_to_heavy+1)
      v_q<-vector(mode="numeric",length=2)
      v_q<- c( threshold1, 
               as.numeric(quantile( mat_ref[i,ix], 
                          probs=(v_p[1]+v_p[2]),type=4)) )
      if (type=="light_rain_threshold") {
        score<-v_q[2]
      } else {
        # Scoring Matrix (from ECMWF memo, Eq.(4))
        # observed categories dry, light, heavy from left to right
        # forecast categories dry, light, heavy from the top down
        scoring_matrix<- 0.5 * cbind( c(            0,     1/v_p[1], 1/v_p[1]+3/(2+v_p[1]) ), 
                                      c( 1/(1-v_p[1]),            0,          3/(2+v_p[1]) ), 
                                      c( 4/(1-v_p[1]), 3/(1-v_p[1]),                     0 ) )
        # vf/class 11/11, 12/10, 13/13, 21/01, 22/00, 23/03, 31/31, 32/30, 33/33
        class<- 10 * as.numeric(mat_ref[i,ix]<v_q[1]) + 30 * as.numeric(mat_ref[i,ix]>v_q[2]) +
                 1 * as.numeric(    mat[i,ix]<v_q[1]) +  3 * as.numeric(    mat[i,ix]>v_q[2])
        contt<- 1/n * cbind( c(length(which(class==11)),length(which(class==10)),length(which(class==13)) ),
                             c(length(which(class== 1)),length(which(class== 0)),length(which(class== 3)) ),
                             c(length(which(class==31)),length(which(class==30)),length(which(class==33)) ) )
        score<-sum(contt*scoring_matrix)
        if (type=="skillscore") score<-1-score
      }
    } else {
      score<-NA
    }
  } 
  score
}

