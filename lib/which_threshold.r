#+ select only certain vector elements based on thresholds
which_threshold<-function(x,threshold,threshold1,type) {
  if (is.na(type)) {
    ix<-1:length(x)
  } else if (type=="below") {
    ix<-which(x<threshold)
  } else if (type=="below=") {
    ix<-which(x<=threshold)
  } else if (type=="=within") {
    ix<-which(x>=threshold & x<threshold1) 
  } else if (type=="within") {
    ix<-which(x>threshold & x<threshold1) 
  } else if (type=="within=") {
    ix<-which(x>threshold & x<=threshold1) 
  } else if (type=="=within=") {
    ix<-which(x>=threshold & x<=threshold1) 
  } else if (type=="above") {
    ix<-which(x>threshold) 
  } else if (type=="above=") {
    ix<-which(x>=threshold) 
  }
  ix
}

