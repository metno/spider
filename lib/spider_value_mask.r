spider_value_mask <- function() {
  for (i in 1:length(argv$value_mask)) {
    if (argv$value_mask_condition[i]=="below") {
      r[r<argv$value_mask[i]]<-NA
    } else if (argv$value_mask_condition[i]=="below=") {
      r[r<=argv$value_mask[i]]<-NA
#    } else if (argv$value_mask_condition[i]=="=within") {
#    } else if (argv$value_mask_condition[i]=="within") {
#    } else if (argv$value_mask_condition[i]=="within=") {
#    } else if (argv$value_mask_condition[i]=="=within=") {
    } else if (argv$value_mask_condition[i]=="above") {
      r[r>argv$value_mask[i]]<-NA
    } else if (argv$value_mask_condition[i]=="above=") {
      r[r>=argv$value_mask[i]]<-NA
    }
  }
  r
}

