#+
spider_mastervalues_mask <- function() {
#------------------------------------------------------------------------------

  if (length(argv$mastervalues_mask_vals1)==0) return(rmaster)

  # mask out based on values
  val <- getValues(rmaster)
  for (i in 1:length(argv$mastervalues_mask_cond)) {
    if (argv$mastervalues_mask_cond[i] == "below") {
      val[which(val<argv$mastervalues_mask_vals1[i])] <- argv$mastervalues_mask_pad[i]
    } else if (argv$mastervalues_mask_cond[i] == "below=") {
      val[which(val<=argv$mastervalues_mask_vals1[i])] <- argv$mastervalues_mask_pad[i]
    } else if (argv$mastervalues_mask_cond[i] == "=within") {
      val[which(val>=argv$mastervalues_mask_vals1[i] & val<argv$mastervalues_mask_vals2[i])] <- argv$mastervalues_mask_pad[i]
    } else if (argv$mastervalues_mask_cond[i] == "within") {
      val[which(val>argv$mastervalues_mask_vals1[i] & val<argv$mastervalues_mask_vals2[i])] <- argv$mastervalues_mask_pad[i]
    } else if (argv$mastervalues_mask_cond[i] == "within=") {
      val[which(val>argv$mastervalues_mask_vals1[i] & val<=argv$mastervalues_mask_vals2[i])] <- argv$mastervalues_mask_pad[i]
    } else if (argv$mastervalues_mask_cond[i] == "=within=") {
      val[which(val>=argv$mastervalues_mask_vals1[i] & val<=argv$mastervalues_mask_vals2[i])] <- argv$mastervalues_mask_pad[i]
    } else if (argv$mastervalues_mask_cond[i] == "above") {
      val[which(val>argv$mastervalues_mask_vals1[i])] <- argv$mastervalues_mask_pad[i]
    } else if (argv$mastervalues_mask_cond[i] == "above=") {
      val[which(val>=argv$mastervalues_mask_vals1[i])] <- argv$mastervalues_mask_pad[i]
    }
  }
  rmaster[] <- val

  #
  if ( !any( !is.na( values <- getValues(rmaster)))) {
    print(paste("warning: all NAs after value mask"))
    return( NULL)
  }

  #
  rmaster
}
