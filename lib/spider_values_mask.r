#+
spider_values_mask <- function() {
#------------------------------------------------------------------------------

  if (length(argv$values_mask_vals1)==0) return(r)

  # mask out based on values
  val <- getValues(r)
  for (i in 1:length(argv$values_mask_cond)) {
    if (argv$values_mask_cond[i] == "below") {
      val[which(val<argv$values_mask_vals1[i])] <- argv$values_mask_pad[i]
    } else if (argv$values_mask_cond[i] == "below=") {
      val[which(val<=argv$values_mask_vals1[i])] <- argv$values_mask_pad[i]
    } else if (argv$values_mask_cond[i] == "=within") {
      val[which(val>=argv$values_mask_vals1[i] & val<argv$values_mask_vals2[i])] <- argv$values_mask_pad[i]
    } else if (argv$values_mask_cond[i] == "within") {
      val[which(val>argv$values_mask_vals1[i] & val<argv$values_mask_vals2[i])] <- argv$values_mask_pad[i]
    } else if (argv$values_mask_cond[i] == "within=") {
      val[which(val>argv$values_mask_vals1[i] & val<=argv$values_mask_vals2[i])] <- argv$values_mask_pad[i]
    } else if (argv$values_mask_cond[i] == "=within=") {
      val[which(val>=argv$values_mask_vals1[i] & val<=argv$values_mask_vals2[i])] <- argv$values_mask_pad[i]
    } else if (argv$values_mask_cond[i] == "above") {
      val[which(val>argv$values_mask_vals1[i])] <- argv$values_mask_pad[i]
    } else if (argv$values_mask_cond[i] == "above=") {
      val[which(val>=argv$values_mask_vals1[i])] <- argv$values_mask_pad[i]
    }
  }
  r[] <- val

  #
  if ( !any( !is.na( values <- getValues(r)))) {
    print(paste("warning: all NAs after value mask"))
    return( NULL)
  }

  #
  r
}
