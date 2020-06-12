#+
read_griddeddata<-function( mode="data",
                            var=NA,
                            argv=NA,
                            ffin=NA,
                            t_to_read=NA,
                            ffin_ref=NA) { #data,master,data_dem,master_dem
#------------------------------------------------------------------------------
  if ( is.na(argv))
    if ("argv" %in% ls(envir = .GlobalEnv)) 
      argv <- get("argv", envir = .GlobalEnv)
  #
  if (mode=="data") {
    ff           <- ffin
    ff_tpos      <- argv$ffin_tpos
    ff_epos      <- argv$ffin_epos
    ff_e         <- argv$ffin_e
    ff_varname   <- ifelse( is.na(var), argv$ffin_varname, var)
    ff_topdown   <- argv$ffin_topdown
    ff_ndim      <- argv$ffin_ndim
    ff_dimnames  <- argv$ffin_dimnames
    ff_proj4     <- argv$ffin_proj4
    ff_proj4_var <- argv$ffin_proj4_var
    ff_proj4_att <- argv$ffin_proj4_att
  } else if (mode=="ref") {
    ff           <- ffin_ref
    ff_tpos      <- argv$ffin_ref_tpos
    ff_epos      <- argv$ffin_ref_epos
    ff_e         <- argv$ffin_ref_e
    ff_varname   <- ifelse( is.na(var), argv$ffin_ref_varname, var)
    ff_topdown   <- argv$ffin_ref_topdown
    ff_ndim      <- argv$ffin_ref_ndim
    ff_dimnames  <- argv$ffin_ref_dimnames
    ff_proj4     <- argv$ffin_ref_proj4
    ff_proj4_var <- argv$ffin_ref_proj4_var
    ff_proj4_att <- argv$ffin_ref_proj4_att
  } else if (mode=="data_dem") {
    ff           <- argv$ffindem
    ff_tpos      <- argv$ffindem_tpos
    ff_epos      <- argv$ffindem_epos
    ff_e         <- argv$ffindem_e
    ff_varname   <- ifelse( is.na(var), argv$ffindem_varname, var)
    ff_topdown   <- argv$ffindem_topdown
    ff_ndim      <- argv$ffindem_ndim
    ff_dimnames  <- argv$ffindem_dimnames
    ff_proj4     <- argv$ffin_proj4
    ff_proj4_var <- argv$ffin_proj4_var
    ff_proj4_att <- argv$ffin_proj4_att
  } else if (mode=="master") {
    ff           <- argv$ffmaster
    ff_tpos      <- argv$ffmaster_tpos
    ff_epos      <- argv$ffmaster_epos
    ff_e         <- argv$ffmaster_e
    ff_varname   <- ifelse( is.na(var), argv$ffmaster_varname, var)
    ff_topdown   <- argv$ffmaster_topdown
    ff_ndim      <- argv$ffmaster_ndim
    ff_dimnames  <- argv$ffmaster_dimnames
    ff_proj4     <- argv$ffmaster_proj4
    ff_proj4_var <- argv$ffmaster_proj4_var
    ff_proj4_att <- argv$ffmaster_proj4_att
  } else if (mode=="master_dem") {
    if (is.na(argv$ffmasterdem)) {
      ff         <- argv$ffmaster
      ff_tpos    <- argv$ffmaster_tpos
      ff_epos    <- argv$ffmaster_epos
      ff_e       <- argv$ffmaster_e
      ff_varname <- ifelse( is.na(argv$ffmasterdem_varname),
                     argv$ffmaster_varname, argv$ffmasterdem_varname)
      ff_topdown <- argv$ffmaster_topdown
      if ( is.na( argv$ffmasterdem_ndim)) {
        ff_ndim     <- argv$ffmaster_ndim
        ff_dimnames <- argv$ffmaster_dimnames
      } else {
        ff_ndim     <- argv$ffmasterdem_ndim
        ff_dimnames <- argv$ffmasterdem_dimnames
      }
    } else {
      ff<-argv$ffmasterdem
      ff_tpos     <- argv$ffmasterdem_tpos
      ff_epos     <- argv$ffmasterdem_epos
      ff_e        <- argv$ffmasterdem_e
      ff_varname  <- ifelse( is.na(argv$ffmasterdem_varname),
               argv$ffmaster_varname, argv$ffmasterdem_varname)
      ff_topdown  <- argv$ffmasterdem_topdown
      ff_ndim     <- argv$ffmasterdem_ndim
      ff_dimnames <- argv$ffmasterdem_dimnames
    }
    ff_proj4     <- argv$ffmaster_proj4
    ff_proj4_var <- argv$ffmaster_proj4_var
    ff_proj4_att <- argv$ffmaster_proj4_att
  }
  # check if variable exists
  if (!is.null( attr(ff_varnames<-try(nc4.getVars(ff)),"class") )) {
    print(paste("warning: not able to read from file ",ff))
    return(NULL)
  }
  if (!(ff_varname %in% ff_varnames)) {
    print(paste("warning: file",ff," variable",ff_varname,"not present"))
    return(NULL)
  }
  # time dimension not present
  if (is.na(ff_tpos)) {
    ff_tpos<-NULL
    ff_t<-NULL
  # time dimension present
  } else {
    # read input file time steps
    if (!is.null( attr(tsteps_in<-try(nc4.getTime(ff,format="%Y%m%d%H%M%S")),"class") )) {
      print(paste("warning: not able to read time dimension from file ",ff))
      return(NULL)
    }
    # special case when the user ask to read the first timestep for each file
    if (argv$one_timestep_for_file) t_to_read<-nc4.getTime(ff,format="%Y%m%d%H%M%S")[1]
    # data, check time to read is available
    if (mode=="data" | mode=="ref") {
      ff_t<-format(t_to_read,format="%Y%m%d%H%M%S",tz="GMT")
      if (!(ff_t %in% tsteps_in)) {
        print(paste("warning: time step to read",ff_t,"not in input file",ff))
        print("available time steps are:")
        print(tsteps_in)
        return(NULL)
      }
    # master grid, set time to read as the first time step
    } else if (mode=="master" | mode=="data_dem" | mode=="master_dem") {
      ff_t<-tsteps_in[1]
    }
  }
  if (is.na(ff_epos)) ff_epos<-NULL
  if (is.na(ff_e)) ff_e<-NULL
  raux<-try(read_dotnc(nc.file=ff,
                       nc.varname=ff_varname,
                       topdown=ff_topdown,
                       out.dim=list(ndim=ff_ndim,
                                    tpos=ff_tpos,
                                    epos=ff_epos,
                                    names=ff_dimnames),
                       proj4=ff_proj4,
                       nc.proj4=list(var=ff_proj4_var,
                                     att=ff_proj4_att),
                       selection=list(t=ff_t,e=ff_e,t_format="%Y%m%d%H%M%S")))
  if (!is.null(raux)) raux<-raux$stack
  if (mode=="data" | mode=="ref") {
    if (!is.na(argv$correction_factor)) raux<-raux*argv$correction_factor
    if (!is.na(argv$offset)) raux<-raux+argv$offset
  } else if (mode=="data_dem") {
    if (!is.na(argv$dem_correction_factor)) raux<-raux*argv$dem_correction_factor
    if (!is.na(argv$dem_offset)) raux<-raux+argv$dem_offset
  }  
  raux
}

