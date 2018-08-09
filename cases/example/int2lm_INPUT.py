import os 

def main(cfg):
    output_file = os.path.join(cfg.int2lm_work,"INPUT")
    with open(output_file,"w") as of:
        to_write = f"""&CONTRL
    yinput_model = 'COSMO', ! string to identify the input model
    ydate_ini = '{cfg.inidate_int2lm_yyyymmddhh}',   ! start of the forecast

    hstart = {cfg.hstart_int2lm},      ! first hour to be processed
    hstop = {cfg.hstop_int2lm},        ! last hour to be processed
    hincbound = 1.0,     ! increment in hours for the processing

    nprocx = {cfg.np_x},         ! number of processors in east-west direction
    nprocy = {cfg.np_y},          ! number of processors in north-south direction
    nprocio = 0,         ! number of extra processors for asynchronous IO
    nincwait = 30,       ! seconds to wait until next attempt if a ready file is
                         ! not available
    nmaxwait = 300,      ! maximum seconds to wait until abort if a ready file is
                         ! not available



    norder_filter = 5,   ! order of the orography filtering
    ilow_pass_oro = 4,   ! type of low-pass filter for orography
    numfilt_oro = 1,     ! number of sequential applications of filter
    ilow_pass_xso = 5,   ! type of low-pass filter for extra smoothing steep oro.
    numfilt_xso = 1,     ! number of sequential applications of xso-filter
    itype_w_so_rel = 1,  ! type of relative soil moisture input (0,1,2)
    idbg_level = 2,      ! to control verbosity of debug output
    itype_balance_pp = 2,     ! To choose the previous (=1) or new (=2) method for
                              ! hydrostatic pressure calculation in case of
                              ! non-hydrostatic input models

    eps_filter = 0.1,    ! parameter for orography filtering
    rxso_mask = 750.0,   ! mask for extra smoothing of steep oro.: dh > rxso_mask
    rfill_valley = 0.0,  ! mask for valley filling: dh > rfill_valley

    luse_t_skin = .TRUE.,   ! if .TRUE., use ECMWF skin temperature for surface
    lpost_0006 = .TRUE.,    ! if .TRUE., force to use ECMWF dataset after 27 June 2000
    luvcor = .TRUE.,        ! if .TRUE., correct winds for given surface pres. tendency
    lvertwind_ini = .TRUE., ! if .TRUE., compute vertical wind for LM for initial data
    lvertwind_bd = .FALSE., ! if .TRUE., compute vertical wind for LM for boundary data
                            ! (MeteoSwiss uses .FALSE.)
    lprog_qi = .TRUE.,      ! if .TRUE., interpolate qi to LM grid
    lprog_qr_qs = .TRUE.,   ! if .TRUE., interpolate qr,qs to LM grid
    lprog_qg = .FALSE.,     ! if .TRUE., interpolate qg to LM grid
    lprog_rho_snow = .TRUE.,  ! if .TRUE., interpolate rho_snow to LM grid
                              ! for ICON input
                              ! (MeteoSwiss uses .TRUE.)
    linitial = .TRUE.,      ! if .TRUE., initial data for LM
    lboundaries = .TRUE.,   ! if .TRUE., lateral boundaries for LM
    ltime_mean = .TRUE.,    ! if .TRUE., mean values of the timings are printed
    lmulti_layer_lm = {cfg.multi_layer}, ! compute data for multi-layer soil model
    lmulti_layer_in = {cfg.multi_layer}, ! data from multi-layer soil model in the incoming data
    lfilter_oro = .TRUE.,   ! if .TRUE., filter the orography
    lxso_first = .FALSE.,   ! if .TRUE., do extra smoothing of orography first
    lfilter_pp = .TRUE.,    ! if .TRUE., filter the pressure deviation after vertical
                            !            vertical interpolation in LM2LM
    lbdclim = .TRUE.,       ! if .TRUE., special boundary data for climate mode
    lforest = .TRUE.,       ! if .TRUE., run with forest (evergreen and deciduous)
    lsso = .FALSE.,         ! process parameters for sso scheme
    lradtopo = .FALSE.,     ! process parameters for topographic correction of radiation
    llbc_smooth = .TRUE.,   ! if .TRUE., run with smooth orography transition to LB
    l_art = .TRUE.,         ! switch for additional cosmo-art fields
    l_art_nested = .FALSE., ! switch for cosmo-art2cosmo-art
    l_art_file = .TRUE.,    ! if .TRUE., art namelist is read from INPUT_ART
    l_smi = .FALSE.,        ! if .TRUE., interpolate soil moisture with SMI
                            ! (MeteoSwiss uses .FALSE.)
    lmixcld = .FALSE.,      ! if .TRUE., qi added in grh instead of being directly
                            !            interp.
                            ! (MeteoSwiss uses .FALSE.)
    lasync_io = .FALSE.,    ! if .TRUE.: the model runs with extra PEs for asynchr. IO
    l_topo_z = .FALSE.,     ! if .TRUE., additional smoothing of the topography
                            ! for the LM-Z coordinate Version
/

&GRID_IN
! COSMO-7 grid
  ie_in_tot = 393,           ! ie (lon) for input grid (total domain) 	
  je_in_tot = 338,           ! je (lat) for input grid (total domain) 	
  ke_in_tot = 60,            ! ke (levels) for input grid (total domain) 	
  pcontrol_fi=30000.,        ! Pressure of control level for geopotential
  pollat_in = 43.0,
  pollon_in = -170.0,
  dlat_in = 0.06,
  dlon_in = 0.06,
  startlat_in_tot = -9.78,
  startlon_in_tot = -16.32,
 /

&LMGRID
! Berlin grid with 0.1 degree
  ielm_tot = 70,
  jelm_tot = 60,
  kelm_tot = 60,
  ivctype = 4,                ! 4 used by MeteoSwiss COSMO-7 -> 1 processing
  irefatm = 2,                ! used by MeteoSwiss
  lanalyt_calc_t0p0 = .TRUE., ! used by MeteoSwiss
  vcflat = 11357.0,
  vcoord_d = 23588.50,22395.93,21304.04,   20307.39,   19399.95,
     18574.03,   17821.88,   17135.64,   16507.79,   15930.60,
     15396.52,   14897.86,   14427.98,   13981.10,   13551.52,
     13133.53,   12721.37,   12312.04,   11900.03,   11485.37,
     11068.19,   10648.54,   10226.48,    9802.09,    9375.43,
      8946.58,    8515.59,    8082.55,    7647.52,    7210.55,
      6771.96,    6332.38,    5896.41,    5468.04,    5050.84,
      4647.96,    4261.91,    3893.26,    3542.15,    3208.52,
      2892.23,    2593.71,    2312.95,    2049.75,    1803.89,
      1575.57,    1364.68,    1170.90,     993.84,     833.44,
       689.53,     561.52,     448.82,     350.95,     267.55,
       197.67,     137.23,      87.33,      48.44,      20.00,       0.00,
!  vcflat = 2000.0,
!  vcoord_d = 23588.50,    5896.41,    5468.04,    5050.84,
!      4647.96,    4261.91,    3893.26,    3542.15,    3208.52,
!      2892.23,    2593.71,    2312.95,    2049.75,    1803.89,
!      1575.57,    1364.68,    1170.90,     993.84,     833.44,
!       689.53,     561.52,     448.82,     350.95,     267.55,
!       197.67,     137.23,      87.33,      48.44,      20.00,       0.00,
  pollat = 43.0,
  pollon = -170.0,
  dlon = 0.1,
  dlat = 0.1,
  startlat_tot =  2.5,
  startlon_tot = -1.4,
/ 

&DATABASE
/

&DATA
  ! General control variables:
  ncenter = 215,           ! Originating center identification
  nprocess_ini = 131,      ! Generating process identification for initial values
  nprocess_bd = 132,       ! Generating process identification for boundary values
  ymode_write = 'w b',     ! Specify open mode for writing
  ytunit_in = 'd',         ! Time unit for input data
  ytunit_out = 'd',        ! Time unit for output data
  yinput_type = 'analysis',! Type of input data:
  !                        ! 'forecast' forecast data
  !                        ! 'analysis' analysis data
  !                        ! 'ana_init' initialized analysis data
  ! ---------------------------------------------------------------- !
  ! Variables for external data:
  ylmext_lfn='{cfg.int2lm_extpar_file}',
  ylmext_cat='../input/extpar/',
  ylmext_form_read='ncdf', ! Input format of external data
  yinext_lfn='laf{cfg.inidate_int2lm_yyyymmddhh}', ! Name of the file with the external fields
  yinext_cat='../input/meteo/',
  yinext_form_read="grb1", ! Input format of external data from coarse grid
  ie_ext=600,
  je_ext=500,
  ! 
  ! ---------------------------------------------------------------- !
  ! Variables for the models:
  yin_cat='../input/meteo/',
  yin_form_read='grb1',
  ylm_cat='../output/',
  ylm_form_write = "nc-4", ! Output format of COSMO-Model data
/

&PRICTR
  igp_tot = 36, 40, 48, 44, 48, 85, 77,
  jgp_tot = 30, 94, 38, 26, 26, 96, 12,
  lchkin = .TRUE.,
  lchkout = .TRUE.,
  lprgp = .FALSE.,
/
"""
        of.write(to_write)



