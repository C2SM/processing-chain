import os 


def main(cfg):
    to_write_AF = """&AFCTL
   hinc_e=1.0, !time increment for read of emissions
   hinc_f=1.0, !time increment for read of fluxes
/END
"""

    to_write_ORG = f"""&LMGRID
! Berlin full domain
  ie_tot        = 70,
  je_tot        = 60,
  ke_tot        = 60,
  pollon        = -170,  ! origin lon: 10.0
  pollat        =  43,   ! origin lat: 47.0
  dlon          = 0.1,
  dlat          = 0.1,
  startlat_tot  =  2.5,
  startlon_tot  = -1.4,
/ 
&RUNCTL
  dt            = 10.0,
  hstart        = {cfg.hstart},
  hstop         = {cfg.hstop},   
  ydate_ini     = '{cfg.inidate_yyyymmddhh}',
  nprocx        = {cfg.np_x},
  nprocy        = {cfg.np_y},
  nprocio       = 0,
  hincmxt       = 1.0,
  lphys         = .TRUE.,
  luse_rttov    = .FALSE.,
  luseobs       = .FALSE.,
  leps          = .FALSE.,
  lreorder      = .FALSE.,
  lreproduce    = .TRUE.,
  itype_timing  = 1,
  ldiagnos      = .TRUE.,
  ldfi          = .FALSE.,
  ldump_ascii   = .FALSE.,
  ldatatypes    = .FALSE.,
  ltime_barrier = .FALSE.,
  ncomm_type    = 1,        ! (OF: we use 1 operationally since 2 was causing trouble with memory consumption, leak in the MPI library)
  num_asynio_comm = {cfg.np_io},
  num_iope_percomm = 0,
  idbg_level    = 2,
  nboundlines   = 3,
  l2dim         = .FALSE.,
/
# Runge-Kutta time integration settings
&TUNING
  wichfakt =   0.0,
  z0m_dia  =   0.2,
  rat_lam  =   1.0,
  rat_can  =   1.0,
  tur_len  = 150.0,     ! 150 is used for MeteoSwiss COSMO-1 production run
  c_soil   =   1.0,
  clc_diag =  0.50,     ! 0.5 is used for MeteoSwiss COSMO-1 production run
  q_crit   =   1.6,     ! 1.6 is used for MeteoSwiss COSMO-1 production run
  mu_rain  =   0.5,     ! 0.5 is used for MeteoSwiss COSMO-1 production run
  rain_n0_factor = 1.0, ! 1.0 is used for MeteoSwiss COSMO-1 production run
  crsmin   = 150.0,
  qc0      =   0.0002,
  qi0      =   0.0,
  v0snow   =  20.0,     ! 20.0 is used for MeteoSwiss COSMO-1 production run
/END
"""

    to_write_IO =f"""&IOCTL
  lasync_io      = .FALSE., 
  lbdclim        = .TRUE.,
  ngribout       = 1,         ! number of output files (defines # GRIBOUT sections)
  yform_read     = "ncdf", 
  nhour_restart  = {int(cfg.hstart + cfg.restart_step)},{int(cfg.hstop)},{int(cfg.restart_step)},  ! start, stop and increment in full forecast hours
  ydir_restart_in  = '{cfg.cosmo_restart_in}',
  ydir_restart_out = '{cfg.cosmo_restart_out}',
  ytunit_restart = 'f',
  ldwd_grib_use  = .FALSE.,
  l_ke_in_gds    = .TRUE.,
  ymode_read     = 'r  ',
  ymode_write    = 'w b',
  nincwait       = 90,
  nmaxwait       = 300,
  nvers          = 917,
  ncenter        = 215,   ! MeteoSwiss
 /
 &DATABASE
 /
 &GRIBIN
   lbdana        = .FALSE.,
   ydirini       = '../../int2lm/output/',
   ytunitbd      = 'd',                 ! or 'f' ?
   lchkini       = .TRUE.,
   hincbound     = 1.0,
   ydirbd        = '../../int2lm/output/',
   lchkbd        = .TRUE.,
   lana_qi       = .TRUE.,
   llb_qi        = .TRUE.,
   lana_qg       = .FALSE.,
   llb_qg        = .FALSE.,
   lana_qr_qs    = .TRUE.,
   llb_qr_qs     = .TRUE.,
   lana_rho_snow = .FALSE.,
   lan_lai       = .TRUE.,
   lan_rootdp    = .TRUE.,
   lan_vio3      = .TRUE.,
   lan_plcov     = .TRUE.,
   lan_t_cl      = .TRUE.,
   lan_w_cl      = .TRUE.,
   lan_hmo3      = .TRUE.,
   lan_t_so0     = .TRUE.,
   lan_t_snow    = .TRUE.,
   lan_w_snow    = .TRUE.,
   lan_w_i       = .TRUE.,
   lan_rho_snow  = .TRUE.,
   newbc         = 0,
   hnewbcdt      = 3.0,
 /
&GRIBOUT
  ytunit      = 'd',
  lanalysis   = .FALSE.,
  yform_write ='ncdf', 
  hcomb       = 0,8761,1,
  yvarml = 'U', 'V', 'W',             ! wind vector
           'T', 'P', 'PS',            ! temperature and (surface) pressure
           'QV','QC','QI',            ! specific humidity, specific cloud liquid and ice content
           'QR','QS',                 ! (for Arjo)
           'CLCT', 'CLC',             ! total cloud cover and layer cloud area fraction
           'CLWC', 'CIWC'             ! Cloud liquid and Ice water content
           'U_10M', 'V_10M',          ! 10m winds
           'T_2M', 'TD_2M',           ! 2m temperatures
           'RAIN_GSP', 'SNOW_GSP',    ! large scale rainfall amount
           'RAIN_CON', 'SNOW_CON',    ! convective rainfail amount (should be zero)
           'Z0',                      ! surface roughness length
           'T_SO',                    ! soil temperature
           'T_S',                    ! Surface temperature (soil ?)
           'T_G',                    ! Surface temperature
           'W_SO',                    ! water content of (multi-layer) soil levels
           'RELHUM_2M',                    ! Relative Humidity at 2 meter
           'W_SNOW',                              ! thickness of surface snow amount
           'ALHFL_S','ASHFL_S',                   ! latent and sensible heat flux
           'ASOB_S','ATHB_S','APAB_S','ALB_RAD',  ! net shortwave, net longwave, downward photosynthetic active, albedo
           'AUMFL_S','AVMFL_S',                   ! momentum flux, u and v components
           'HPBL',                                ! boundary layer height
           'ASWDIFU_S',                           ! short wave diffuse up
           'ATHD_S',                              ! long wave down
           'ASWDIR_S',                            ! short wave direct down
           'ASWDIFD_S',                           ! short wave diffuse down
           'TKVH',                                ! atmosphere heat diffusivity (for Arjo)
           'HP',                                  ! air pressure at layer interfaces (for Arjo)
           'CO2_BG','CO2_GPP','CO2_RA','CO2_TOT','CO2_SURF','CO2_A','CO2_BV','CO2_JV','CO_TOT','NOX_TOT',
  l_z_filter       = .FALSE.,
  l_p_filter       = .FALSE.,
  luvmasspoint     = .FALSE.,
  l_fi_pmsl_smooth = .FALSE.,
  lcheck           = .TRUE.,
  ydir             = '../output/',
/
"""
    #  ideflate_level   = 2,


    to_write_DYN = f"""&DYNCTL
 lcpp_dycore      = .TRUE.,
 l2tls            = .TRUE.,
 lcond            = .TRUE.,
 irk_order        = 3,
 y_scalar_advect  = 'BOTT2_STRANG', ! MeteoSwiss COSMO-1
 itype_fast_waves = 2,
 divdamp_slope    = 1.0,
 itype_bbc_w      = 114,
 ldyn_bbc         = .FALSE.,
 l_diff_Smag      = .TRUE.,
 itype_hdiff      = 2,
 hd_corr_u_bd     = 0.75,  ! MeteoSwiss COSMO-1
 hd_corr_t_bd     = 0.75,  ! MeteoSwiss COSMO-1
 hd_corr_p_bd     = 0.75,  ! MeteoSwiss COSMO-1
 hd_corr_trcr_bd  = 0.0,
 hd_corr_p_in     = 0.0,
 hd_corr_u_in     = 0.0,   ! no horizontal diffusion (MeteoSwiss COSMO-1)
 hd_corr_trcr_in  = 0.0,
 hd_corr_t_in     = 0.0,
 hd_dhmax         = 250.,
 ldiabf_lh        = .TRUE.,
 lexpl_lbc        = .TRUE.,
 lspubc           = .TRUE.,
 nrdtau           = 3,
 xkd              = 0.1,
 rlwidth          = 35000.,  ! 35 km (MeteoSwiss COSMO-1)
 iadv_order       = 5,       ! MeteoSwiss COSMO-1
 itype_spubc      = 3,       ! only relaxation of vertical velocity (MeteoSwiss COSMO-1)
 itype_outflow_qrsg = 1,     ! MeteoSwiss does use 2 but maybe does not work for tracer)
                             ! type of relaxation treatment for qr, qs, qg
                             ! =1: (default) same treatment as all variables
                             ! =2: no relaxation for qr,qs,qg at outflow boundary points
 l_diff_cold_pools = .TRUE., ! MeteoSwiss COSMO-1
/
"""
    to_write_PHY = f"""&PHYCTL
  lgsp           = .TRUE.,
  itype_gscp     = 4,
  lrad           = .TRUE.,
  nradcoarse     = 1,
  lradf_avg      = .FALSE.,
  hincrad        = 0.1666667,      ! MeteoSwiss COSMO-1
  lforest        = .TRUE.,
  lradtopo       = .FALSE.,
  ltur           = .TRUE.,
  ninctura       = 1,
  itype_turb     = 3,   
  imode_turb     = 1,
  icldm_turb     = 2,
  l3dturb        = .FALSE.,
  lexpcor        = .FALSE., ! set to .FALSE. because not tested with GPU version
  ltmpcor        = .FALSE.,
  lprfcor        = .FALSE.,
  lnonloc        = .FALSE.,
  lcpfluc        = .FALSE.,
  itype_tran     = 2,
  imode_tran     = 1,              ! MeteoSwiss COSMO-1
  itype_wcld     = 2,
  icldm_tran     = 0,
  icldm_rad      = 4,
  itype_root     = 1,              ! MeteoSwiss COSMO-1
  itype_synd     = 2,
  limpltkediff   = .TRUE.,
  itype_albedo   = 1,              ! 3 = MeteoSwiss COSMO-1 (but needs ALB_DIF) ::: set to =1
  itype_heatcond = 2,
  itype_hydcond  = 1,              ! MeteoSwiss COSMO-1
  lsoil          = .TRUE.,   
  itype_trvg     = 2,  
  itype_evsl     = 2,              ! MeteoSwiss COSMO-1  
  lmulti_layer   = {cfg.multi_layer},
  lmelt          = .TRUE.,         ! MeteoSwiss COSMO-1
  lmelt_var      = .TRUE.,         ! MeteoSwiss COSMO-1
  lmulti_snow    = .FALSE.,
  ke_soil        = 7,  
  czml_soil      = 0.005, 0.02, 0.06, 0.18, 0.54, 1.62, 4.86, 14.58,
  lconv          = .FALSE., 
  itype_conv     = 3,       
  lcape          = .FALSE.,
  lsso           = .FALSE.,
  lseaice        = .FALSE.,
  nincconv       = 10,              ! MeteoSwiss COSMO-1
/
"""

    to_write_DIA = """&DIACTL
  n0meanval      = 0,
  nincmeanval    = 1,
  itype_diag_t2m = 1,
  lgplong        = .TRUE., 
  lgpshort       = .FALSE.,
  lgpspec        = .FALSE.,
  n0gp           = 0,
  hincgp         = 1.0,
  l_integrals    = .FALSE.,
/END
"""

    to_write_ASS="""&NUDGING
  lnudge = .FALSE.,
/END
"""

    for section in ["AF","ORG","IO","DYN","PHY","DIA","ASS"]:
        output_file = os.path.join(cfg.cosmo_work,"INPUT_"+section)
        with open(output_file,"w") as of:
            to_write = eval("to_write_"+section)
            of.write(to_write)

