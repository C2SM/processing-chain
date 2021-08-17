import os

# GENERAL SETTINGS =========================================================== 
user = os.environ['USER']
target = 'cosmo-ghg'
restart_step = 24 # hours
#subtarget = 'spinup'
#spinup = 6

compute_host = 'daint'
compute_queue = 'normal' # 'debug'
compute_account = 'em05' 
constraint = 'gpu' # 'mc'

if constraint == 'gpu':
    ntasks_per_node = 12
    mpich_cuda = ('export MPICH_RDMA_ENABLED_CUDA=1\n'
                  'export MPICH_G2G_PIPELINE=256\n'
                  'export CRAY_CUDA_MPS=1\n'
                  'export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/cray/nvidia/default/lib64'
                 ) 
elif constraint == 'mc':
    ntasks_per_node = 36
    mpich_cuda = ''

# case name = pathname in cases/
path = os.path.realpath(__file__)
casename = os.path.basename(os.path.dirname(path))

# Root directory of the sourcecode of the chain (where run_chain.py is)
chain_src_dir = os.getcwd()

# Root directory of the working space of the chain
work_root = os.environ['SCRATCH'] + "/processing_chain"

# Directory where executables are stored
exe_dir = "/store/empa/em05/executables"

# PRE-PROCESSING =============================================================
input_root = '/store/empa/em05/mjaehn/Carbosense/input'

# METEO ----------------------------------------------------------------------
meteo_dir = '/store/empa/em05' # will use '/store/mch/msopr/owm/COSMO-7' instead
meteo_prefix = "laf"
meteo_inc = 1

# ICBC -----------------------------------------------------------------------
input_root_cams = '/store/empa/em05/dbrunner/carbosense'
cams_dir_orig = os.path.join(input_root_cams, 'icbc') # Input directory
cams_dir_proc = os.path.join(input_root_cams, 'icbc', 'processed') # Output directory
year = 2020
if year >= 2020:
    cams_prefix1 = 'cams_h9sp'
    cams_prefix2 = 'sfc_h9sp'
elif year == 2019:
    cams_prefix1 = 'cams_gznv'
    cams_prefix2 = 'sfc_gznv'
else:
    cams_prefix1 = 'cams_ghqy'
    cams_prefix2 = 'sfc_ghqy'
cams_parameters = [
    {'suffix': 'cams_co2',
     'species': ['CO2','CO','CH4'],
     'inc': 3,
     'prefix1': cams_prefix1,
     'prefix2': cams_prefix2,
     'lev': 137,
     }
    ]

# OBS_NUDGING ---------------------------------------------------------------- 
obs_nudging_dir = '/store/empa/em05/obs_nudging_cosmo'
obs_nudging_prefixes = ['cdfin_amdar',
                        'cdfin_ship',
                        'cdfin_synop',
                        'cdfin_temp',
                        'cdfin_wprof']
obs_nudging_date_format = "-%Y%m%d000000"

# OAE ------------------------------------------------------------------------
# Online anthropogenic emissions
oae_dir = os.path.join(input_root, 'oae')
oae_gridded_emissions_nc = 'emis_merged_CO2_CO_CH4_X_1km.nc'
oae_vertical_profiles_nc = 'vertical_profiles.nc'
oae_hourofday_nc = 'hourofday.nc'
oae_hourofyear_nc = 'hourofyear.nc'
oae_dayofweek_nc = 'dayofweek.nc'
oae_monthofyear_nc = 'monthofyear.nc'

# ONLINE_VPRM ---------------------------------------------------------------- 
# MODIS and vegetation data for online VPRM
online_vprm_dir = os.path.join(input_root, 'online_vprm')
modis_filename = 'MODIS_sur_refl_COSMO_Carbosense_1km_20200401-20200930.nc'
vegetation_filename = 'VPRM_VegClasses_COSMO1_Carbosense.nc' 

# INT2LM ---------------------------------------------------------------------
# Extpar-file
int2lm_extpar_dir = os.path.join(input_root, 'extpar')
int2lm_extpar_file = 'extpar_empa_cosmo1_aster_opt.nc'

# Executable
int2lm_bin = os.path.join(exe_dir, "int2lm_gnu_208d68e_20201005")

# Namelist and slurm runscript templates
int2lm_namelist = '%s/cases/%s/int2lm_INPUT.cfg' % (chain_src_dir,casename)
int2lm_runjob = '%s/cases/%s/int2lm_runjob.cfg' % (chain_src_dir,casename)

# Walltimes
if compute_queue=="normal":
    int2lm_walltime="10:00:00"
elif compute_queue=="debug":
    int2lm_walltime="00:30:00"
else: 
    logging.error("Unset queue name: %s" % compute_queue)
    sys.exit(1)

# Domain decomposition
int2lm_nodes = 2
int2lm_ntasks_per_node = 12 
int2lm_np_x = 6
int2lm_np_y = 4
int2lm_np_tot = int2lm_np_x * int2lm_np_y

# POST_INT2LM ----------------------------------------------------------------
# Fields that are used as initial conditions
post_int2lm_species = ['CO2_BG', 'CO_BG', 'CH4_BG']


# SIMULATION =================================================================
# COSMO ----------------------------------------------------------------------
# Executable
cosmo_bin = os.path.join(exe_dir, "cosmo-org-ghg_pgi_e5e9e5ae_20201125")

# Namelists and slurm runscript templates
cosmo_namelist = '%s/cases/%s/cosmo_INPUT_' % (chain_src_dir,casename)
cosmo_runjob = '%s/cases/%s/cosmo_runjob.cfg' % (chain_src_dir,casename)

# Walltimes and domain decomposition
if compute_queue == "normal":
    cosmo_walltime = "03:00:00"
    cosmo_np_x = 24
    cosmo_np_y = 18
elif compute_queue == "debug":
    cosmo_walltime = "00:30:00"
    cosmo_np_x = 4
    cosmo_np_y = 3
else: 
    logging.error("Unknown queue name: %s" % compute_queue)
    sys.exit(1)

# Total node count
cosmo_np_io = 0
cosmo_np_tot = int(cosmo_np_x * cosmo_np_y / ntasks_per_node) + cosmo_np_io     


# POST-PROCESSING ============================================================
# REDUCE_OUTPUT --------------------------------------------------------------
convert_gas = True
output_levels = 20

# POST_COSMO -----------------------------------------------------------------
output_root = os.path.join("/store/empa/em05/", user, 
                           "processing_chain_output", casename)


