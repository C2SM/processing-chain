import os

# GENERAL SETTINGS =========================================================== 
user = os.environ['USER']
target = 'cosmo-ghg'
restart_step = 12 # hours
#subtarget = 'spinup'
#spinup = 6

compute_host = 'daint'
compute_queue = 'debug' # 'normal'
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
input_root = '/store/empa/em05/input_processing_chain_example/'

# METEO ----------------------------------------------------------------------
meteo_dir = os.path.join(input_root, 'meteo')
meteo_prefix = "laf"
meteo_inc = 1

# ICBC -----------------------------------------------------------------------
# CAMS for CO2, CO and NOX initial and boundary conditions
# if the data is already preprocessed and just need to be copied, 
# - cams_dir_orig is not used
# - cams_dir_proc is where your data to be copied is
# - cams_parameters should have one element per type of file you need to copy.
# It should have:
#       - "suffix" : files are called cams_dir_proc/suffix_date.nc
#       - "inc" : increment between icbc data 
cams_dir_orig = os.path.join(input_root, 'icbc') # Input directory
cams_dir_proc = os.path.join(input_root, 'icbc', 'processed') # Output directory

# If the data is not yet preprocessed and needs to run cams4int2cosmo
# cams_parameters should have one element per type of file you need to output.
# It should have:
# - species : the list of species to put in said file (within CO2, CO, CH4, NOX)
# - inc : the increment between timesteps
# - prefix1 : the input file prefix (cams_dir_orig/prefix1_date.nc)
# - prefix2 : the input surface pressure file prefix
#      (cams_dir_orig/prefix2_date.nc)
# - lev : the number of levels (137 or 60)
# - suffix : for the output file (cams_dir_proc/suffix_date.nc)
cams_parameters = [{
    "suffix" : "cams_co2",
    "inc" : 3
    #"species" :["CO2","CO","CH4"],     
    #"prefix1":"cams_gf39",
    #"prefix2":"sfc_gf39",
    #"lev":137,
},
]

# EMISSIONS ------------------------------------------------------------------
# Offline anthropogenic emissions
emissions_dir = os.path.join(input_root, 'emissions_coarse')
emis_gridname = "co2_"

# OAE ------------------------------------------------------------------------
# Online anthropogenic emissions
oae_dir = os.path.join(input_root, 'oae')
oae_gridded_emissions_nc = 'emissions.nc'
oae_vertical_profiles_nc = 'vertical_profiles.nc'
oae_hourofday_nc = 'hourofday.nc'
oae_hourofyear_nc = 'hourofyear.nc'
oae_dayofweek_nc = 'dayofweek.nc'
oae_monthofyear_nc = 'monthofyear.nc'

# BIOFLUXES ------------------------------------------------------------------ 
# VPRM biogenic fluxes for offline VPRM
vprm_dir = os.path.join(input_root,'vprm_smartcarb','processed')
vprm_prefix = ["vprm2_"]

# ONLINE_VPRM ---------------------------------------------------------------- 
# MODIS and vegetation data for online VPRM
online_vprm_dir = os.path.join(input_root, 'online_vprm')
modis_filename = 'MODIS_sur_refl_Example_20150101-20150201.nc'
vegetation_filename = 'VPRM_VegClasses_example_vprm.nc' 

# INT2LM ---------------------------------------------------------------------
# Extpar-file
int2lm_extpar_dir = os.path.join(input_root, 'extpar')
int2lm_extpar_file = "test_domain.nc"

# Executable
int2lm_bin = os.path.join(exe_dir, "int2lm_gnu_208d68e_20201005")

# Namelist and slurm runscript templates
int2lm_namelist = '%s/cases/%s/int2lm_INPUT.cfg' % (chain_src_dir, casename)
int2lm_runjob = '%s/cases/%s/int2lm_runjob.cfg' % (chain_src_dir, casename)

# Walltimes
if compute_queue=="normal":
    int2lm_walltime="01:00:00"
elif compute_queue=="debug":
    int2lm_walltime="00:30:00"
else: 
    logging.error("Unset queue name: %s" % compute_queue)
    sys.exit(1)

# Domain decomposition
int2lm_nodes = 2
int2lm_ntasks_per_node = 12 
int2lm_np_x = 8
int2lm_np_y = 3
int2lm_np_tot = int2lm_np_x * int2lm_np_y

# POST_INT2LM ----------------------------------------------------------------
# Fields that are used as initial conditions
post_int2lm_species = ["CO2_BG"]


# SIMULATION =================================================================
# COSMO ----------------------------------------------------------------------
# Executable
cosmo_bin = os.path.join(exe_dir, "cosmo-org-ghg-pgi_2b2a54e7_20210127")

# Namelists and slurm runscript templates
cosmo_namelist = '%s/cases/%s/cosmo_INPUT_' % (chain_src_dir, casename)
cosmo_runjob = '%s/cases/%s/cosmo_runjob.cfg' % (chain_src_dir, casename)

# Walltimes and domain decomposition
if compute_queue == "normal":
    cosmo_walltime = "00:30:00"
    cosmo_np_x = 6
    cosmo_np_y = 4
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
# Root directory where the output of the chain is copied to
output_root = os.path.join("/store/empa/em05/", user, 
                           "processing_chain_output", casename)

# VERIFY_CHAIN --------------------------------------------------------------- 
reference_dir = os.path.join(input_root, "reference_output")

# If the output file that gets compared to the reference is not at the location
# that post_cosmo copied it to, give the path to it here. Else leave it 'None'
#output_dir = None
output_dir = os.path.join(work_root, casename, '2015010100_12_24', 'cosmo', 'output')

# variables_to_check is a dict() with a tuple() of filenames as key and a list
# of variables-names as value. The tuple consists of the filenames of the two
# files to check, the list contains the variable-names that are compared.
# The verify_chain job will look for the files in the reference_dir (first tuple
# element) and the ouput_dir (second tuple element)
values_to_check = {("cosmo-ghg_pgi_gpu_e5e9e5a_lffd2015010200.nc",
                    "lffd2015010200.nc") :
                   ['T', 'P', 'U', 'V', 'W', 
                    'CO2_BG', 
                    'CO2_A', 'CO2_GPP', 'CO2_RA',
                    'CO2_A2', 'CO2_GPP2', 'CO2_RA2',
                   ]
                  }


