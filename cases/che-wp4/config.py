import os

# GENERAL SETTINGS =========================================================== 
user = os.environ['USER']
mail_address = {
    'dbrunner': 'dominik.brunner@empa.ch',
    'shenne':   'stephan.henne@empa.ch',
    'mjaehn':   'michael.jaehn@empa.ch',
    'haussaij': 'jean-matthieu.haussaire@empa.ch',
    'gkuhl':    'gerrit.kuhlmann@empa.ch',
    'isuter':   'ivo.suter@empa.ch',
}[user]

target = 'cosmo'
restart_step = 120 # 5 days
#subtarget = 'spinup'
#spinup = 6

compute_host = 'daint'
compute_queue = 'normal'
compute_account = 'em05'
constraint = 'gpu'

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
input_root = "/store/empa/em05/mjaehn/CHE/input"
input_root_brd = "/store/empa/em05/dbrunner/che" # for meteo and oae

# METEO ----------------------------------------------------------------------
meteo_dir = os.path.join(input_root_brd, 'meteo')
meteo_prefix = "eas"
meteo_inc = 3

# ICBC -----------------------------------------------------------------------
cams_dir_orig = os.path.join(input_root_brd, 'icbc') 
cams_dir_proc = os.path.join(input_root, 'icbc', 'processed')
cams_parameters = [{
    "suffix" : "cams_co2",
    "inc" : 3,
    "species" : ["CO2", "CO"],
    "prefix1": "cams_gvri",
    "prefix2": "sfc_gvri",
    "lev": 137,
},
{
    "suffix" : "LSCE_d14C",
    "inc" : 3,
    "species" : ["C14"],
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

# EMISSIONS ------------------------------------------------------------------
emissions_dir = [ os.path.join(input_root, 'emissions'),
                  os.path.join(input_root, 'emissions'),
                  os.path.join(input_root, 'emissions'),
                ]
emis_gridname = ["emis_", "npp_", "ocean_"]


# BIOFLUXES ------------------------------------------------------------------
vprm_dir = os.path.join(input_root, 'vprm', 'processed')
vprm_prefix = ["vprm_", "dc14__"]

# INT2LM ---------------------------------------------------------------------
# Extpar-file
int2lm_extpar_dir = os.path.join(input_root, 'extpar')
int2lm_extpar_file = 'external_parameter_hjm_globe.nc'

# Executable
int2lm_bin = os.path.join(exe_dir, 'int2lm_gnu_208d68e_20201005')

# Namelist and slurm runscript templates
int2lm_namelist = '%s/cases/%s/int2lm_INPUT.cfg' % (chain_src_dir, casename)
int2lm_runjob = '%s/cases/%s/int2lm_runjob.cfg' % (chain_src_dir, casename)

# Walltime
if compute_queue == "normal":
    int2lm_walltime = "03:30:00"
elif compute_queue == "debug":
    int2lm_walltime = "00:30:00"
else: 
    logging.error("Unset queue name: %s" % compute_queue)
    sys.exit(1)

# Domain decomposition
int2lm_nodes = 4
int2lm_ntasks_per_node = 12 
int2lm_np_x = 8
int2lm_np_y = 6
int2lm_np_tot = int2lm_np_x * int2lm_np_y

# POST_INT2LM ----------------------------------------------------------------
# Fields that are used as initial conditions
post_int2lm_species = ['CO2_BG', 'CO_BG', 'C14_BG']


# SIMULATION =================================================================
# COSMO ----------------------------------------------------------------------
# Executable
cosmo_bin = os.path.join(exe_dir, "cosmo-org-ghg_pgi_e5e9e5ae_20201125")

# Namelists and slurm runscript templates
cosmo_namelist = '%s/cases/%s/cosmo_INPUT_' % (chain_src_dir, casename)
cosmo_runjob = '%s/cases/%s/cosmo_runjob.cfg' % (chain_src_dir, casename)

# Walltimes and domain decomposition
if compute_queue == "normal":
    cosmo_walltime = "03:00:00"
    cosmo_np_x = 32
    cosmo_np_y = 30
elif compute_queue == "debug":
    cosmo_walltime = "00:30:00"
    cosmo_np_x = 4
    cosmo_np_y = 3
else: 
    logging.error("Unknown queue name: %s" % compute_queue)
    sys.exit(1)

# Total node count
cosmo_np_io = 0
cosmo_np_tot = int(cosmo_np_x * cosmo_np_y / ntasks_per_node) + cosmo_np_io # 80 


# POST-PROCESSING ===========================================================
# REDUCE_OUTPUT --------------------------------------------------------------
convert_gas = True
#output_levels = 20  # keep all levels

# POST_COSMO -----------------------------------------------------------------
output_root = os.path.join("/store/empa/em05/", user, 
                           "processing_chain_output", casename)


