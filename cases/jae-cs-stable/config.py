import os
import types


user = os.environ['USER']
mail_address = {
    'gkuhl':    'gerrit.kuhlmann@empa.ch',
    'kug':      'gerrit.kuhlmann@empa.ch',
    'dbrunner': 'dominik.brunner@empa.ch',
    'haussaij': 'jean-matthieu.haussaire@empa.ch',
    'mjaehn':   'michael.jaehn@empa.ch',
}[user]

# Everything defined before is not written to os.environ
# and thus not available to bash scripts.
not_config = list(locals().keys())

compute_host = 'daint'
compute_queue = 'normal' #'debug' #'normal'
compute_account = 'sd02' #'pr04'

# case name = pathname in cases/
path = os.path.realpath(__file__)
casename = os.path.basename(os.path.dirname(path))

# input root
input_root = '/store/empa/em05/mjaehn/Carbosense/input'
meteo_dir = os.path.join(input_root, 'meteo')
meteo_prefix = "laf"
meteo_inc = 1

# output
output_root = '/scratch/snx3000/mjaehn/Carbosense/output/%s' % casename

# working root
work_root = '/scratch/snx3000/mjaehn/Carbosense'
log_dir = os.path.join(work_root, 'logs')

# anthropogenic emissions pre-processed for mother and nested domain
emissions_dir = os.path.join(input_root, 'emissions')
emis_gridname = 'CO2_CO_CH4_Carbosense-1.0_'

# VPRM biogenic fluxes
vprm_dir = os.path.join(input_root,'vprm','processed')
vprm_prefix = ['gpp_', 'ra_'] 

# CAMS for CO2, CO and NOX initial and boundary conditions
cams_dir_orig = os.path.join(input_root, 'icbc') #Input directory
cams_dir_proc = os.path.join(input_root, 'icbc', 'processed') #Output directory
# required parameters for cams preprocessing
# The list should contain one element per output file
# If cams4int2cosmo.py is used, need all the following, otherwise just the suffix
# - species : the list of species to put in said file (within CO2, CO, CH4, NOX)
# - inc : the increment between timesteps
# - prefix1 : the input file prefix (cams_dir_orig/prefix1_date.nc)
# - prefix2 : the input surface pressure file prefix (cams_dir_orig/prefix2_date.nc)
# - lev : the number of levels (137 or 60)
# - suffix : for the output file (cams_dir_proc/suffix_date.nc)
cams_parameters = [
    {'suffix':'cams_co2',
     # ' species' :['CO2','CO','CH4'],
     'inc' : 3,
     # 'prefix1':'cams_gf39',
     # 'prefix2':'sfc_gf39',
     # 'lev':137,
     }
     #{'suffix':'cams_nox',
     # 'species' :['NOX'],
     # 'inc' : 3,
     # 'prefix1':'cams_0001',
     # 'prefix2':'sfc_0001',
     # 'lev':60,
     #}
    ]

# CarbonTracker for CO2, CO and NOX initial and boundary conditions
# ct_dir_orig = os.path.join(input_root, 'icbc') #Input directory
# ct_dir_proc = os.path.join(input_root, 'icbc', 'processed') #Output directory
# # required parameters for ct preprocessing
# # The list should contain one element per output file
# If ctnoaa4int2cosmo.py is used, need all the following, otherwise just the suffix
# # - inc : the increment between two input files. 
# #         It should be 24 since there's 1 input file per day
# #         The increment is hard coded to 3h
# # - prefix : the input file prefix (cams_dir_orig/prefix_YYYY-MM-DD.nc)
# # - suffix : for the output file (cams_dir_proc/suffix_YYYYMMDDHH.nc)
# # as of today, it only computes CO2
# ct_parameters = [
#     {'inc' : 24,
#     'prefix':'CT2016.molefrac_glb3x2',
#     'suffix':'ct'}]



# chain root
chain_src_dir = os.getcwd()
tools_dir = os.path.join(chain_src_dir, 'jobs/tools')

# some constants for scripts
del_tmp_timeout = 120   # timeout in days after which temporary files 
                        # like restarts (last forecast hour of day - 1) are removed
                        # by 'purgetmp'

meteo_spinup = 0        # time in hours the model is integrated before transport simulation 
                        # is started (i.e. spinupstart = inidate - meteospinup)
                        # NOTE: has to be multiple of meteo increment (e.g 3hrs for IFS)

# INT2LM
int2lm_extpar_dir = os.path.join(input_root, 'extpar')
int2lm_extpar_file = 'extpar_empa_cosmo1_aster_opt.nc'
int2lm_bin = '/users/mjaehn/github/int2lm/int2lm_tracer_conversion_cray'

# post_int2lm
post_int2lm_species = ['CO2_BG', 'CO_BG', 'CH4_BG']

# COSMO
cosmo_bin = '/users/mjaehn/github/cosmo-pompa/cosmo/cosmo_smartcarb_ser_gpu'

# Case specific settings (int2lm and cosmo namelists and runscripts)
int2lm_namelist = '%s/cases/%s/int2lm_INPUT.cfg' % (chain_src_dir,casename)
int2lm_runjob = '%s/cases/%s/int2lm_runjob.cfg' % (chain_src_dir,casename)
cosmo_namelist = '%s/cases/%s/cosmo_INPUT_' % (chain_src_dir,casename)
cosmo_runjob = '%s/cases/%s/cosmo_runjob.cfg' % (chain_src_dir,casename)


# Walltimes and domain decomposition

## INT2LM
if compute_queue=="normal":
    int2lm_walltime="12:00:00"
elif compute_queue=="debug":
    int2lm_walltime="00:30:00"
else: 
    logging.error("Unset queue name: %s" % compute_queue)
    sys.exit(1)

int2lm_nodes = 2
int2lm_ntasks_per_node = 12 
int2lm_np_x = 8
int2lm_np_y = 3
int2lm_np_tot = int2lm_np_x * int2lm_np_y

## COSMO 
if compute_queue=="normal":
    cosmo_walltime="18:00:00"
    cosmo_np_x=6
    cosmo_np_y=5
elif compute_queue=="debug":
    cosmo_walltime="00:30:00"
    cosmo_np_x=1
    cosmo_np_y=1
else: 
    logging.error("Unknown queue name: %s" % compute_queue)
    sys.exit(1)

cosmo_np_io = 1
cosmo_np_tot = cosmo_np_x * cosmo_np_y + cosmo_np_io     

# Restart 
restart_step = 240
