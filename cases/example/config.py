import os
import types


user = os.environ['USER']
mail_address = {
    'gkuhl':    'gerrit.kuhlmann@empa.ch',
    'kug':      'gerrit.kuhlmann@empa.ch',
    'dbrunner': 'dominik.brunner@empa.ch',
    'haussaij' : 'jean-matthieu.haussaire@empa.ch'
}[user]


# Everything defined before is not written to os.environ
# and thus not available to bash scripts.
not_config = list(locals().keys())


compute_host = 'daint'
compute_queue = 'debug' #'debug' #'normal'
compute_account = 'sd02' #'pr04'

# input root
input_root = '/store/empa/em05/input_processing_chain_example/'
meteo_dir = os.path.join(input_root, 'meteo')

# output
output_root = '/scratch/snx3000/%s/processing_chain/output_example' % user

# working root
work_root = '/scratch/snx3000/%s/processing_chain' % user
log_dir = os.path.join(work_root, 'logs')


# anthropogenic emissions pre-processed for mother and nested domain
emissions_dir = os.path.join(input_root, 'emissions_coarse')
emis_gridname = "CO2_CO_NOX_Berlin-coarse_"


# VPRM biogenic fluxes
vprm_dir = os.path.join(input_root,'vprm_smartcarb','processed')
vprm_prefix = ["vprm_"] #could be [gpp_, ra_]

# CAMS for CO2, CO and NOX initial and boundary conditions

# if the data is already preprocessed and just need to be copied, 
# - cams_dir_orig is not used
# - cams_dir_proc is where your data to be copied is
# - cams_parameters should have one element per type of file you need to copy. It should have:
#       - "suffix" : files are called cams_dir_proc/suffix_date.nc
#       - "inc" : increment between icbc data 
cams_dir_orig = os.path.join(input_root, 'icbc') #Input directory
cams_dir_proc = os.path.join(input_root, 'icbc', 'processed') #Output directory


# If the data is not yet preprocessed and needs to run cams4int2cosmo
# cams_parameters should have one element per type of file you need to output. It should have:
# - species : the list of species to put in said file (within CO2, CO, CH4, NOX)
# - inc : the increment between timesteps
# - prefix1 : the input file prefix (cams_dir_orig/prefix1_date.nc)
# - prefix2 : the input surface pressure file prefix (cams_dir_orig/prefix2_date.nc)
# - lev : the number of levels (137 or 60)
# - suffix : for the output file (cams_dir_proc/suffix_date.nc)
cams_parameters = [{
    "suffix" : "cams_co2",
    "inc" : 3
     # "species" :["CO2","CO","CH4"],     
     # "prefix1":"cams_gf39",
     # "prefix2":"sfc_gf39",
     # "lev":137,
},
    {"suffix":"cams_nox",
     "inc" : 3,
     # "species" :["NOX"],     
     # "prefix1":"cams_0001",
     # "prefix2":"sfc_0001",
     # "lev":60,
     }]

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
#     {"inc" : 24,
#     "prefix":"CT2016.molefrac_glb3x2",
#     "suffix":"ct"}]



# chain root (TODO: remove)
chain_src_dir = os.getcwd()
tools_dir = os.path.join(chain_src_dir, 'jobs/tools')

# some constants for scripts
del_tmp_timeout = 120   # timeout in days after which temporary files 
                        # like restarts (last forecast hour of day - 1) are removed
                        # by "purgetmp"

meteo_spinup = 0        # time in hours the model is integrated before transport simulation 
                        # is started (i.e. spinupstart = inidate - meteospinup)
                        # NOTE: has to be multiple of meteo increment (e.g 3hrs for IFS)


# INT2LM
int2lm_extpar_dir = os.path.join(input_root, 'extpar')
int2lm_extpar_file = "test_domain.nc"
int2lm_bin = os.path.join(input_root,"executables/int2lm") 

#post_int2lm
post_int2lm_species = ["CO2_BG"]#,"CO_BG","CH4_BG","NOX_BG"]


# COSMO
cosmo_bin=  os.path.join(input_root,"executables/cosmo") 

# Case specific settings (int2lm and cosmo namelists and runscripts)
path = os.path.realpath(__file__)
casename = os.path.basename(os.path.dirname(path)) # pathname in example/

int2lm_namelist = '%s/cases/%s/int2lm_INPUT' % (chain_src_dir,casename)
int2lm_runjob = '%s/cases/%s/int2lm_runjob' % (chain_src_dir,casename)
cosmo_namelist = '%s/cases/%s/cosmo_INPUT' % (chain_src_dir,casename)
cosmo_runjob = '%s/cases/%s/cosmo_runjob' % (chain_src_dir,casename)


# Walltimes and domain decomposition

## INT2LM
if compute_queue=="normal":
    int2lm_walltime="24:00:00"
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
    cosmo_walltime="24:00:00"
    cosmo_np_x=6
    cosmo_np_y=5
elif compute_queue=="debug":
    cosmo_walltime="00:30:00"
    cosmo_np_x=1
    cosmo_np_y=1
else: 
    logging.error("Unknown queue name: %s" % compute_queue)
    sys.exit(1)

cosmo_np_io = 0
cosmo_np_tot = cosmo_np_x * cosmo_np_y + cosmo_np_io     
