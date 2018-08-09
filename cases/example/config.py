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
compute_queue = 'normal' #'debug' #'normal'
compute_account = 'sd02' #'pr04'

# input root
input_root = '/scratch/snx3000/haussaij/input'
meteo_dir = os.path.join(input_root, 'meteo')

# output
output_root = '/scratch/snx3000/haussaij/output_proc_chain'

# working root
work_root = '/scratch/snx3000/haussaij/test_processing_chain'
log_dir = os.path.join(work_root, 'logs')


# anthropogenic emissions pre-processed for mother and nested domain
emissions_dir = os.path.join(input_root, 'emissions_coarse')
emis_gridname = "CO2_CO_NOX_Berlin-coarse_"


# VPRM biogenic fluxes
vprm_dir = os.path.join(input_root,'vprm_smartcarb','processed')
vprm_prefix = ["vprm_"] #could be [gpp_, ra_]

# CAMS for CO2, CO and NOX initial and boundary conditions
cams_dir_orig = os.path.join(input_root, 'icbc') #Input directory
cams_dir_proc = os.path.join(input_root, 'icbc', 'processed2') #Output directory
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
    {"suffix":"cams_co2",
     # " species" :["CO2","CO","CH4"],
     # "inc" : 3,
     # "prefix1":"cams_gf39",
     # "prefix2":"sfc_gf39",
     # "lev":137,
     }
    {"suffix":"cams_nox",
     # "species" :["NOX"],
     # "inc" : 3,
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
chain_src_dir = '/users/haussaij/cosmo_test/python_proc_chain/cosmo_processing_chain/'
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
int2lm_bin = '/users/haussaij/cosmo_official/int2lm/int2lm'

#post_int2lm
post_int2lm_species = ["CO2_BG"]#,"CO_BG","CH4_BG","NOX_BG"]


# COSMO
cosmo_bin= '/users/haussaij/cosmo_official/cosmo-pompa/cosmo/cosmo'

# Case specific settings (int2lm and cosmo namelists and runscripts)
casename = "example"

int2lm_namelist = '%s/cases/%s/int2lm_INPUT' % (chain_src_dir,casename)
int2lm_runjob = '%s/cases/%s/int2lm_runjob' % (chain_src_dir,casename)
cosmo_namelist = '%s/cases/%s/cosmo_INPUT' % (chain_src_dir,casename)
cosmo_runjob = '%s/cases/%s/cosmo_runjob' % (chain_src_dir,casename)


# add local variables (defined after "not_config") to os.environ to be used
# in bash scripts called by this script
for key, value in list(locals().items()):
    if key != 'not_config' and key not in not_config:
        os.environ[key] = str(value)
