import os
import types


user = os.environ['USER']
mail_address = {
    'gkuhl':     'gerrit.kuhlmann@empa.ch',
    'kug':       'gerrit.kuhlmann@empa.ch',
    'dbrunner':  'dominik.brunner@empa.ch',
    'haussaij':  'jean-matthieu.haussaire@empa.ch',
    'mjaehn':    'michael.jaehn@empa.ch',
    'muq':       'qing.mu@empa.ch',
    'parsenov' : 'pavle.arsenovic@empa.ch',
    'ochsnerd' : 'david.ochsner@empa.ch',
}[user]


# Everything defined before is not written to os.environ
# and thus not available to bash scripts.
not_config = list(locals().keys())

compute_host = 'daint'
compute_queue = 'normal'
compute_account = 'em05'

# Controls which flavour of cosmo is used to do the simulation. Has to be either 'comso' or 'cosmoart'
# If omitted, will default to 'cosmo'
target = 'cosmoart'

# case name = pathname in cases/
path = os.path.realpath(__file__)
casename = os.path.basename(os.path.dirname(path))

# input root
input_root = '/store/empa/em05/input_cosmoart_processing_chain_example/'

# output
output_root = os.environ['SCRATCH'] + "/cosmoart_processing_chain/output/" + casename

# working root
work_root = os.environ['SCRATCH'] + "/cosmoart_processing_chain"
log_dir = os.path.join(work_root, 'logs')

# anthropogenic emissions pre-processed for mother and nested domain
emissions_dir = os.path.join(input_root, 'emissions', 'emissions_mother_MACC')
emis_gridname = "macc_"

# ifs_hres_bc files
ifs_hres_dir = os.path.join(input_root, 'ifs_hres_bc')
ifs_hres_inc = 3  # increment between timesteps
ifs_basename = "eas"

# photolysis-rate file
photo_rate_file = os.path.join(input_root, 'art_photolysis', 'papa_data.p')

# obs_nudging files
obs_nudging_dir = os.path.join(input_root, 'obs_nudging')
# nudging-filename: obs_nudging_prefix +
#                   sim_date.strftime(obs_nudging_date_format) +
#                   (sim_date + timedelta(days=1).strftime(obs_nudging_date_format)
# Example: obs_nudging temp for simulation on 04.02.2015:
#          cdfin_temp-20150204000000-20150205000000
obs_nudging_prefixes = ['cdfin_amdar', 'cdfin_buoy', 'cdfin_pilot_p',
                        'cdfin_ship', 'cdfin_synop', 'cdfin_temp',
                        'cdfin_wprof']
obs_nudging_date_format = "-%Y%m%d%H%M%S"


# ICBC
# if the data is already preprocessed and just need to be copied, 
# - mozart_file_orig is not used
# - mozart_dir_proc is where your data to be copied is
mozart_file_orig = os.path.join(input_root, 'icbc', 'mozart4geos5_20150203-20150221.nc')
mozart_dir_proc = os.path.join(input_root, 'icbc', 'processed')
mozart_inc = 6 # increment between timesteps, not sure if this can be changed even
mozart_prefix = 'mozart'

# chain root (TODO: remove)
chain_src_dir = os.getcwd()
tools_dir = os.path.join(chain_src_dir, 'jobs/tools')

# INT2LM
int2lm_extpar_dir = os.path.join(input_root, 'extpar')
int2lm_extpar_file = "external_parameter_empa_cosmo14.nc"
int2lm_bin = os.path.join(input_root,"executables/int2lm")

int2lm_libgrib_dir = os.path.join(input_root, 'libgrib_api')

# COSMO
cosmo_bin = os.path.join(input_root,"executables/cosmoart") 

# Case specific settings (int2lm and cosmo namelists and runscripts)

int2lm_namelist = '%s/cases/%s/int2lm_INPUT.cfg' % (chain_src_dir,casename)
int2lm_runjob = '%s/cases/%s/int2lm_runjob.cfg' % (chain_src_dir,casename)
cosmo_namelist = '%s/cases/%s/cosmo_INPUT_' % (chain_src_dir,casename)
cosmo_runjob = '%s/cases/%s/cosmo_runjob.cfg' % (chain_src_dir,casename)


# Walltimes and domain decomposition

## INT2LM
int2lm_walltime="00:10:00"

int2lm_nodes = 4
int2lm_ntasks_per_node = 36 
int2lm_np_x = 12
int2lm_np_y = 12
int2lm_np_tot = int2lm_np_x * int2lm_np_y

## COSMO 
cosmo_walltime="01:30:00"
cosmo_np_x=12
cosmo_np_y=12
cosmo_np_io = 0
cosmo_np_tot = cosmo_np_x * cosmo_np_y + cosmo_np_io     

assert cosmo_np_tot//36 == cosmo_np_tot/36, ("n-tasks-per node is fixed at 36. "
                                             "The number of processes has there"
                                             "fore be divisible by 36 to get "
                                             "nodes at full capacity")
cosmo_n_nodes = cosmo_np_tot // 36

# Restart 
restart_step = 24

## Verification
reference_dir = os.path.join(input_root, "reference_output")
# If the output file that gets compared to the reference is not at the location
# that post_cosmo copied it to, give the path to it here. Else leave it 'None'
# output_dir = None
# Use this if the post_cosmo job is not executed
output_dir = os.environ['SCRATCH'] + ("/cosmoart_processing_chain/"
    "example_cosmoart_mother/2015020400_0_12/cosmo/output")

# variables_to_check is a dict() with a tuple() of filenames as key and a list
# of variables-names as value. The tuple consists of the filenames of the two
# files to check, the list contains the variable-names that are compared.
# The verify_chain job will look for the files in the reference_dir (first tuple
# element) and the ouput_dir (second tuple element)
values_to_check = {("reference_mother_lffd2015020412.nc","lffd2015020412.nc") : None}
