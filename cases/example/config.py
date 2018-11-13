import os


# ORGANIZATIONAL ============================================================= #

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

compute_host = 'daint'
compute_queue = 'debug' #'debug' #'normal'
compute_account = 'em05' #'pr04'

# case name = pathname in cases/
path = os.path.realpath(__file__)
casename = os.path.basename(os.path.dirname(path)) 

# Restart 
restart_step = 24

# Root directory of the sourcecode of the chain (where run_chain.py is)
chain_src_dir = os.getcwd()
# Root directory of the input data (for convenience, not used outside this file)
input_root = '/store/empa/em05/input_processing_chain_example/'
# Root directory of the working space of the chain
work_root = os.environ['SCRATCH'] + "/processing_chain"


# INPUT ====================================================================== #

# METEO ---------------------------------------------------------------------- #
meteo_dir = os.path.join(input_root, 'meteo')
meteo_prefix = "laf"
meteo_inc = 1


# EMISSIONS ------------------------------------------------------------------ #
# anthropogenic emissions pre-processed for mother and nested domain
emissions_dir = os.path.join(input_root, 'emissions_coarse')
emis_gridname = "CO2_CO_NOX_Berlin-coarse_"


# BIOFLUXES ------------------------------------------------------------------ #
# VPRM biogenic fluxes
vprm_dir = os.path.join(input_root,'vprm_smartcarb','processed')
vprm_prefix = ["vprm_"] #could be ["gpp_", "ra_"]


# ICBC ----------------------------------------------------------------------- #
# CAMS for CO2, CO and NOX initial and boundary conditions

# if the data is already preprocessed and just need to be copied, 
# - cams_dir_orig is not used
# - cams_dir_proc is where your data to be copied is
# - cams_parameters should have one element per type of file you need to copy.
# It should have:
#       - "suffix" : files are called cams_dir_proc/suffix_date.nc
#       - "inc" : increment between icbc data 
cams_dir_orig = os.path.join(input_root, 'icbc') #Input directory
cams_dir_proc = os.path.join(input_root, 'icbc', 'processed') #Output directory


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
# If ctnoaa4int2cosmo.py is used, need all the following, otherwise
# just the suffix
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


# SIMULATION ================================================================= #

# INT2LM --------------------------------------------------------------------- #
# Extpar-file
int2lm_extpar_dir = os.path.join(input_root, 'extpar')
int2lm_extpar_file = "test_domain.nc"

# Executable
int2lm_bin = os.path.join(input_root,"executables/int2lm") 

# Namelist and slurm runscript templates
int2lm_namelist = '%s/cases/%s/int2lm_INPUT.cfg' % (chain_src_dir,casename)
int2lm_runjob = '%s/cases/%s/int2lm_runjob.cfg' % (chain_src_dir,casename)

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


# POST_INT2LM ---------------------------------------------------------------- #
post_int2lm_species = ["CO2_BG"]#,"CO_BG","CH4_BG","NOX_BG"]


# COSMO ---------------------------------------------------------------------- #
# Executable
# COSMO ---------------------------------------------------------------------- #
# Executable
cosmo_bin=  os.path.join(input_root,"executables/cosmo_20181029") 

# Namelists and slurm runscript templates
cosmo_namelist = '%s/cases/%s/cosmo_INPUT_' % (chain_src_dir,casename)
cosmo_runjob = '%s/cases/%s/cosmo_runjob.cfg' % (chain_src_dir,casename)

# Walltimes and domain decomposition
if compute_queue=="normal":
    cosmo_walltime="01:00:00"
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


# POSTPROCESSING ============================================================= #

# POST_COSMO ----------------------------------------------------------------- #
# Root directory where the output of the chain is copied to
output_root = os.environ['SCRATCH'] + "/processing_chain/output/" + casename


# VERIFY_CHAIN --------------------------------------------------------------- #
reference_dir = os.path.join(input_root, "reference_output")

# If the output file that gets compared to the reference is not at the location
# that post_cosmo copied it to, give the path to it here. Else leave it 'None'
# output_dir = None
# Use this if the post_cosmo job is not executed
output_dir = os.environ['SCRATCH'] + ("/processing_chain/example/"
                                      "2015010100_0_24/cosmo/output")

# variables_to_check is a dict() with a tuple() of filenames as key and a list
# of variables-names as value. The tuple consists of the filenames of the two
# files to check, the list contains the variable-names that are compared.
# The verify_chain job will look for the files in the reference_dir (first tuple
# element) and the ouput_dir (second tuple element)
values_to_check = {("reference_lffd2015010200.nc","lffd2015010200.nc") :
                      ['T', 'U', 'V', 'CO2_A']}
