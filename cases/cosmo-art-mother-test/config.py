import os
"""
Configuration file for the 'cosmo-art-mother-test' case with COSMO-ART
"""

# GENERAL SETTINGS ===========================================================
user = os.environ['USER']
if os.path.exists(os.environ['HOME'] + '/.acct'):
    with open(os.environ['HOME'] + '/.acct', 'r') as file:
        compute_account = file.read().rstrip()
else:
    compute_account = os.popen("id -gn").read().splitlines()[0]
compute_host = 'daint'
compute_queue = 'normal'

# Controls which flavor of cosmo is used to do the simulation.
model = 'cosmo-art'

# case name = pathname in cases/
path = os.path.realpath(__file__)
casename = os.path.basename(os.path.dirname(path))

# Root directory of the sourcecode of the chain (where run_chain.py is)
chain_src_dir = os.getcwd()
# Root directory of the input data (for convenience, not used outside this file)
input_root = '/store/empa/em05/input_cosmoart_processing_chain_example/'
# Root directory of the working space of the chain
work_root = os.path.join(chain_src_dir, 'work')

# INPUT ====================================================================== #

# METEO ---------------------------------------------------------------------- #
# meteo files (either path to input dir or name of mother run)
meteo_dir = os.path.join(input_root, 'meteo')
meteo_prefix = "eas"
meteo_nameformat = meteo_prefix + '%Y%m%d%H'
meteo_inc = 3  # increment between timesteps

# EMISSIONS ------------------------------------------------------------------ #
# (possibly multiple) emissions-datasets
# for multiple datasets: emissions_dir & emis_gridname should be lists with
# corresponding path/prefix
emissions_dir = [
    os.path.join(input_root, 'emissions', 'emissions_mother_MACC_noSwiss'),
    os.path.join(input_root, 'emissions', 'emissions_mother_ch')
]
emis_gridname = ["macc_", "swiss_mu_coarse_"]

# PHOTO_RATE ----------------------------------------------------------------- #
photo_rate_file = os.path.join(input_root, 'art_photolysis', 'papa_data.d')

# OBS_NUDGING ---------------------------------------------------------------- #
obs_nudging_dir = os.path.join(input_root, 'obs_nudging')
# nudging-filename: obs_nudging_prefix +
#                   sim_date.strftime(obs_nudging_date_format) +
#                   (sim_date + timedelta(days=1)
#                   .strftime(obs_nudging_date_format)
# Example: obs_nudging temp for simulation on 04.02.2015:
#          cdfin_temp-20150204000000-20150205000000
obs_nudging_prefixes = [
    'cdfin_amdar', 'cdfin_buoy', 'cdfin_pilot_p', 'cdfin_ship', 'cdfin_synop',
    'cdfin_temp', 'cdfin_wprof'
]
obs_nudging_date_format = "-%Y%m%d%H%M%S"

# ICBC ----------------------------------------------------------------------- #
# if the data is already preprocessed and just need to be copied,
# - mozart_file_orig is not used
# - mozart_dir_proc is where your data to be copied is
mozart_file_orig = os.path.join(input_root, 'icbc',
                                'mozart4geos5_20150625-20150720_Europe.nc')
mozart_dir_proc = os.path.join(input_root, 'icbc', 'processed')
mozart_inc = 6  # increment between timesteps, change at your own risk
mozart_prefix = 'mozart'

# SIMULATION ================================================================= #

# INT2LM --------------------------------------------------------------------- #
# Extpar file
int2lm_extpar_dir = os.path.join(input_root, 'extpar')
int2lm_extpar_file = "external_parameter_empa_cosmo14.nc"
# landuse file
int2lm_lu_dir = os.path.join(input_root, 'extpar')
int2lm_lu_file = 'landuse_cosmo14.nc'
# plant functional types
int2lm_pft_dir = os.path.join(input_root, 'extpar')
int2lm_pft_file = 'pft_0.05_andrew.nc'
# Executable
int2lm_bin = os.path.join(input_root, "executables/int2lm")

int2lm_libgrib_dir = os.path.join(input_root, 'libgrib_api')

# Namelist and slurm runscript templates
int2lm_namelist = '%s/cases/%s/int2lm_INPUT.cfg' % (chain_src_dir, casename)
int2lm_runjob = '%s/cases/%s/int2lm_runjob.cfg' % (chain_src_dir, casename)

int2lm_walltime = "00:30:00"

# Domain decomposition
int2lm_nodes = 4
int2lm_ntasks_per_node = 36
int2lm_np_x = 12
int2lm_np_y = 12
int2lm_np_tot = int2lm_np_x * int2lm_np_y

# COSMO ---------------------------------------------------------------------- #
# Executable
cosmo_bin = os.path.join(input_root, "executables/cosmoart")

# Namelists and slurm runscript templates
cosmo_namelist = '%s/cases/%s/cosmo_INPUT_' % (chain_src_dir, casename)
cosmo_runjob = '%s/cases/%s/cosmo_runjob.cfg' % (chain_src_dir, casename)

cosmo_walltime = "01:30:00"

# Domain decomposition
cosmo_np_x = 12
cosmo_np_y = 12
cosmo_np_io = 0
cosmo_np_tot = cosmo_np_x * cosmo_np_y + cosmo_np_io

assert cosmo_np_tot // 36 == cosmo_np_tot / 36, (
    "n-tasks-per node is fixed at 36. "
    "The number of processes has there"
    "fore be divisible by 36 to get "
    "nodes at full capacity")
cosmo_n_nodes = cosmo_np_tot // 36

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
output_dir = os.environ['SCRATCH'] + (
    "/cosmoart_processing_chain/"
    "example_cosmoart_mother/2015020400_0_12/cosmo/output")

# variables_to_check is a dict() with a tuple() of filenames as key and a list
# of variables-names as value. The tuple consists of the filenames of the two
# files to check, the list contains the variable-names that are compared.
# The verify_chain job will look for the files in the reference_dir (first tuple
# element) and the ouput_dir (second tuple element)
values_to_check = {
    ("reference_mother_lffd2015020412.nc", "lffd2015020412.nc"): None
}
