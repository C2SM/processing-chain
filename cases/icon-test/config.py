import os

"""
Configuration file for the 'icon-test' case with ICON
"""

# GENERAL SETTINGS =========================================================== 
user = os.environ['USER']
target = 'icon'
restart_step = 24 # hours

compute_host = 'daint'
compute_queue = 'debug' # 'normal' / 'debug'
compute_account = 'em05'
constraint = 'gpu' # 'mc' / 'gpu'

if constraint == 'gpu':
    ntasks_per_node = 12
elif constraint == 'mc':
    ntasks_per_node = 36

# case name = pathname in cases/
path = os.path.realpath(__file__)
casename = os.path.basename(os.path.dirname(path))

# Root directory of the sourcecode of the chain (where run_chain.py is)
chain_src_dir = os.getcwd()

# Root directory of the working space of the chain
work_root = os.environ['SCRATCH'] + "/processing_chain"

# Directory where executables are stored
exe_dir = "/store/empa/em05/executables"

# Case directory
case_dir = os.path.join(chain_src_dir, 'cases', casename)

# PRE-PROCESSING =============================================================
input_root = '/store/empa/em05/input_icon_processing_chain_example/'
input_root_icbc = os.path.join(input_root, 'icbc')
# meteo
input_root_meteo = '/store/empa/em05/dbrunner/icon-art/meteo'
meteo_prefix = 'ifs'
source_nameformat = meteo_prefix + '_%Y%m%d%H'
meteo_inc = 3

# ICONTools ------------------------------------------------------------------
icontools_parameter = {
    'remap_ic_runjob': 'icontools_remap_ic_runjob.cfg',
    'auxgrid_runjob': 'icontools_auxgrid_runjob.cfg',
    'remap_ana_lbc_runjob': 'icontools_remap_ana_lbc_runjob.cfg',
    'remap_fc_lbc_runjob': 'icontools_remap_fc_lbc_runjob.cfg',
    'remap_aux_runjob': 'icontools_remap_aux_runjob.cfg',
    'namelist_iconsub': 'icontools_namelist_iconsub.cfg',
    'namelist_remapfields_ic': 'icontools_namelist_remapfields_ic.cfg',
    'namelist_remapfields_ana_lbc': 'icontools_namelist_remapfields_ana_lbc.cfg',
    'namelist_remapfields_fc_lbc': 'icontools_namelist_remapfields_fc_lbc.cfg',
    'namelist_remap': 'icontools_namelist_remap.cfg',
}

# Input data for runscript----------------------------------------------------
# Grid
input_root_grid = os.path.join(input_root, 'grid')
radiation_grid_filename = os.path.join(input_root_grid, "VERIFY_DOM_DOM01.parent.nc")
dynamics_grid_filename = os.path.join(input_root_grid, "VERIFY_DOM_DOM01.nc")
map_file_latbc = os.path.join(input_root_grid, "map_file.latbc")
extpar_filename = os.path.join(input_root_grid, 
                               "external_parameter_icon_VERIFY_DOM_DOM01_tiles.nc")
lateral_boundary_grid = os.path.join(input_root_grid, "lateral_boundary.grid.nc")

input_root_rad = os.path.join(input_root, 'rad')
cldopt_filename = os.path.join(input_root_rad, 'rrtm_cldopt.nc')
lrtm_filename = os.path.join(input_root_rad, 'rrtmg_lw.nc')

input_root_mapping = os.path.join(input_root, 'mapping')
map_file_ana = os.path.join(input_root_mapping, "map_file.ana")

# File names -----------------------------------------------------------------
latbc_filename = "ifs_201801<d><h>_lbc.nc"
inidata_filename = "ifs_init_2018010100.nc"

output_filename = "NWP_LAM"
filename_format = "<output_filename>_DOM<physdom>_<ddhhmmss>"

# SIMULATION =================================================================
# ICON -----------------------------------------------------------------------
# Executable
icon_bin = os.path.join(exe_dir, "icon-pgi-20.1.1-cpu-20210816")

# Icontools executables
icontools_dir = exe_dir
iconremap_bin = "iconremap"
iconsub_bin   = "iconsub"

# Namelists and slurm runscript templates
icon_runjob = os.path.join(case_dir, 'runscript.cfg')

# Walltimes and domain decomposition
if compute_queue == "normal":
    icon_walltime = "00:30:00"
    icon_np_tot = 12
elif compute_queue == "debug":
    icon_walltime = "00:30:00"
    icon_np_tot = 10
else: 
    logging.error("Unknown queue name: %s" % compute_queue)
    sys.exit(1)

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
# that post_icon copied it to, give the path to it here. Else leave it 'None'
#output_dir = None
output_dir = os.path.join(work_root, casename, '2018010100_0_24', 'icon', 'output')

# variables_to_check is a dict() with a tuple() of filenames as key and a list
# of variables-names as value. The tuple consists of the filenames of the two
# files to check, the list contains the variable-names that are compared.
# The verify_chain job will look for the files in the reference_dir (first tuple
# element) and the ouput_dir (second tuple element)
values_to_check = {("icon-pgi-20.1.1-cpu-20210215-NWP_LAM_DOM01_01000000.nc",
                    "NWP_LAM_DOM01_01000000.nc") :
                   ['temp', 'pres', 'u', 'v', 'w', 
                   ]
                  }


