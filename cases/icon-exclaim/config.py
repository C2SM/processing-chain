import os, glob
"""
Configuration file for the 'icon-exclaim' case with ICON
"""

# GENERAL SETTINGS ===========================================================
user = os.environ['USER']
target = 'icon'
restart_step = 24  # hours

compute_host = 'daint'
compute_queue = 'normal'  # 'normal' / 'debug'
compute_account = 'g142'
constraint = 'gpu'  # 'mc' / 'gpu'

if constraint == 'gpu':
    ntasks_per_node = 12
elif constraint == 'mc':
    ntasks_per_node = 36

# Walltimes and domain decomposition
if compute_queue == "normal":
    icon_walltime = "12:00:00"
    icon_np_tot = 30
elif compute_queue == "debug":
    icon_walltime = "00:30:00"
    icon_np_tot = 10
else:
    logging.error("Unknown queue name: %s" % compute_queue)
    sys.exit(1)

# case name = pathname in cases/
path = os.path.realpath(__file__)
casename = os.path.basename(os.path.dirname(path))

# Root directory of the sourcecode of the chain (where run_chain.py is)
chain_src_dir = os.getcwd()

# Root directory of the working space of the chain
work_root = os.environ['SCRATCH'] + "/processing_chain"

# Directory where executables are stored
exe_dir = os.path.join("/store/g142", user, "local/bin")

# Case directory
case_dir = os.path.join(chain_src_dir, 'cases', casename)

# PRE-PROCESSING =============================================================
input_root = os.path.join("/store/g142", user, "icon_data/cases/exclaim")
input_root_icbc = os.path.join(input_root, 'icbc')

# meteo
doMeteo = False
input_root_meteo = input_root_icbc  # this might actually be the default
meteo_inc = 1
meteo_prefix = 'efsf'
meteo_nameformat = meteo_prefix + '%d%H%M%S'
meteo_suffix = '_lbc.nc'

# Ic Bc
copyICBC = True

# ICONTools ------------------------------------------------------------------
# Icontools executables
icontools_dir = glob.glob(
    '/project/g110/spack-install/daint/icontools/c2sm-master/gcc/*/bin')[0]
iconremap_bin = os.path.join(icontools_dir, "iconremap")
iconsub_bin = os.path.join(icontools_dir, "iconsub")

icontools_runjobs = [
    #'icontools_remap_ic_runjob.cfg',
    #'icontools_remap_00_lbc_runjob.cfg',
    #'icontools_remap_lbc_rest_runjob.cfg',
]

# Input data for runscript----------------------------------------------------
# Grid
input_root_grid = os.path.join(input_root, 'grid')
radiation_grid_filename = os.path.join(input_root_grid,
                                       'ICON-1E_DOM01.parent.nc')
dynamics_grid_filename = os.path.join(input_root_grid, 'ICON-1E_DOM01.nc')
map_file_latbc = os.path.join(input_root_grid, 'map_file.latbc')
extpar_filename = os.path.join(input_root_grid, 'extpar_exclaim.nc')
lateral_boundary_grid = os.path.join(input_root_grid,
                                     'lateral_boundary.grid.nc')

# Radiation
input_root_rad = os.path.join(input_root, 'rad')
cldopt_filename = os.path.join(input_root_rad, 'rrtm_cldopt.nc')
lrtm_filename = os.path.join(input_root_rad, 'rrtmg_lw.nc')
ecrad_data = os.path.join(input_root_rad, 'ecrad_data')

# Mapping
input_root_mapping = os.path.join(input_root, 'mapping')
map_file_ana = os.path.join(input_root_mapping, 'map_file.ana')

# File names -----------------------------------------------------------------
latbc_prefix = 'efsf'
latbc_nameformat = '<ddhhmmss>'
latbc_suffix = '_lbc.nc'
latbc_filename = latbc_prefix + latbc_nameformat + latbc_suffix
inidata_filename = 'laf2021111812.nc'

output_filename = "exclaim"
filename_format = "<output_filename>_DOM<physdom>_<ddhhmmss>"

# SIMULATION =================================================================
# ICON -----------------------------------------------------------------------
# Executable
icon_bin = os.path.join(exe_dir, "icon")

# Namelists and slurm runscript templates
icon_runjob = os.path.join(case_dir, 'icon_runjob.cfg')

# POST-PROCESSING ============================================================
# REDUCE_OUTPUT --------------------------------------------------------------
convert_gas = False
output_levels = 20

# POST_ICON -----------------------------------------------------------------
# Root directory where the output of the chain is copied to
output_root = os.path.join("/store/g142/", user, "icon_output", casename)

# VERIFY_CHAIN ---------------------------------------------------------------
reference_dir = os.path.join(input_root, "reference_output")

# If the output file that gets compared to the reference is not at the location
# that post_icon copied it to, give the path to it here. Else leave it 'None'
output_dir = None

# variables_to_check is a dict() with a tuple() of filenames as key and a list
# of variables-names as value. The tuple consists of the filenames of the two
# files to check, the list contains the variable-names that are compared.
# The verify_chain job will look for the files in the reference_dir (first tuple
# element) and the ouput_dir (second tuple element)
values_to_check = {
    ("icon-pgi-20.1.1-cpu-20210215-NWP_LAM_DOM01_01000000.nc", "NWP_LAM_DOM01_01000000.nc"):
    [
        'temp',
        'pres',
        'u',
        'v',
        'w',
    ]
}
