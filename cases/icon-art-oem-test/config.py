import os

"""
Configuration file for the 'icon-art-test' case with ICON-ART
"""

# GENERAL SETTINGS =========================================================== 
user = os.environ['USER']
target = 'icon-art-oem'
restart_step = 24 # hours

compute_host = 'daint'
compute_queue = 'debug' # 'normal' / 'debug'
compute_account = 'em05'
constraint = 'mc' # 'mc' / 'gpu'

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

# PREPARE_DATA ---------------------------------------------------------------
input_root = '/store/empa/em05/input_iconart_processing_chain_example/'

input_root_meteo = '/store/empa/em05/input_iconart_processing_chain_example/meteo'
meteo_prefix = 'ifs_'
meteo_nameformat = meteo_prefix + '%Y%m%d%H'
meteo_suffix = '.grb'
meteo_inc = 3

input_root_icbc = os.path.join(input_root, 'icbc')
chem_prefix = 'cams_gqpe'
chem_nameformat = chem_prefix + '_%Y%m%d_%H'
chem_suffix = '.nc'

icontools_runjobs = [
    'icontools_remap_ic_runjob.cfg',
    'icontools_remap_00_lbc_runjob.cfg',
    'icontools_remap_lbc_rest_runjob.cfg',
]

# Icontools executables
#icontools_dir = exe_dir
icontools_dir = '/project/s903/mjaehn/spack-install/daint/icontools/master/cce/ldcbgsjjzq2p73xbei7ws4wce5ivzxer/bin/'
iconremap_bin = os.path.join(icontools_dir, "iconremap")
iconsub_bin   = os.path.join(icontools_dir, "iconsub")

# Input data for runscript----------------------------------------------------
# Grid
input_root_grid = os.path.join(input_root, 'grids')
radiation_grid_filename = os.path.join(input_root_grid, "testcase_DOM01.parent.nc")
dynamics_grid_filename = os.path.join(input_root_grid, "testcase_DOM01.nc")
map_file_latbc = os.path.join(input_root_grid, "map_file.latbc")
extpar_filename = os.path.join(input_root_grid, 
                               "external_parameter_icon_testcase_DOM01_tiles.nc")
lateral_boundary_grid = os.path.join(input_root_grid, "lateral_boundary.grid.nc")

input_root_rad = os.path.join(input_root, 'rad')
cldopt_filename = os.path.join(input_root_rad, 'rrtm_cldopt.nc')
lrtm_filename = os.path.join(input_root_rad, 'rrtmg_lw.nc')

input_root_mapping = os.path.join(input_root, 'mapping')
map_file_ana = os.path.join(input_root_mapping, "map_file.ana")

# File names -----------------------------------------------------------------
latbc_filename = "ifs_<y><m><d><h>_lbc.nc"
inidata_prefix = "ifs_init_"
inidata_nameformat = inidata_prefix + '%Y%m%d%H'
inidata_filename_suffix = ".nc"

output_filename = "icon-art-test"
filename_format = "<output_filename>_DOM<physdom>_<ddhhmmss>"

# ART settings----------------------------------------------------------------
input_root_tracers = os.path.join(input_root, 'XML')
chemtracer_xml_filename = os.path.join(input_root_tracers, 'tracers_oh_pntsrc.xml')
pntSrc_xml_filename = os.path.join(input_root_tracers, 'pntSrc_example.xml')
art_input_folder = os.path.join(input_root, 'ART')

# OAE ------------------------------------------------------------------------
# Online anthropogenic emissions
oae_dir = os.path.join(input_root, 'OEM')
oae_gridded_emissions_nc = 'tno_3cat.nc'
oae_vertical_profiles_nc = 'vertical_profiles.nc'
oae_hourofday_nc = 'hourofday.nc'
oae_dayofweek_nc = 'dayofweek.nc'
oae_monthofyear_nc = 'monthofyear.nc'
#oae_hourofyear_nc = 'hourofyear.nc'
oae_chem_init_nc = 'cams_gqpe_20180101_00_wet.nc'
oae_ens_reg_nc = 'regions_testcase.nc'
oae_ens_lambda_nc = 'lambdas_testcase.nc'

# SIMULATION =================================================================
# ICON -----------------------------------------------------------------------
# Executable
icon_bin = os.path.join(exe_dir, "icon-kit-art_20211018")

# Namelists and slurm runscript templates
icon_runjob = os.path.join(case_dir, 'icon_runjob.cfg')
icon_namelist_master = os.path.join(case_dir, 'icon_master.namelist.cfg')
icon_namelist_nwp = os.path.join(case_dir, 'icon_NAMELIST_NWP.cfg')

# Walltimes and domain decomposition
if compute_queue == "normal":
    icon_walltime = "00:30:00"
    icon_np_tot = 16
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
output_root = os.path.join("/scratch/snx3000", user, 
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
values_to_check = {("icon-oem-pgi-20.1.1-cpu-20210215-NWP_LAM_DOM01_01000000.nc",
                    "NWP_LAM_DOM01_01000000.nc") :
                   ['temp', 'pres', 'u', 'v', 'w', 
                    'OEM_tracer_1', 'OEM_tracer_2',
                   ]
                  }


