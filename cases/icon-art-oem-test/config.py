import os

# GENERAL SETTINGS =========================================================== 
user = os.environ['USER']
target = 'icon-oem'
restart_step = 24 # hours

compute_host = 'daint'
compute_queue = 'normal' # 'normal' / 'debug'
compute_account = 'em05'
constraint = 'mc' # 'mc' / 'gpu'

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
# cams
input_root_chem = '/store/empa/em05/dbrunner/icon-art/icbc'
chem_prefix = 'cams_gqpe'
chem_nameformat = chem_prefix + '_%Y%m%d_%H'

# ICONTools ------------------------------------------------------------------
icontools_parameter = {
    'remap_ic_runjob': 'icontools_remap_ic_runjob.cfg',
    'auxgrid_runjob': 'icontools_auxgrid_runjob.cfg',
    'remap_ana_lbc_runjob': 'icontools_remap_ana_lbc_runjob.cfg',
    'remap_fc_lbc_runjob': 'icontools_remap_fc_lbc_runjob.cfg',
    'remap_aux_runjob': 'icontools_remap_aux_runjob.cfg',
    'remap_chem_ic_runjob': 'icontools_remap_chem_ic_runjob.cfg',
    'remap_chem_lbc_runjob': 'icontools_remap_chem_lbc_runjob.cfg',
    'namelist_iconsub': 'icontools_namelist_iconsub.cfg',
    'namelist_remapfields_ic': 'icontools_namelist_remapfields_ic.cfg',
    'namelist_remapfields_ana_lbc': 'icontools_namelist_remapfields_ana_lbc.cfg',
    'namelist_remapfields_fc_lbc': 'icontools_namelist_remapfields_fc_lbc.cfg',
    'namelist_remapfields_chem_ic': 'icontools_namelist_remapfields_chem_ic.cfg',
    'namelist_remapfields_chem_lbc': 'icontools_namelist_remapfields_chem_lbc.cfg',
    'namelist_remap': 'icontools_namelist_remap.cfg',
    'namelist_remap_chem': 'icontools_namelist_remap_chem.cfg',
}

# Input data for runscript----------------------------------------------------
# Grid
input_root_grid = os.path.join(input_root, 'grid')
radiation_grid_filename = "VERIFY_DOM_DOM01.parent.nc"
dynamics_grid_filename = "VERIFY_DOM_DOM01.nc"
map_file_latbc = "map_file.latbc"
extpar_filename = "external_parameter_icon_VERIFY_DOM_DOM01_tiles.nc"
lateral_boundary_grid = "lateral_boundary.grid.nc"

# Radiation
input_root_rad = os.path.join(input_root, 'rad')
cldopt_filename = 'rrtm_cldopt.nc'
lrtm_filename = 'rrtmg_lw.nc'

# Mapping
input_root_mapping = os.path.join(input_root, 'mapping')
map_file_ana = "map_file.ana"

# File names -----------------------------------------------------------------
latbc_filename = "ifs_201801<d><h>_lbc.nc"
inidata_filename = "ifs_init_2018010100.nc"

output_filename = "NWP_LAM"
filename_format = "<output_filename>_DOM<physdom>_<ddhhmmss>"

# OAE ------------------------------------------------------------------------
# Online anthropogenic emissions
oae_dir = os.path.join(input_root, 'OEM')
oae_gridded_emissions_nc = 'emissions.nc'
oae_vertical_profiles_nc = 'vertical_profiles.nc'
oae_hourofday_nc = 'hourofday.nc'
oae_dayofweek_nc = 'dayofweek.nc'
oae_monthofyear_nc = 'monthofyear.nc'
#oae_hourofyear_nc = 'hourofyear.nc'
oae_chem_init_nc = 'cams_gqpe_20180101_00_wet.nc'
oae_ens_reg_nc = 'reg.nc'
oae_ens_lambda_nc = 'lambdas.nc'


# SIMULATION =================================================================
# ICON -----------------------------------------------------------------------
# Executable
icon_bin = os.path.join(exe_dir, "icon-oem-pgi-20.1.1-cpu-20210215")

# Icontools executables
icontools_dir = exe_dir
iconremap_bin = "iconremap"
iconsub_bin   = "iconsub"

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
    icon_np_tot = 2
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
values_to_check = {("icon-oem-pgi-20.1.1-cpu-20210215-NWP_LAM_DOM01_01000000.nc",
                    "NWP_LAM_DOM01_01000000.nc") :
                   ['temp', 'pres', 'u', 'v', 'w', 
                    'OEM_tracer_1', 'OEM_tracer_2',
                   ]
                  }


