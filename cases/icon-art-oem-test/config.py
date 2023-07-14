import os
"""
Configuration file for the 'icon-art-oem-test' case with ICON-ART
"""

# GENERAL SETTINGS ===========================================================
user = os.environ['USER']
if user == 'jenkins':
    compute_account = 'g110'
elif os.path.exists(os.environ['HOME'] + '/.acct'):
    with open(os.environ['HOME'] + '/.acct', 'r') as file:
        compute_account = file.read().rstrip()
else:
    compute_account = os.popen("id -gn").read().splitlines()[0]
compute_host = 'daint'
compute_queue = 'normal'
constraint = 'gpu'  # 'mc'

model = 'icon-art-oem'
restart_step = 24  # hours

# Number of tasks per node
ntasks_per_node = 36 if constraint == 'mc' else 12

# Case name = pathname in cases/
casename = os.path.basename(os.path.dirname(os.path.realpath(__file__)))

# Root directory of the sourcecode of the chain (where run_chain.py is)
chain_src_dir = os.getcwd()

# Root directory of the working space of the chain
work_root = os.path.join(chain_src_dir, 'work')

# Case directory
case_dir = os.path.join(chain_src_dir, 'cases', casename)

# PRE-PROCESSING =============================================================
input_root = os.path.join(chain_src_dir, 'input', model)
# meteo
input_root_meteo = os.path.join(input_root, 'meteo')
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
    'icontools_remap_ic_chem_runjob.cfg',
    'icontools_remap_lbc_chem_runjob.cfg',
]

# Input data for runscript----------------------------------------------------
# Grid

input_files = {
    'radiation_grid_filename': ['testcase_DOM01.parent.nc', 'grid'],
    'dynamics_grid_filename': ['testcase_DOM01.nc', 'grid'],
    'map_file_latbc': ['map_file.latbc', 'grid'],
    'lateral_boundary_grid': ['lateral_boundary.grid.nc', 'grid'],
    'extpar_filename':
    ['external_parameter_icon_testcase_DOM01_tiles.nc', 'grid'],
    'cldopt_filename': ['rrtm_cldopt.nc', 'rad'],
    'lrtm_filename': ['rrtmg_lw.nc', 'rad'],
    'map_file_ana': ['map_file.ana', 'mapping'],
    'chemtracer_xml_filename': ['tracers_oh_pntsrc.xml', 'XML'],
    'pntSrc_xml_filename': ['pntSrc_example.xml', 'XML'],
    'oae_gridded_emissions_nc': ['tno_3cat.nc', 'OEM'],
    'oae_vertical_profiles_nc': ['vertical_profiles.nc', 'OEM'],
    'oae_hourofday_nc': ['hourofday.nc', 'OEM'],
    'oae_dayofweek_nc': ['dayofweek.nc', 'OEM'],
    'oae_monthofyear_nc': ['monthofyear.nc', 'OEM'],
}

# File names -----------------------------------------------------------------
latbc_filename = "ifs_<y><m><d><h>_lbc.nc"
inidata_prefix = "ifs_init_"
inidata_nameformat = inidata_prefix + '%Y%m%d%H'
inidata_filename_suffix = ".nc"

output_filename = "icon-art-oem-test"
filename_format = "<output_filename>_DOM<physdom>_<ddhhmmss>"

lateral_boundary_grid_order = 'lateral_boundary'

# ART settings----------------------------------------------------------------
art_input_folder = os.path.join(input_root, 'ART')

# SIMULATION =================================================================
# ICON -----------------------------------------------------------------------
# Executable
icon_bin = os.path.join(chain_src_dir, 'src', 'icon-art', 'bin', 'icon')

# eccodes
eccodes_dir = os.path.join(chain_src_dir, 'input', 'eccodes_definitions')

# Icontools executables
iconremap_bin = 'iconremap'
iconsub_bin = 'iconsub'

# Namelists and slurm runscript templates
icon_runjob = os.path.join(case_dir, 'icon_runjob.cfg')

# Walltimes and domain decomposition
if compute_queue == "normal":
    icon_walltime = "00:30:00"
    icon_np_tot = 16
elif compute_queue == "debug":
    icon_walltime = "00:30:00"
    icon_np_tot = 10

# POST-PROCESSING ============================================================
# REDUCE_OUTPUT --------------------------------------------------------------
convert_gas = True
output_levels = 20

# POST_COSMO -----------------------------------------------------------------
# Root directory where the output of the chain is copied to
output_root = os.path.join(chain_src_dir, "output", casename)

# VERIFY_CHAIN ---------------------------------------------------------------
reference_dir = os.path.join(input_root, "reference_output")

# If the output file that gets compared to the reference is not at the location
# that post_icon copied it to, give the path to it here. Else leave it 'None'
#output_dir = None
output_dir = os.path.join(work_root, casename, '2018010100_0_24', 'icon',
                          'output')

# variables_to_check is a dict() with a tuple() of filenames as key and a list
# of variables-names as value. The tuple consists of the filenames of the two
# files to check, the list contains the variable-names that are compared.
# The verify_chain job will look for the files in the reference_dir (first tuple
# element) and the ouput_dir (second tuple element)
values_to_check = {
    ("icon-oem-pgi-20.1.1-cpu-20210215-NWP_LAM_DOM01_01000000.nc", "NWP_LAM_DOM01_01000000.nc"):
    [
        'temp',
        'pres',
        'u',
        'v',
        'w',
        'OEM_tracer_1',
        'OEM_tracer_2',
    ]
}
