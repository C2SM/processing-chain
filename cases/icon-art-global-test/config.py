import os
"""
Configuration file for the 'icon-art-global' case with ICON-ART
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

model = 'icon-art-global'
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

# -----------------------------------------------------------
# SIMULATION
# -----------------------------------------------------------

# Executable
icon_bin = os.path.join(chain_src_dir, 'src', 'icon-art', 'bin', 'icon')

# eccodes
eccodes_dir = os.path.join(chain_src_dir, 'input', 'eccodes_definitions')

# Paths for namelists and slurm runscript templates
# icon_runjob = os.path.join(case_dir, 'icon_runjob_withoutart.cfg')
icon_runjob = os.path.join(case_dir, 'icon_runjob.cfg')
icon_era5_inijob = os.path.join(case_dir, 'icon_era5_inicond.sh')
icon_era5_nudgingjob = os.path.join(case_dir, 'icon_era5_nudging.sh')
icon_species_inijob = os.path.join(case_dir, 'icon_species_inicond.sh')
icon_species_nudgingjob = os.path.join(case_dir, 'icon_species_nudging.sh')

# Number of hours between two output data
output_writing_step = 12  # TO MODIFY

# Initial conditios
era5_inicond = False  # TO MODIFY
species_inicond = True
species2restart = ['TROH']

# Nudging (meteorological and tracers)
era5_global_nudging = False
species_global_nudging = False
species2nudge = []
nudging_step = 12

# Walltimes and domain decomposition
if compute_queue == "normal":
    icon_walltime = "00:30:00"
    icon_np_tot = 2
elif compute_queue == "debug":
    icon_walltime = "00:30:00"
    icon_np_tot = 2

# -----------------------------------------------------------
# INPUT DATA
# -----------------------------------------------------------
# ART settings-----------------------------------------------
input_root = os.path.join(chain_src_dir, 'input', model)
art_input_folder = os.path.join(input_root, 'art')

input_files = {
    'inicond_filename': ['era2icon_R2B03_2022060200.nc', 'icbc'],
    'map_file_nudging': ['map_file.nudging', 'icbc'],
    'dynamics_grid_filename': ["iconR2B03-DOM01.nc", 'grid'],
    'radiation_grid_filename': ["iconR2B03-DOM01.nc", 'grid'],
    'extpar_filename': ["extpar_iconR2B03-DOM01.nc", 'grid'],
    'cldopt_filename': ['ECHAM6_CldOptProps.nc', 'rad'],
    'lrtm_filename': ['rrtmg_lw.nc', 'rad'],
    'oh_molec_filename': ['oh_gcp2022_icongrid.nc', 'chemistry'],
    'pntSrc_xml_filename': ['point-sources.xml', 'config'],
    'chemtracer_xml_filename': ['tracers.xml', 'config'],
}

# -----------------------------------------------------------
# Additional settings derived from constants
# -----------------------------------------------------------

# Nudge type (global or nothing)
nudge_type = 2 if era5_global_nudging else 0

# Time step for global nudging in seconds
nudging_step_seconds = nudging_step * 3600

# Prescribed initial conditions for CH4, CO and/or OH
iart_init_gas = 4 if species_inicond else 0
