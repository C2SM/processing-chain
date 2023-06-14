import os
import logging
import sys
"""
Configuration file for the 'icon-art-global' case with ICON-ART
"""

# -----------------------------------------------------------
# GENERAL SETTINGS
# -----------------------------------------------------------

user = os.environ['USER']
compute_host = 'daint'
compute_queue = 'debug'  # 'normal' / 'debug'
constraint = 'mc'  # 'mc' / 'gpu'
if os.path.exists(os.environ['HOME'] + '/.acct'):
    with open(os.environ['HOME'] + '/.acct', 'r') as file:
        compute_account = file.read().rstrip()
else:
    compute_account = os.popen("id -gn").read().splitlines()[0]

# -- Model to run
target = 'icon-art-global'

# -- Number of tasks per node
ntasks_per_node = 36 if constraint == 'mc' else 12

# -- case name = pathname in cases/
casename = os.path.basename(os.path.dirname(os.path.realpath(__file__)))

# -- Root directory of the sourcecode of the chain (where run_chain.py is)
chain_src_dir = os.getcwd()

# -- Case directory
case_dir = os.path.join(chain_src_dir, 'cases', casename)

# -----------------------------------------------------------
# -- SIMULATION
# -----------------------------------------------------------

# -- Root directory of the working space of the chain
work_dir = os.path.join(chain_src_dir, 'work')

# -- Executable
# icon_bin = os.path.join('/scratch/snx3000/jthanwer/spack-install/daint/icon/c2sm-master/gcc/qcndg6qwq6e5gfutwoycqmwf2m4lvg7g/', 'bin', 'icon') # -- eccodes, ocean, noart
# icon_bin = os.path.join('/scratch/snx3000/jthanwer/spack-install/daint/icon/c2sm-master/gcc/x6pisrz7umszlrpnazse3cuosdxt45kt/', 'bin', 'icon')  # -- art
# icon_bin = os.path.join('/scratch/snx3000/jthanwer/icon-online-traj/cpu/', 'bin', 'icon')  # -- online-traj, cpu+art, dev-build
icon_bin = os.path.join('/scratch/snx3000/jthanwer/icon/cpu/', 'bin',
                        'icon')  #

# -- Paths for namelists and slurm runscript templates
# icon_runjob = os.path.join(case_dir, 'icon_runjob_withoutart.cfg')
icon_runjob = os.path.join(case_dir, 'icon_runjob.cfg')
icon_era5_inijob = os.path.join(case_dir, 'icon_era5_inicond.sh')
icon_era5_nudgingjob = os.path.join(case_dir, 'icon_era5_nudging.sh')
icon_species_inijob = os.path.join(case_dir, 'icon_species_inicond.sh')
icon_species_nudgingjob = os.path.join(case_dir, 'icon_species_nudging.sh')

# -- Number of hours simulated by one job / directory
restart_step = '1MS'  # -- hours or Pandas frequency

# -- Number of hours between two output data
output_writing_step = 12  # -- TO MODIFY

# -- Initial conditios
era5_inicond = False  # -- TO MODIFY
species_inicond = True
species2restart = ['TROH']

# -- Nudging (meteorological and tracers)
era5_global_nudging = False
species_global_nudging = False
species2nudge = []
nudging_step = 12

# -- Online trajectories
online_traj = True

# -- Walltimes and domain decomposition
if compute_queue == "normal":
    icon_walltime = "00:30:00"
    icon_np_tot = 2

elif compute_queue == "debug":
    icon_walltime = "00:30:00"
    icon_np_tot = 2

else:
    logging.error("Unknown queue name: %s" % compute_queue)
    sys.exit(1)

# -----------------------------------------------------------
# -- INPUT DATA
# -----------------------------------------------------------

input_root = '/scratch/snx3000/jthanwer/DATA/ICON_INPUT/'
input_root_icbc = os.path.join(input_root, 'ICBC')
input_root_grid = os.path.join(input_root, 'GRIDS')
input_root_rad = os.path.join(input_root, 'RAD')
input_root_oem = os.path.join(input_root, 'OEM', 'SF6')
input_root_chemistry = os.path.join(input_root, 'CHEMISTRY',
                                    'OH_GCP2022_ORIGINAL')
input_root_tracers = os.path.join(input_root, 'XML/examples')
input_root_configs = os.path.join(input_root, 'CONFIGS')
input_root_art = os.path.join(input_root, 'ART')

# -- Initial conditions and boundary conditions
inicond_filename = '/scratch/snx3000/jthanwer/DATA/ICON_INPUT/ICBC/era2icon_R2B03_2022060200.nc'

# -- Grid
dynamics_grid_filename = os.path.join(input_root_grid, "iconR2B03-DOM01.nc")
extpar_filename = os.path.join(input_root_grid, "extpar_iconR2B03-DOM01.nc")

# -- Radiation
cldopt_filename = os.path.join(input_root_rad, 'ECHAM6_CldOptProps.nc')
lrtm_filename = os.path.join(input_root_rad, 'rrtmg_lw.nc')

# -- OEM
# oem_emis_filename = os.path.join(input_root_oem, 'OEM_SF6_{year}.nc')
oem_vertprof_filename = os.path.join(input_root_oem, 'vertical_profiles.nc')
oem_hourofday_filename = os.path.join(input_root_oem, 'hourofday.nc')
oem_dayofweek_filename = os.path.join(input_root_oem, 'dayofweek.nc')
oem_monthofyear_filename = os.path.join(input_root_oem, 'monthofyear.nc')

# -- Chemistry (OH)
oh_molec_filename = os.path.join(input_root_chemistry,
                                 'oh_gcp2022_icongrid.nc')  # -- TO MODIFY

# -- ART
# pntsrc_xml_filename = os.path.join(input_root_tracers, 'chemistry_lt/point-sources.xml')
# boundcond_xml_filename = os.path.join(input_root_tracers, 'boundary-conditions.xml')
# chemtracer_xml_filename = os.path.join(input_root_tracers, 'chemistry_lt/tracers.xml')

pntsrc_xml_filename = os.path.join(input_root_configs,
                                   'CONFIG2/point-sources.xml')  # -- TO MODIFY
boundcond_xml_filename = os.path.join(input_root_tracers,
                                      'boundary-conditions.xml')
chemtracer_xml_filename = os.path.join(input_root_configs,
                                       'CONFIG2/tracers.xml')  # -- TO MODIFY

# -- Nudging
map_file_nudging = os.path.join(input_root_icbc, 'map_file.nudging')

# -- Online trajectories
online_traj_filename = '/scratch/snx3000/jthanwer/DATA/ICON_INPUT/ONLINE_TRAJ/startf_traj_dom1.nc'
online_traj_table2moment = '/scratch/snx3000/jthanwer/DATA/ICON_INPUT/ONLINE_TRAJ/dmin_wetgrowth_lookup.nc'
# -----------------------------------------------------------
# -- Additional settings derived from constants
# -----------------------------------------------------------

# -- Nudge type (global or nothing)
nudge_type = 2 if era5_global_nudging else 0

# -- Time step for global nudging in seconds
nudging_step_seconds = nudging_step * 3600

# --  Prescribed initial conditions for CH4, CO and/or OH
iart_init_gas = 4 if species_inicond else 0

# -- Online trajectories
online_traj_scratch = '.TRUE.' if online_traj else '.FALSE.'
