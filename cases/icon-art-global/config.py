import os
import logging
import sys

"""
Configuration file for the 'icon-art' case with ICON-ART
"""

# -----------------------------------------------------------
# GENERAL SETTINGS
# -----------------------------------------------------------

USER = os.environ['USER']
COMPUTE_HOST = 'daint'
COMPUTE_QUEUE = 'debug'  # 'normal' / 'debug'
CONSTRAINT = 'mc'  # 'mc' / 'gpu'
if os.path.exists(os.environ['HOME'] + '/.acct'):
    with open(os.environ['HOME'] + '/.acct', 'r') as file:
        COMPUTE_ACCOUNT = file.read().rstrip()
else:
    COMPUTE_ACCOUNT = os.popen("id -gn").read().splitlines()[0]

# -- Model to run
TARGET = 'icon-art'

# -- Number of tasks per node
NTASKS_PER_NODE = 36 if CONSTRAINT == 'mc' else 12

# -- case name = pathname in cases/
CASENAME = os.path.basename(os.path.dirname(os.path.realpath(__file__)))

# -- Root directory of the sourcecode of the chain (where run_chain.py is)
CHAIN_SRC_DIR = os.getcwd()

# -- Case directory
CASE_DIR = os.path.join(CHAIN_SRC_DIR, 'cases', CASENAME)


# -----------------------------------------------------------
# -- SIMULATION
# -----------------------------------------------------------

# -- Root directory of the working space of the chain
WORK_DIR = os.path.join(CHAIN_SRC_DIR, 'work')

# -- Executable
# ICON_BIN = os.path.join('/scratch/snx3000/jthanwer/spack-install/daint/icon/c2sm-master/gcc/qcndg6qwq6e5gfutwoycqmwf2m4lvg7g/', 'bin', 'icon') # -- eccodes, ocean, noart
# ICON_BIN = os.path.join('/scratch/snx3000/jthanwer/spack-install/daint/icon/c2sm-master/gcc/x6pisrz7umszlrpnazse3cuosdxt45kt/', 'bin', 'icon')  # -- art
ICON_BIN = os.path.join('/scratch/snx3000/jthanwer/icon/cpu/', 'bin', 'icon')  # -- art, dev-build

# -- Paths for namelists and slurm runscript templates
ICON_RUNJOB = os.path.join(CASE_DIR, 'icon_runjob.cfg')
ICON_ERA5_INIJOB = os.path.join(CASE_DIR, 'icon_era5_inicond.sh')
ICON_ERA5_NUDGINGJOB = os.path.join(CASE_DIR, 'icon_era5_nudging.sh')
ICON_SPECIES_INIJOB = os.path.join(CASE_DIR, 'icon_species_inicond.sh')
ICON_SPECIES_NUDGINGJOB = os.path.join(CASE_DIR, 'icon_species_nudging.sh')

# -- Number of hours simulated by one job / directory
RESTART_STEP = '1MS'   # -- hours or Pandas frequency

# -- Number of hours between two output data
OUTPUT_WRITING_STEP = 12

# -- Initial conditios
ERA5_INICOND = False
SPECIES_INICOND = False
SPECIES2RESTART = []

# -- Nudging (meteorological and tracers)
ERA5_GLOBAL_NUDGING = False
SPECIES_GLOBAL_NUDGING = False
SPECIES2NUDGE = []
NUDGING_STEP = 12

# -- Walltimes and domain decomposition
if COMPUTE_QUEUE == "normal":
    ICON_WALLTIME = "00:30:00"
    ICON_NP_TOT = 2

elif COMPUTE_QUEUE == "debug":
    ICON_WALLTIME = "00:30:00"
    ICON_NP_TOT = 1

else:
    logging.error("Unknown queue name: %s" % COMPUTE_QUEUE)
    sys.exit(1)

# -----------------------------------------------------------
# -- INPUT DATA
# -----------------------------------------------------------

INPUT_ROOT = '/scratch/snx3000/jthanwer/DATA/ICON_INPUT/'
INPUT_ROOT_ICBC = os.path.join(INPUT_ROOT, 'ICBC')
INPUT_ROOT_GRID = os.path.join(INPUT_ROOT, 'GRIDS')
INPUT_ROOT_RAD = os.path.join(INPUT_ROOT, 'RAD')
INPUT_ROOT_OEM = os.path.join(INPUT_ROOT, 'OEM', 'SF6')
INPUT_ROOT_CHEMISTRY = os.path.join(INPUT_ROOT, 'CHEMISTRY', 'OH_GCP2022_ORIGINAL')
INPUT_ROOT_TRACERS = os.path.join(INPUT_ROOT, 'XML')
INPUT_ROOT_ART = os.path.join(INPUT_ROOT, 'ART')

# -- Initial conditions and boundary conditions
INICOND_FILENAME = '/scratch/snx3000/jthanwer/DATA/ICON_INPUT/ICBC/era2icon_R2B03_2022060200.nc'

# -- Grid
DYNAMICS_GRID_FILENAME = os.path.join(INPUT_ROOT_GRID, "iconR2B03-DOM01.nc")
EXTPAR_FILENAME = os.path.join(INPUT_ROOT_GRID, "extpar_iconR2B03-DOM01.nc")

# -- Radiation
CLDOPT_FILENAME = os.path.join(INPUT_ROOT_RAD, 'ECHAM6_CldOptProps.nc')
LRTM_FILENAME = os.path.join(INPUT_ROOT_RAD, 'rrtmg_lw.nc')

# -- OEM
OEM_EMIS_FILENAME = os.path.join(INPUT_ROOT_OEM, 'OEM_SF6_{year}.nc')
OEM_VERTPROF_FILENAME = os.path.join(INPUT_ROOT_OEM, 'vertical_profiles.nc')
OEM_HOUROFDAY_FILENAME = os.path.join(INPUT_ROOT_OEM, 'hourofday.nc')
OEM_DAYOFWEEK_FILENAME = os.path.join(INPUT_ROOT_OEM, 'dayofweek.nc')
OEM_MONTHOFYEAR_FILENAME = os.path.join(INPUT_ROOT_OEM, 'monthofyear.nc')

# -- Chemistry (OH)
OH_MOLEC_FILENAME = os.path.join(INPUT_ROOT_CHEMISTRY, 'oh_gcp2022_icongrid.nc')

# -- ART
PNTSRC_XML_FILENAME = os.path.join(INPUT_ROOT_TRACERS, 'point-sources.xml')
CHEMTRACER_XML_FILENAME = os.path.join(INPUT_ROOT_TRACERS, 'tracers.xml')

# -- Nudging
MAP_FILE_NUDGING = os.path.join(INPUT_ROOT_ICBC, 'map_file.nudging')

# -----------------------------------------------------------
# -- Additional settings derived from constants
# -----------------------------------------------------------

# -- Nudge type (global or nothing)
nudge_type = 2 if ERA5_GLOBAL_NUDGING else 0

# -- Time step for global nudging in seconds
nudging_step_seconds = NUDGING_STEP * 3600

# --  Prescribed initial conditions for CH4 and CO
iart_init_gas = 4 if SPECIES_INICOND else 0
