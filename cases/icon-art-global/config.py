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
ICON_CAMS_INIJOB = os.path.join(CASE_DIR, 'icon_cams_inicond.sh')
ICON_CAMS_NUDGINGJOB = os.path.join(CASE_DIR, 'icon_cams_nudging.sh')

# -- Number of hours simulated by one job / directory
RESTART_STEP = 240    # -- hours

# -- Number of hours between two output data
OUTPUT_WRITING_STEP = 1

# -- Use ERA5 data or not. If not, use prescribed initial conditions below
ERA5_INICOND = False
ERA5_GLOBAL_NUDGING = False

# -- Use CAMS data for CH4 and CO or not
CAMS_INICOND = False
CAMS_GLOBAL_NUDGING = False

# -- Nudging step for ERA5 and CAMS (hours)
NUDGING_STEP = 12

# ----------------------Deprecated ??-------------------------------------------
# -- Number of hours of spin-up before each restart
SPINUP_TIME = 0      # -- hours

# -- Variables for which initial conditions from previous run must be used
VARS_SPINUP_MERGE = ['tracer3.TL', 'tracer4.TL']
# -----------------------------------------------------------------------------

# -- Walltimes and domain decomposition
if COMPUTE_QUEUE == "normal":
    ICON_WALLTIME = "01:00:00"
    ICON_NP_TOT = 4

elif COMPUTE_QUEUE == "debug":
    ICON_WALLTIME = "00:15:00"
    ICON_NP_TOT = 2

else:
    logging.error("Unknown queue name: %s" % COMPUTE_QUEUE)
    sys.exit(1)

# -----------------------------------------------------------
# -- INPUT DATA
# -----------------------------------------------------------

INPUT_ROOT = '/scratch/snx3000/jthanwer/processing-chain/input/'

# -- Initial conditions and boundary conditions
INPUT_ROOT_ICBC = os.path.join(INPUT_ROOT, 'icbc')
INICOND_FILENAME = '/scratch/snx3000/jthanwer/processing-chain/input/icbc/era2icon_R2B03_2022060200.nc'

# -- Grid
INPUT_ROOT_GRID = os.path.join(INPUT_ROOT, 'grids')
DYNAMICS_GRID_FILENAME = os.path.join(INPUT_ROOT_GRID, "iconR2B03-DOM01.nc")
EXTPAR_FILENAME = os.path.join(INPUT_ROOT_GRID, "extpar_iconR2B03-DOM01.nc")

# -- Radiation
INPUT_ROOT_RAD = os.path.join(INPUT_ROOT, 'rad')
CLDOPT_FILENAME = os.path.join(INPUT_ROOT_RAD, 'ECHAM6_CldOptProps.nc')
LRTM_FILENAME = os.path.join(INPUT_ROOT_RAD, 'rrtmg_lw.nc')

# -- ART
INPUT_ROOT_TRACERS = os.path.join(INPUT_ROOT, 'XML')
PNTSRC_XML_FILENAME = os.path.join(INPUT_ROOT_TRACERS, 'point_sources.xml')
ART_INPUT_FOLDER = os.path.join(INPUT_ROOT, 'art')

if CAMS_INICOND and not CAMS_GLOBAL_NUDGING:
    CHEMTRACER_XML_FILENAME = os.path.join(INPUT_ROOT_TRACERS, 'tracers_inicond_only.xml')
elif CAMS_INICOND and CAMS_GLOBAL_NUDGING:
    CHEMTRACER_XML_FILENAME = os.path.join(INPUT_ROOT_TRACERS, 'tracers_nudging.xml')
else:
    CHEMTRACER_XML_FILENAME = os.path.join(INPUT_ROOT_TRACERS, 'tracers_basic.xml')

# -- Nudging
MAP_FILE_NUDGING = os.path.join(INPUT_ROOT_ICBC, 'map_file.nudging')

# -----------------------------------------------------------
# -- Additional settings derived from constants
# -----------------------------------------------------------

# -- Number of steps per output
steps_per_output = max(int(RESTART_STEP / OUTPUT_WRITING_STEP) + 1, 1)

# -- Nudge type (global or nothing)
nudge_type = 2 if ERA5_GLOBAL_NUDGING else 0

# -- Time step for global nudging in seconds
nudging_step_seconds = NUDGING_STEP * 3600

# --  Prescribed initial conditions for CH4 and CO
iart_init_gas = 4 if CAMS_GLOBAL_NUDGING else 0
