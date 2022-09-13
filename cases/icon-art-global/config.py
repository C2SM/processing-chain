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
# -- INPUT DATA
# -----------------------------------------------------------

INPUT_ROOT = '/scratch/snx3000/jthanwer/processing-chain/input/'

# -- Initial conditions and boundary conditions
INPUT_ROOT_ICBC = os.path.join(INPUT_ROOT, 'icbc')
INICOND_FILENAME = '/users/jthanwer/scripts/create-inicond-era5/era2icon-R2B04_DOM01.nc'

# -- Grid
INPUT_ROOT_GRID = os.path.join(INPUT_ROOT, 'grids')
DYNAMICS_GRID_FILENAME = os.path.join(INPUT_ROOT_GRID, "iconR2B03-DOM01.nc")
EXTPAR_FILENAME = os.path.join(INPUT_ROOT_GRID, "extpar_iconR2B03-DOM01.nc")

# -- Radiation
INPUT_ROOT_RAD = os.path.join(INPUT_ROOT, 'rad')
CLDOPT_FILENAME = os.path.join(INPUT_ROOT_RAD, 'ECHAM6_CldOptProps.nc')
LRTM_FILENAME = os.path.join(INPUT_ROOT_RAD, 'rrtmg_lw.nc')

# -- ART settings
INPUT_ROOT_TRACERS = os.path.join(INPUT_ROOT, 'XML')
CHEMTRACER_XML_FILENAME = os.path.join(INPUT_ROOT_TRACERS, 'tracer_passive.xml')
PNTSRC_XML_FILENAME = os.path.join(INPUT_ROOT_TRACERS, 'pntSrc_example.xml')
ART_INPUT_FOLDER = os.path.join(INPUT_ROOT, 'ART')

# -----------------------------------------------------------
# -- SIMULATION
# -----------------------------------------------------------

# -- Root directory of the working space of the chain
WORK_DIR = os.path.join(CHAIN_SRC_DIR, 'simu-1month-r2b3-testconfig')

# -- Executable
ICON_BIN = os.path.join('/scratch/snx3000/jthanwer/icon/', 'bin', 'icon')

# -- Paths for namelists and slurm runscript templates
ICON_RUNJOB = os.path.join(CASE_DIR, 'icon_runjob.cfg')
ICON_INIJOB = os.path.join(CASE_DIR, 'icon_era5_inicond_test.sh')

# -- Number of hours simulated by one job / directory
RESTART_STEP = 720    # -- hours

# -- Number of hours of spin-up before each restart
SPINUP_TIME = 0      # -- hours

# -- Variables for which initial conditions from previous run must be used
VARS_SPINUP_MERGE = ['tracer1', 'tracer2', 'tracer3', 'tracer4']

# -- Number of hours between two output data
OUTPUT_WRITING_STEP = 12

# -- Number of steps per output
steps_per_output = max(int(RESTART_STEP / OUTPUT_WRITING_STEP) + 1, 1)

# -- Use ERA5 data for initial conditions. Else, use prescribed initial conditions above
USE_ERA5_INICOND = False

# -- Walltimes and domain decomposition
if COMPUTE_QUEUE == "normal":
    ICON_WALLTIME = "01:00:00"
    ICON_NP_TOT = 4

elif COMPUTE_QUEUE == "debug":
    ICON_WALLTIME = "00:30:00"
    ICON_NP_TOT = 2

else:
    logging.error("Unknown queue name: %s" % COMPUTE_QUEUE)
    sys.exit(1)

# -----------------------------------------------------------
# POST-PROCESSING
# -----------------------------------------------------------
