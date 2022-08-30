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

# -- Root directory of the working space of the chain
WORK_DIR = os.path.join(CHAIN_SRC_DIR, 'work')

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
DYNAMICS_GRID_FILENAME = os.path.join(INPUT_ROOT_GRID, "iconR2B04-DOM01.nc")
RADIATION_GRID_FILENAME = os.path.join(INPUT_ROOT_GRID, "iconR2B03-DOM01_R.nc")
EXTPAR_FILENAME = os.path.join(INPUT_ROOT_GRID, "extpar_iconR2B04-DOM01.nc")

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

# -- Executable
ICON_BIN = os.path.join('/scratch/snx3000/jthanwer/icon/', 'bin', 'icon')

# -- Paths for namelists and slurm runscript templates
ICON_RUNJOB = os.path.join(CASE_DIR, 'icon_runjob.cfg')
ICON_INIJOB = os.path.join(CASE_DIR, 'icon_era5_inicond.sh')

# -- Number of hours simulated by one job / directory 
RESTART_STEP = 3     # -- hours

# -- Number of hours of spin-up before each restart
SPINUP_TIME = 1      # -- hours

# -- Walltimes and domain decomposition
if COMPUTE_QUEUE == "normal":
    ICON_WALLTIME = "01:00:00"
    ICON_NP_TOT = 2

elif COMPUTE_QUEUE == "debug":
    ICON_WALLTIME = "00:30:00"
    ICON_NP_TOT = 1

else:
    logging.error("Unknown queue name: %s" % COMPUTE_QUEUE)
    sys.exit(1)

# -----------------------------------------------------------
# POST-PROCESSING
# -----------------------------------------------------------

# -- REDUCE_OUTPUT
CONVERT_GAS = True
OUTPUT_LEVELS = 20

# -- Root directory where the output of the chain is copied to
OUTPUT_ROOT = os.path.join(CHAIN_SRC_DIR, "output", CASENAME)

# -- VERIFY_CHAIN
REFERENCE_DIR = os.path.join(INPUT_ROOT, "reference_output")

# If the output file that gets compared to the reference is not at the location
# that post_icon copied it to, give the path to it here. Else leave it 'None'
#output_dir = None
OUTPUT_DIR = os.path.join(WORK_DIR, CASENAME, '2018010100_0_24', 'icon', 'output')

