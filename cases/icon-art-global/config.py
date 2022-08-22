import os
"""
Configuration file for the 'icon-art-jthanwer' case with ICON-ART
"""

# ===========================================================
# GENERAL SETTINGS
# ===========================================================
user = os.environ['USER']
if os.path.exists(os.environ['HOME'] + '/.acct'):
    with open(os.environ['HOME'] + '/.acct', 'r') as file:
        compute_account = file.read().rstrip()
else:
    compute_account = os.popen("id -gn").read().splitlines()[0]
compute_host = 'daint'
compute_queue = 'debug'  # 'normal' / 'debug'
constraint = 'mc'  # 'mc' / 'gpu'

target = 'icon-art'

# -- Number of hours simulated by one job / directory 
restart_step = 72  # hours

# -- Number of tasks per node
if constraint == 'gpu':
    ntasks_per_node = 12
elif constraint == 'mc':
    ntasks_per_node = 36

# -- case name = pathname in cases/
path = os.path.realpath(__file__)
casename = os.path.basename(os.path.dirname(path))

# -- Root directory of the sourcecode of the chain (where run_chain.py is)
chain_src_dir = os.getcwd()

# -- Root directory of the working space of the chain
work_root = os.path.join(chain_src_dir, 'work')

# -- Case directory
case_dir = os.path.join(chain_src_dir, 'cases', casename)

# ===========================================================
# -- INPUT DATA
# ===========================================================
input_root = '/scratch/snx3000/jthanwer/processing-chain/input/'

# -- Initial conditions and boundary conditions
input_root_icbc = os.path.join(input_root, 'icbc')
inicond_filename = os.path.join(input_root_icbc, "ifs2icon_R2B04_DOM01.nc")

# -- Grid
input_root_grid = os.path.join(input_root, 'grids')
dynamics_grid_filename = os.path.join(input_root_grid, "iconR2B04-DOM01.nc")
radiation_grid_filename = os.path.join(input_root_grid, "iconR2B03-DOM01_R.nc")
extpar_filename = os.path.join(input_root_grid, "extpar_iconR2B04-DOM01_tmp.nc")

# -- Radiation
input_root_rad = os.path.join(input_root, 'rad')
cldopt_filename = os.path.join(input_root_rad, 'ECHAM6_CldOptProps.nc')
lrtm_filename = os.path.join(input_root_rad, 'rrtmg_lw.nc')

# -- ART settings
input_root_tracers = os.path.join(input_root, 'XML')
chemtracer_xml_filename = os.path.join(input_root_tracers, 'tracer_passive.xml')
pntSrc_xml_filename = os.path.join(input_root_tracers, 'pntSrc_example.xml')
art_input_folder = os.path.join(input_root, 'ART')


# ===========================================================
# -- SIMULATION
# ===========================================================

# -- Executable
icon_bin = os.path.join('/scratch/snx3000/jthanwer/icon/', 'bin', 'icon')

# -- Paths for namelists and slurm runscript templates
icon_runjob = os.path.join(case_dir, 'icon_runjob.cfg')
icon_namelist_master = os.path.join(case_dir, 'icon_master.namelist.cfg')
icon_namelist_nwp = os.path.join(case_dir, 'icon_NAMELIST_NWP.cfg')

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

# ===========================================================
# POST-PROCESSING
# ===========================================================

# -- REDUCE_OUTPUT
convert_gas = True
output_levels = 20

# -- Root directory where the output of the chain is copied to
output_root = os.path.join(chain_src_dir, "output", casename)

# -- VERIFY_CHAIN
reference_dir = os.path.join(input_root, "reference_output")

# If the output file that gets compared to the reference is not at the location
# that post_icon copied it to, give the path to it here. Else leave it 'None'
#output_dir = None
output_dir = os.path.join(work_root, casename, '2018010100_0_24', 'icon', 'output')

