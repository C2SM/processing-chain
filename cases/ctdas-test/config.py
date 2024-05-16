import os
"""
Configuration file for the 'icon-art-oem-vprm-test' case with ICON-ART
"""

# GENERAL SETTINGS ===========================================================
user = os.environ['USER']
compute_account = 's1152'
compute_host = 'daint'
compute_queue = 'normal'  # 'normal' / 'debug'
constraint = 'mc'  # 'mc' / 'gpu'

Init_from_ICON = False

target = 'icon-art-oem'
restart_step = 2 * 8760  # hours
restart_cycle_window = 60 * 60 * 24 * 10  #10 days in sec

if constraint == 'gpu':
    ntasks_per_node = 12
elif constraint == 'mc':
    ntasks_per_node = 36

#Path to the CTDAS root directory
ctdas_root = '/scratch/snx3000/ekoene/ctdas-icon'
Init_from_ICON = False

# case name = pathname in cases/
path = os.path.realpath(__file__)
casename = os.path.basename(os.path.dirname(path))

# Root directory of the sourcecode of the chain (where run_chain.py is)
chain_src_dir = os.path.join('/scratch/snx3000/ekoene/processing-chain')

# Root directory of the working space of the chain
work_root = os.path.join(chain_src_dir, 'work')

# Directory where executables are stored
# exe_dir = "/scratch/snx3000/ekoene/processing-chain/work/backupVPRM_EU_ERA5_22/2018010100_0_72/icon/run/" # This one works with the current setup...
# exe_dir = "/scratch/snx3000/nponomar/processing_chain_python/processing-chain/work/CTDAS_ZH/2022071400_0_504/icon/run/" # Nikolai's latest file
exe_dir = "/scratch/snx3000/nponomar/icon-vprm-try2/icon-vprm/bin/"
# exe_dir = "/scratch/snx3000/msteiner/ICON_TRANSCOM_SIMULATIONS/executables"

# Case directory
case_dir = os.path.join(chain_src_dir, 'cases', casename)

# PREPARE_DATA ---------------------------------------------------------------
input_root_meteo = '/scratch/snx3000/ekoene/ERA5'
meteo_prefix = 'era5_'
meteo_nameformat = meteo_prefix + '%Y%m%d%H'
meteo_suffix = '.nc'
meteo_inc = 3

input_root_chem = '/store/empa/em05/dbrunner/icon-art/icbc/'
input_root_icbc = input_root_chem
chem_prefix = 'cams_gqpe_'
# input_root_chem = '/scratch/snx3000/ekoene/MERGE2/'
# input_root_icbc = input_root_chem
# chem_prefix = 'CAMS_'

chem_nameformat = chem_prefix + '%Y%m%d%H'
chem_suffix = '.nc'
chem_inc = 3

icontools_runjobs = [
    # 'icontools_remap_ic_runjob.cfg',
    # 'icontools_remap_00_lbc_runjob.cfg',
    # 'icontools_remap_lbc_rest_runjob.cfg',
    # 'icontools_remap_ic_chem_runjob.cfg',
    # 'icontools_remap_lbc_chem_runjob.cfg',
]

# Icontools executables
#icontools_dir = '/project/s903/mjaehn/spack-install/daint/icontools/master/cce/ldcbgsjjzq2p73xbei7ws4wce5ivzxer/bin/'
icontools_dir = '/scratch/snx3000/nponomar/spack-install/daint/icontools/c2sm-master/gcc/hv7e5pklc6hyntvowrgkywb6rrwzdevb/bin'
#icontools_dir = '/scratch/snx3000/nponomar/icon-vprm/bin'
iconremap_bin = os.path.join(icontools_dir, "iconremap")
iconsub_bin = os.path.join(icontools_dir, "iconsub")

# Input data for runscript----------------------------------------------------
# Grid
input_root = '/store/empa/em05/input_iconart_processing_chain_example/'
input_root_grid_icon = '/users/ekoene/CTDAS_inputs/'
radiation_grid_filename = os.path.join(input_root_grid_icon,
                                       "icon_europe_DOM01.parent.nc")
dynamics_grid_filename = os.path.join(input_root_grid_icon,
                                      "icon_europe_DOM01.nc")

input_root_mapping = '/users/ekoene/CTDAS_inputs'
map_file_ana = os.path.join(input_root_mapping, "map_file.ana")
map_file_latbc = os.path.join(input_root_mapping, "map_file.latbc")
extpar_filename = os.path.join(input_root_grid_icon,
                               "icon_extpar_EriksGrid.nc")
input_root_rad = os.path.join(input_root, 'rad')
cldopt_filename = os.path.join(input_root_rad, 'rrtm_cldopt.nc')
lrtm_filename = os.path.join(input_root_rad, 'rrtmg_lw.nc')

# File names -----------------------------------------------------------------
latbc_filename = "era5_<y><m><d><h>_lbc.nc"
inidata_prefix = "era5_init_"
inidata_nameformat = inidata_prefix + '%Y%m%d%H'
inidata_filename_suffix = ".nc"

output_filename = "icon-art-test"
filename_format = "<output_filename>_DOM<physdom>_<datetime2>"

# ART settings----------------------------------------------------------------
input_root_tracers = '/users/ekoene/CTDAS_inputs/input/xml/'
chemtracer_xml_filename = os.path.join(input_root_tracers,
                                       'tracers_CoCO2ens_restart.xml')
pntSrc_xml_filename = os.path.join(input_root_tracers, 'pntSrc_example.xml')
# art_input_folder = os.path.join(input_root, 'ART')
# input_root_tracers = '/scratch/snx3000/nponomar/processing_chain_python/processing-chain/work/CTDAS/2022070200_0_672/icon/input/xml/'
# chemtracer_xml_filename = '/scratch/snx3000/nponomar/processing_chain_python/processing-chain/work/VPRM_ENSEMBLE_testing/2022070100_0_24/icon/input/xml/vprm_ensemble_co2_icos_cities_full.xml'

# pntSrc_xml_filename = os.path.join(input_root_tracers, 'vprm_ensemble_co2_icos_cities_full_restart.xml')
art_input_folder = '/scratch/snx3000/nponomar/ICON_ctdas_msteiner/ART'
# THESE TWO MUST BE AVAILABLE ON SCRATCH!
vprm_regions_synth_nc = '/scratch/snx3000/ekoene/lambdaregions.nc'
vprm_lambdas_synth_nc = '/scratch/snx3000/ekoene/prior_all_ones.nc'

# OAE ------------------------------------------------------------------------
# Online anthropogenic emissions
oae_dir = '/users/ekoene/CTDAS_inputs/'
oae_gridded_emissions_nc = 'INV_20180101.nc'
oae_vertical_profiles_nc = 'vertical_profiles_t.nc'
oae_hourofday_nc = 'hourofday.nc'
oae_dayofweek_nc = 'dayofweek.nc'
oae_monthofyear_nc = 'monthofyear.nc'
oae_hourofyear_nc = 'hourofyear8784.nc'

# VPRM ------------------------------------------------------------------------
# ICON-ART VPRM coefficients calculated using MODIS data
online_vprm_dir = '/users/ekoene/CTDAS_inputs/'
vprm_coeffs_nc = 'VPRM_indices_ICON.nc'

# CTDAS ------------------------------------------------------------------------
ctdas_restart = False
ctdas_BG_run = False
# CTDAS cycle length in days
ctdas_cycle = int(restart_cycle_window / 60 / 60 / 24)
ctdas_nlag = 2
ctdas_tracer = 'co2'
# CTDAS number of regions and cells->regions file
ctdas_nreg_params = 21184
ctdas_regionsfile = vprm_regions_synth_nc
# Number of boundaries, and boundaries mask/regions file
ctdas_bg_params = 8
ctdas_boundary_mask_file = '/scratch/snx3000/ekoene/boundary_mask_bg.nc'
# Number of ensemble members (make this consistent with your XML file!)
ctdas_optimizer_nmembers = 180
# CTDAS path
ctdas_dir = '/scratch/snx3000/ekoene/ctdas-icon/exec'
# Distance file from region to region (shape: [N_reg x N_reg]), for statevector localization
ctdas_sv_distances = '/scratch/snx3000/ekoene/CTDAS_cells2cells.nc'
# Distance file from region to stations (shape: [N_reg x N_obs]), for observation localization
ctdas_op_loc_coeffs = '/scratch/snx3000/ekoene/cells2stations.nc'
# Directory containing a file with all the observations
ctdas_datadir = '/scratch/snx3000/ekoene/ICOS_extracted/2018/'
# CTDAS localization setting
ctdas_system_localization = 'spatial'
# CTDAS statevector length for one window
ctdas_nparameters = 2 * ctdas_nreg_params + 8  # 2 (A , VPRM) * ctdas_nreg_params + ctdas_bg_params
# Extraction template
ctdas_extract_template = '/scratch/snx3000/ekoene/processing-chain/cases/VPRM_EU_ERA5_22/extract_template_icos_EU'
# ICON runscript template
ctdas_ICON_template = '/scratch/snx3000/ekoene/processing-chain/cases/VPRM_EU_ERA5_22/runscript_template_restart_icos_EU'
# Full path to SBATCH template that can submit extraction scripts
ctdas_sbatch_extract_template = '/scratch/snx3000/ekoene/processing-chain/cases/VPRM_EU_ERA5_22/sbatch_extract_template'
# Full path to possibly time-varying emissionsgrid (if not time-varying, supply a filename without {}!)
ctdas_oae_grid = "/scratch/snx3000/ekoene/inventories/INV_{}.nc"
ctdas_oae_grid_fname = '%Y%m%d'  # Specifies the naming scheme to use for the emission grids
# Spinup time length
ctdas_restart_init_time = 60 * 60 * 24  # 1 day in seconds
# Restart file for the first simulation
ctdas_first_restart_init = '/scratch/snx3000/ekoene/processing-chain/work/VPRM_EU_ERA5_22/2018010100_0_240/icon/output_INIT'
# Number of vertical levels
nvlev = 60
# NOT NEEDED FOR ANYTHING, EXCEPT TO MAKE CTDAS RUN
ctdas_obsoperator_home = '/scratch/snx3000/msteiner/ctdas_test/exec/da/rc/stilt'
ctdas_obsoperator_rc = os.path.join(ctdas_obsoperator_home, 'stilt_0.rc')
ctdas_regtype = 'olson19_oif30'

# SIMULATION =================================================================
# ICON -----------------------------------------------------------------------
# Executable
# icon_bin = os.path.join(exe_dir, "icon.exe")
icon_bin = os.path.join(exe_dir, "icon_vprm_ens_categories_hoy_bg")

# icon_bin = os.path.join(exe_dir, "20220822_icon_art_offset_nuding_radioactm2")
#icon_bin = os.path.join(exe_dir, "icon_oem_emissions")
# Namelists and slurm runscript templates
icon_runjob = os.path.join(case_dir, 'icon_runjob.cfg')
icon_namelist_master = os.path.join(case_dir, 'icon_master.namelist.cfg')
icon_namelist_nwp = os.path.join(case_dir, 'icon_NAMELIST_NWP.cfg')

# Walltimes and domain decomposition
if compute_queue == "normal":
    icon_walltime = "24:00:00"
    icon_np_tot = 32
elif compute_queue == "debug":
    icon_walltime = "00:30:00"
    icon_np_tot = 2
else:
    logging.error("Unknown queue name: %s" % compute_queue)
    sys.exit(1)
