import xarray as xr
import numpy as np
import subprocess

import xarray as xr
import numpy as np
import subprocess


def create_lambda_regions(input_grid, output_path, lambdas_ids):
    """
    Create a spatial map of lambda regions and save to NetCDF.
    """
    ds = xr.open_dataset(input_grid)
    ncells = ds.cell.size
    nregs = ncells  # Set nregs to match grid cells; modify as needed
    categories = np.arange(1, len(lambdas_ids) + 1)
    regions = np.arange(nregs)
    cells = np.arange(ncells) + 1

    # Create dataset
    ds_cells = xr.Dataset(data_vars={
        'REG': (['cell'], regions),
        'Lambda_indicies': (['cat'], lambdas_ids)
    },
                          coords={
                              'cell': (['cell'], cells),
                              'cat': (['cat'], categories)
                          },
                          attrs={'author': 'Processing Chain'})

    try:
        ds_cells.to_netcdf(output_path,
                           encoding={
                               'REG': {
                                   'dtype': 'int32'
                               },
                               'cell': {
                                   'dtype': 'int32'
                               }
                           })
    except:
        print("File currently open. Please close the file and try again.")
    print(f"Lambda regions saved to {output_path}")
    return nregs, categories[-1]


def create_prior_all_ones(output_path,
                          nensembles,
                          ncats,
                          nregs,
                          propagate_bg=False):
    """
    Create a dataset of initial lambdas (all ones) for testing.
    """
    nensembles = nensembles + 1 if propagate_bg else nensembles
    arr = np.ones((nensembles, nregs, ncats, 1), dtype=np.float32)
    arr[-1, :, :, :] = 0 if propagate_bg else 1
    data = xr.DataArray(arr, dims=['ens', 'reg', 'cat', 'tracer'])
    ds = xr.Dataset({'lambda': data})
    try:
        ds.to_netcdf(output_path)
    except:
        print("File currently open. Please close the file and try again.")
    print(f"Prior all ones saved to {output_path}")


def create_prior_all_zeros(output_path, nensembles, ncats, nregs):
    """
    Create a dataset of initial lambdas (all zeros) for testing.
    """
    arr = np.zeros((nensembles, nregs, ncats, 1), dtype=np.float32)
    data = xr.DataArray(arr, dims=['ens', 'reg', 'cat', 'tracer'])
    ds = xr.Dataset({'lambda': data})
    try:
        ds.to_netcdf(output_path)
    except:
        print("File currently open. Please close the file and try again.")
    print(f"Prior all zeros saved to {output_path}")


def create_boundary_regions(grid_filename, output_path, n_bg_ens, cdo_nco_cmd,
                            cdo_nco_cmd_post):
    """
    Create boundary region masks based on geographical quadrants and save to NetCDF.
    """
    cmd = f"""
{cdo_nco_cmd}
cat > NAMELIST_ICONSUB << EOF_1
&iconsub_nml
    grid_filename = '{grid_filename}',
    output_type = 4,
    lwrite_grid = .TRUE.,
/
&subarea_nml
    ORDER = "outgrid",
    grf_info_file = '{grid_filename}',
    min_refin_c_ctrl = 1,
    max_refin_c_ctrl = 120
/
EOF_1

iconsub --nml NAMELIST_ICONSUB
{cdo_nco_cmd_post}
    """
    subprocess.check_output(cmd, shell=True)

    ds_grid = xr.open_dataset('outgrid.grid.nc')
    clon, clat = np.rad2deg(ds_grid['clon']), np.rad2deg(ds_grid['clat'])

    # Compute the central reference point
    mid_lon, mid_lat = np.nanquantile(clon, 0.5), np.nanquantile(clat, 0.5)

    # Center coordinates relative to the midpoint
    clon_cent, clat_cent = clon - mid_lon, clat - mid_lat

    # Compute angles of all points relative to the center
    angles = np.arctan2(clat_cent, clon_cent)  # Range: [-π, π]

    # Set number of regions
    sector_size = (2 * np.pi) / n_bg_ens  # Each sector covers an angle range

    # Assign each point to a region (0 to N-1)
    region_indices = (angles // sector_size).astype(int)  # Range: [-π, π]

    # One-hot encode the region assignments
    boundary_regions = np.zeros((len(clon), n_bg_ens), dtype=np.int32)
    boundary_regions[np.arange(len(clon)), region_indices] = 1

    ds_boundary = xr.Dataset(data_vars={
        'boundaryregion': (['cell', 'reg'], boundary_regions),
        'global_cell_idx': (['cell'], np.arange(len(clon)))
    },
                             coords={
                                 'cell': (['cell'], np.arange(len(clon))),
                                 'reg': (['reg'], np.arange(n_bg_ens))
                             },
                             attrs={
                                 'author': 'Erik Koene',
                                 'email': 'erik.koene@empa.ch'
                             })
    try:
        ds_boundary.to_netcdf(output_path)
    except:
        print("File currently open. Please close the file and try again.")
    print(f"Boundary regions saved to {output_path}")


def create_boundary_prior_all_ones(output_path,
                                   n_bg_ens,
                                   nensembles,
                                   propagate_bg=False):
    """
    Create boundary lambdas dataset and save to NetCDF.
    """
    nensembles = nensembles + 1 if propagate_bg else nensembles
    lambdas = np.ones((nensembles, n_bg_ens), dtype=np.float32)
    ds_lambdas = xr.Dataset(data_vars={'lambda': (['ens', 'reg'], lambdas)},
                            coords={
                                'ens': (['ens'], np.arange(nensembles)),
                                'reg': (['reg'], np.arange(n_bg_ens))
                            },
                            attrs={
                                'author': 'Erik Koene',
                                'email': 'erik.koene@empa.ch'
                            })
    try:
        ds_lambdas.to_netcdf(output_path)
    except:
        print("File currently open. Please close the file and try again.")
    print(f"Boundary lambdas saved to {output_path}")


def create_boundary_prior_separate(output_path, n_bg_ens):
    """
    Create boundary lambdas dataset and save to NetCDF.
    """
    lambdas = np.identity(n_bg_ens, dtype=np.float32)
    ds_lambdas = xr.Dataset(data_vars={'lambda': (['ens', 'reg'], lambdas)},
                            coords={
                                'ens': (['ens'], np.arange(n_bg_ens)),
                                'reg': (['reg'], np.arange(n_bg_ens))
                            },
                            attrs={
                                'author': 'Erik Koene',
                                'email': 'erik.koene@empa.ch'
                            })
    try:
        ds_lambdas.to_netcdf(output_path)
    except:
        print("File currently open. Please close the file and try again.")
    print(f"Boundary-separated lambdas saved to {output_path}")


# Example usage
# lambdas_ids = np.array([1]*8+[1]*8+[1]*15)
# nensembles=180
# nregs, ncats = create_lambda_regions('/users/ekoene/CTDAS_inputs/icon_europe_DOM01.nc', '/scratch/snx3000/ekoene/lambdaregions.nc', lambdas_ids)
# create_prior_all_ones('/scratch/snx3000/ekoene/prior_all_ones.nc', nensembles=nensembles, ncats=lambdas_ids.max(), nregs=nregs)
# create_boundary_regions('/users/ekoene/CTDAS_inputs/icon_europe_DOM01.nc', '/scratch/snx3000/ekoene/boundary_mask_bg.nc')
# create_boundary_prior_all_ones('/scratch/snx3000/ekoene/boundary_lambdas_bg.nc', nensembles=nensembles)
