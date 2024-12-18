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

    ds_cells.to_netcdf(output_path,
                       encoding={
                           'REG': {
                               'dtype': 'int32'
                           },
                           'cell': {
                               'dtype': 'int32'
                           }
                       })
    print(f"Lambda regions saved to {output_path}")
    return nregs, categories[-1]


def create_prior_all_ones(output_path, nensembles, ncats, nregs):
    """
    Create a dataset of initial lambdas (all ones) for testing.
    """
    arr = np.ones((nensembles, nregs, ncats, 1), dtype=np.float32)
    data = xr.DataArray(arr, dims=['ens', 'reg', 'cat', 'tracer'])
    ds = xr.Dataset({'lambda': data})
    ds.to_netcdf(output_path)
    print(f"Prior all ones saved to {output_path}")


def create_boundary_regions(grid_filename, output_path):
    """
    Create boundary region masks based on geographical quadrants and save to NetCDF.
    """
    cmd = f"""
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

/scratch/snx3000/ekoene/spack-c2sm/spack/opt/spack/icontools-c2sm-master/gcc-9.3.0/zktezcs5cjwjsptd747zhipi53nd6phr/bin/iconsub --nml NAMELIST_ICONSUB
    """
    subprocess.check_output(cmd, shell=True)

    ds_grid = xr.open_dataset('outgrid.grid.nc')
    clon, clat = np.rad2deg(ds_grid['clon']), np.rad2deg(ds_grid['clat'])
    mid_lon, mid_lat = np.nanquantile(clon, 0.5), np.nanquantile(clat, 0.5)

    boundary_regions = np.zeros((len(clon), 8), dtype=np.int32)
    clon_cent, clat_cent = clon - mid_lon, clat - mid_lat

    for i, (lon, lat) in enumerate(zip(clon_cent, clat_cent)):
        if lon > 0 and lat > 0:
            boundary_regions[i][6 if lon > lat else 7] = 1
        elif lon < 0 and lat < 0:
            boundary_regions[i][2 if lon > lat else 3] = 1
        elif lon > 0 and lat < 0:
            boundary_regions[i][4 if lon > abs(lat) else 5] = 1
        elif lon < 0 and lat > 0:
            boundary_regions[i][0 if abs(lon) > lat else 1] = 1

    ds_boundary = xr.Dataset(data_vars={
        'boundaryregion': (['cell', 'reg'], boundary_regions),
        'global_cell_idx': (['cell'], np.arange(len(clon)))
    },
                             coords={
                                 'cell': (['cell'], np.arange(len(clon))),
                                 'reg': (['reg'], np.arange(8))
                             },
                             attrs={
                                 'author': 'Erik Koene',
                                 'email': 'erik.koene@empa.ch'
                             })
    ds_boundary.to_netcdf(output_path)
    print(f"Boundary regions saved to {output_path}")


def create_boundary_prior_all_onesll_ones(output_path, nensembles):
    """
    Create boundary lambdas dataset and save to NetCDF.
    """
    lambdas = np.ones((nensembles, 8), dtype=np.float32)
    ds_lambdas = xr.Dataset(data_vars={'lambda': (['ens', 'reg'], lambdas)},
                            coords={
                                'ens': (['ens'], np.arange(nensembles)),
                                'reg': (['reg'], np.arange(8))
                            },
                            attrs={
                                'author': 'Erik Koene',
                                'email': 'erik.koene@empa.ch'
                            })
    ds_lambdas.to_netcdf(output_path)
    print(f"Boundary lambdas saved to {output_path}")


# Example usage
# lambdas_ids = np.array([1]*8+[1]*8+[1]*15)
# nensembles=180
# nregs, ncats = create_lambda_regions('/users/ekoene/CTDAS_inputs/icon_europe_DOM01.nc', '/scratch/snx3000/ekoene/lambdaregions.nc', lambdas_ids)
# create_prior_all_ones('/scratch/snx3000/ekoene/prior_all_ones.nc', nensembles=nensembles, ncats=lambdas_ids.max(), nregs=nregs)
# create_boundary_regions('/users/ekoene/CTDAS_inputs/icon_europe_DOM01.nc', '/scratch/snx3000/ekoene/boundary_mask_bg.nc')
# create_boundary_prior_all_ones('/scratch/snx3000/ekoene/boundary_lambdas_bg.nc', nensembles=nensembles)
