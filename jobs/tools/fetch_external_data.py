import os
import shutil
import zipfile
import logging
from pathlib import Path

import cdsapi
import numpy as np
import pandas as pd
import xarray as xr
import gcsfs


def fetch_era5(date, dir2move):
    """Fetch ERA5 data from ECMWF for initial conditions

    Parameters
    ----------
    date : initial date to fetch

    """

    c = cdsapi.Client()

    # -- CRWC : Specific rain water content              - 75
    # -- CSWC : Specific snow water content              - 76
    # -- T    : Temperature                             - 130
    # -- U    : U component of wind                     - 131
    # -- V    : V component of wind                     - 132
    # -- Q    : Specific humidity                       - 133
    # -- W    : Vertical velocity                       - 135
    # -- CLWC : Specific cloud liquid water content     - 246
    # -- CIWC : Specific cloud ice water content        - 247

    c.retrieve(
        'reanalysis-era5-complete', {
            'class': 'ea',
            'date': date.strftime('%Y-%m-%d'),
            'time': date.strftime('%H:%M:%S'),
            'expver': '1',
            'levelist':
            '1/2/3/4/5/6/7/8/9/10/11/12/13/14/15/16/17/18/19/20/21/22/23/24/25/26/27/28/29/30/31/32/33/34/35/36/37/38/39/40/41/42/43/44/45/46/47/48/49/50/51/52/53/54/55/56/57/58/59/60/61/62/63/64/65/66/67/68/69/70/71/72/73/74/75/76/77/78/79/80/81/82/83/84/85/86/87/88/89/90/91/92/93/94/95/96/97/98/99/100/101/102/103/104/105/106/107/108/109/110/111/112/113/114/115/116/117/118/119/120/121/122/123/124/125/126/127/128/129/130/131/132/133/134/135/136/137',
            'levtype': 'ml',
            'param': '75/76/130/131/132/133/135/246/247',
            'stream': 'oper',
            'type': 'an',
            'grid': '1.0/1.0',
        }, 'era5_ml.grib')

    # -- CI   : Sea Ice Cover                   - 31
    # -- ASN  : Snow albedo                     - 32
    # -- RSN  : Snow density                    - 33
    # -- SST  : Sea Surface Temperature         - 34
    # -- SWV1 : Volumetric soil water layer 1   - 39
    # -- SWV2 : Volumetric soil water layer 2   - 40
    # -- SWV3 : Volumetric soil water layer 3   - 41
    # -- SWV4 : Volumetric soil water layer 4   - 42
    # -- SLT  : Soil type                       - 43
    # -- Z    : Geopotential                   - 129
    # -- SP   : Surface pressure               - 134
    # -- STL1 : Soil temperature level 1       - 139
    # -- SD   : Snow depth                     - 141
    # -- STL2 : Soil temperature level 2       - 170
    # -- LSM  : Land-Sea Mask                  - 172
    # -- STL3 : Soil temperature level 3       - 183
    # -- SRC  : Skin reservoir content         - 198
    # -- SKT  : Skin Temperature               - 235
    # -- STL4 : Soil temperature level 4       - 236
    # -- TSN  : Temperature of snow layer      - 238

    c.retrieve(
        'reanalysis-era5-single-levels', {
            'product_type': 'reanalysis',
            'param':
            '31/32/33/34/39/40/41/42/43/129/134/139/141/170/172/183/198/235/236/238',
            'date': date.strftime('%Y-%m-%d'),
            'time': date.strftime('%H:%M:%S'),
            'grid': '1.0/1.0',
        }, 'era5_surf.grib')

    shutil.move('era5_ml.grib', os.path.join(dir2move, 'era5_ml.grib'))
    shutil.move('era5_surf.grib', os.path.join(dir2move, 'era5_surf.grib'))


def fetch_era5_nudging(date, dir2move):
    """Fetch ERA5 data from ECMWF for global nudging

    Parameters
    ----------
    date : initial date to fetch

    """

    c = cdsapi.Client()

    c.retrieve(
        'reanalysis-era5-complete', {
            'class': 'ea',
            'date': date.strftime('%Y-%m-%d'),
            'time': date.strftime('%H:%M:%S'),
            'expver': '1',
            'levelist':
            '1/2/3/4/5/6/7/8/9/10/11/12/13/14/15/16/17/18/19/20/21/22/23/24/25/26/27/28/29/30/31/32/33/34/35/36/37/38/39/40/41/42/43/44/45/46/47/48/49/50/51/52/53/54/55/56/57/58/59/60/61/62/63/64/65/66/67/68/69/70/71/72/73/74/75/76/77/78/79/80/81/82/83/84/85/86/87/88/89/90/91/92/93/94/95/96/97/98/99/100/101/102/103/104/105/106/107/108/109/110/111/112/113/114/115/116/117/118/119/120/121/122/123/124/125/126/127/128/129/130/131/132/133/134/135/136/137',
            'levtype': 'ml',
            'param': '75/76/130/131/132/133/135/246/247',
            'stream': 'oper',
            'type': 'an',
            'grid': '1.0/1.0',
        }, 'era5_ml_nudging.grib')

    c.retrieve(
        'reanalysis-era5-single-levels', {
            'product_type': 'reanalysis',
            'param': '129/134',
            'date': date.strftime('%Y-%m-%d'),
            'time': date.strftime('%H:%M:%S'),
            'grid': '1.0/1.0',
        }, 'era5_surf_nudging.grib')

    shutil.move('era5_ml_nudging.grib',
                os.path.join(dir2move, 'era5_ml_nudging.grib'))
    shutil.move('era5_surf_nudging.grib',
                os.path.join(dir2move, 'era5_surf_nudging.grib'))


# -----------------------------------------------------------------------------
# ERA5 via the public Google ARCO Zarr store, and CAMS restricted to the
# months actually needed. Both avoid CDS/ADS request queueing entirely (ARCO
# needs no credentials at all; CAMS still uses ~/.cdsapirc but only ever asks
# for the month(s) a chunk covers), so they're fast enough to run on the
# login node instead of through SLURM.
# -----------------------------------------------------------------------------

# -- Properties of IFS soil types (see Table 1, ERA5 data documentation:
# -- https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation)
# -- index 0 = no soil / water, unused
#                          Soil type    1      2      3      4      5      6      7
_ERA5_SOIL_WILTINGP = np.array(
    [np.nan, 0.059, 0.151, 0.133, 0.279, 0.335, 0.267, 0.151])
_ERA5_SOIL_FIELDCAP = np.array(
    [np.nan, 0.244, 0.347, 0.383, 0.448, 0.541, 0.663, 0.347])

_ERA5_SURFACE_VARS = [
    "snow_albedo",
    "sea_ice_cover",
    "geopotential_at_surface",
    "land_sea_mask",
    "surface_pressure",
    "snow_density",
    "skin_temperature",
    "soil_type",
    "volumetric_soil_water_layer_1",
    "volumetric_soil_water_layer_2",
    "volumetric_soil_water_layer_3",
    "volumetric_soil_water_layer_4",
    "sea_surface_temperature",
    "soil_temperature_level_1",
    "soil_temperature_level_2",
    "soil_temperature_level_3",
    "soil_temperature_level_4",
    "temperature_of_snow_layer",
    "skin_reservoir_content",
    "snow_depth",
]
_ERA5_MODELLEVEL_VARS = [
    "specific_cloud_ice_water_content",
    "specific_cloud_liquid_water_content",
    "specific_humidity",
    "specific_rain_water_content",
    "specific_snow_water_content",
    "temperature",
    "u_component_of_wind",
    "v_component_of_wind",
    "vertical_velocity",
    "hybrid",
]
_ERA5_RENAME_MAP = {
    "hybrid": "lev",
    "land_sea_mask": "LSM",
    "sea_surface_temperature": "SST",
    "sea_ice_cover": "CI",
    "skin_temperature": "SKT",
    "soil_temperature_level_1": "STL1",
    "soil_temperature_level_2": "STL2",
    "soil_temperature_level_3": "STL3",
    "soil_temperature_level_4": "STL4",
    "soil_type": "SLT",
    "snow_albedo": "ALB_SNOW",
    "snow_density": "RHO_SNOW",
    "snow_depth": "W_SNOW",
    "temperature_of_snow_layer": "T_SNOW",
    "skin_reservoir_content": "W_I",
    "surface_pressure": "PS",
    "volumetric_soil_water_layer_1": "SMIL1",
    "volumetric_soil_water_layer_2": "SMIL2",
    "volumetric_soil_water_layer_3": "SMIL3",
    "volumetric_soil_water_layer_4": "SMIL4",
    "geopotential_at_surface": "GEOP_SFC",
    "specific_cloud_ice_water_content": "QI",
    "specific_cloud_liquid_water_content": "QC",
    "specific_humidity": "QV",
    "specific_rain_water_content": "QR",
    "specific_snow_water_content": "QS",
    "temperature": "T",
    "u_component_of_wind": "U",
    "v_component_of_wind": "V",
    "vertical_velocity": "W",
}
# -- Soil-depth coordinates to attach to STL1..4/SMIL1..4: mid-depth [cm], bounds [cm], source vars
_ERA5_DEPTH_INFO = {
    "depth": (0.0, [0.0, 7.0], ["STL1", "SMIL1"]),
    "depth_2": (7.0, [7.0, 28.0], ["STL2", "SMIL2"]),
    "depth_3": (28.0, [28.0, 100.0], ["STL3", "SMIL3"]),
    "depth_4": (100.0, [100.0, 289.0], ["STL4", "SMIL4"]),
}
_ERA5_BNDS = [0, 1]
_ERA5_NHYM = np.arange(137)
_ERA5_NHYI = np.arange(138)
_ERA5_HYBRID_ATTRS = {
    "hyai": {
        "long_name": "hybrid A coefficient at layer interfaces",
        "units": "Pa"
    },
    "hyam": {
        "long_name": "hybrid A coefficient at layer midpoints",
        "units": "Pa"
    },
    "hybi": {
        "long_name": "hybrid B coefficient at layer interfaces",
        "units": "1"
    },
    "hybm": {
        "long_name": "hybrid B coefficient at layer midpoints",
        "units": "1"
    },
    "lev": {
        "standard_name": "hybrid_sigma_pressure",
        "long_name": "hybrid level at layer midpoints",
        "formula": "hyam hybm (mlev=hyam+hybm*aps)",
        "formula_terms": "ap: hyam b: hybm ps: aps",
        "units": "level",
        "positive": "down",
    },
}


def _era5_swvl_to_smi(ds,
                      levels=(1, 2, 3, 4),
                      var_prefix="SMIL",
                      soiltype_var="SLT"):
    """Convert volumetric soil water content (swvl1..4) to a soil moisture
    index, using per-soil-type wilting point / field capacity, in place."""
    slt = ds[soiltype_var].astype(int)
    wp = xr.DataArray(_ERA5_SOIL_WILTINGP[slt.values],
                      dims=slt.dims,
                      coords=slt.coords)
    fc = xr.DataArray(_ERA5_SOIL_FIELDCAP[slt.values],
                      dims=slt.dims,
                      coords=slt.coords)

    for ilev in levels:
        var = f"{var_prefix}{ilev}"
        smi = (ds[var] - wp) / (fc - wp)
        ds[var] = smi.where(slt > 0, 0.0)  # soil_type==0 (water) -> 0
    return ds


def _era5_hybrid_coeffs():
    """Fetch the L137 model-level hyai/hybi coefficients (invariant across
    timesteps, so this only needs to be looked up once per fetch call)."""
    url = "https://confluence.ecmwf.int/spaces/UDOC/pages/108117123/L137+model+level+definitions"
    for table in pd.read_html(url):
        if "a [Pa]" in table.columns and "b" in table.columns:
            a = table["a [Pa]"].to_numpy()
            b = table["b"].to_numpy()
            hyai, hybi = a, b  # length 138, already in Pa
            hyam = 0.5 * (hyai[:-1] + hyai[1:])  # length 137
            hybm = 0.5 * (hybi[:-1] + hybi[1:])  # length 137
            return hyai, hybi, hyam, hybm
    raise RuntimeError("Could not find the L137 a/b coefficient table")


def icon_grid_bounds(grid_file, buffer_deg=0.5):
    """Lat/lon bounding box of an ICON grid, in degrees.

    Uses the cell vertices (``vlat``/``vlon``, stored in radians), so the box
    covers every cell completely, and widens it by `buffer_deg` on each side
    so that remapping onto the ICON grid has source data beyond its edges.

    Parameters
    ----------
    grid_file : Path
        ICON grid file.
    buffer_deg : float, optional
        Margin added on each side, in degrees. The default (0.5) is two ERA5
        grid cells.

    Returns
    -------
    tuple of float
        (latmin, latmax, lonmin, lonmax) in degrees, latitudes clipped to
        [-90, 90].
    """
    with xr.open_dataset(grid_file) as grid:
        vlat = np.degrees(grid['vlat'].values)
        vlon = np.degrees(grid['vlon'].values)
    return (max(float(vlat.min()) - buffer_deg,
                -90.), min(float(vlat.max()) + buffer_deg, 90.),
            float(vlon.min()) - buffer_deg, float(vlon.max()) + buffer_deg)


def fetch_era5_arco(times, out_dir, area):
    """Fetch ERA5 model-level and surface fields from the public Google ARCO
    (Analysis-Ready, Cloud-Optimized) Zarr store, for each timestamp in
    `times`, writing one already-renamed ``ERA5_<YYYYMMDDHH>.nc`` per
    timestep to `out_dir`. Needs no CDS/ADS credentials at all. Skips
    timestamps whose output file already exists.

    Parameters
    ----------
    times : iterable of datetime
        Timestamps to fetch (ARCO holds hourly data; request only the ones
        actually needed, e.g. every `meteo_nudging_step` hours).
    out_dir : Path
        Directory to write the per-timestep NetCDF files to.
    area : tuple of float
        (latmin, latmax, lonmin, lonmax) bounding box to crop to, in
        degrees, e.g. from :func:`icon_grid_bounds`.
    """
    out_dir = Path(out_dir)
    times = [
        t for t in times
        if not (out_dir / f"ERA5_{t.strftime('%Y%m%d%H')}.nc").exists()
    ]
    if not times:
        logging.info("All requested ERA5 timesteps already fetched")
        return

    latmin, latmax, lonmin, lonmax = area

    fs = gcsfs.GCSFileSystem(token="anon", cache_timeout=3600)
    ar_model = xr.open_zarr(fs.get_mapper(
        "gs://gcp-public-data-arco-era5/ar/model-level-1h-0p25deg.zarr-v1"),
                            consolidated=True)
    ar_surface = xr.open_zarr(fs.get_mapper(
        "gs://gcp-public-data-arco-era5/ar/full_37-1h-0p25deg-chunk-1.zarr-v3"
    ),
                              consolidated=True)

    ar_model = ar_model.assign_coords(
        longitude=((ar_model.longitude + 180) % 360) - 180).sortby("longitude")
    ar_surface = ar_surface.assign_coords(
        longitude=((ar_surface.longitude + 180) % 360) -
        180).sortby("longitude")

    hyai, hybi, hyam, hybm = _era5_hybrid_coeffs()

    for t in times:
        outfile = out_dir / f"ERA5_{t.strftime('%Y%m%d%H')}.nc"
        timestamp = pd.Timestamp(t).tz_localize(None)

        surf = ar_surface.sel(
            time=timestamp,
            latitude=slice(latmax, latmin),
            longitude=slice(
                lonmin,
                lonmax))[_ERA5_SURFACE_VARS].expand_dims(time=[timestamp])
        ml = ar_model.sel(
            time=timestamp,
            latitude=slice(latmax, latmin),
            longitude=slice(
                lonmin,
                lonmax))[_ERA5_MODELLEVEL_VARS].expand_dims(time=[timestamp])

        ds = xr.merge([surf, ml], compat="override").compute()
        ds = ds.rename(_ERA5_RENAME_MAP)

        # -- GEOSP/Q are ECHAM-convention aliases for GEOP_SFC/QV
        ds["GEOSP"] = ds["GEOP_SFC"]
        ds["Q"] = ds["QV"]

        ds = ds.assign_coords(
            nhym=_ERA5_NHYM,
            nhyi=_ERA5_NHYI,
            bnds=_ERA5_BNDS,
            **{
                dim: [mid]
                for dim, (mid, _, _) in _ERA5_DEPTH_INFO.items()
            },
        )
        ds["hyam"] = ("nhym", hyam)
        ds["hybm"] = ("nhym", hybm)
        ds["hyai"] = ("nhyi", hyai)
        ds["hybi"] = ("nhyi", hybi)
        for name, attrs in _ERA5_HYBRID_ATTRS.items():
            ds[name].attrs = attrs

        ds = _era5_swvl_to_smi(ds)

        # NOTE: LNPS is intentionally not computed here -- it's derived from
        # PS after remapping (LNPS = ln(remapped PS)), so it must stay
        # consistent with the PS variable in the same output file: ln() and
        # spatial remapping don't commute.

        for var in ds.data_vars:
            ds[var].attrs.pop("GRIB_pv", None)

        for dim, (mid, bounds, variables) in _ERA5_DEPTH_INFO.items():
            ds[f"{dim}_bnds"] = ((dim, "bnds"), [bounds])
            for var in variables:
                ds[var] = ds[var].expand_dims({dim: [mid]}, axis=1)
            # -- must be set *after* expand_dims: expand_dims replaces
            # -- ds[dim] with a fresh, attribute-less coordinate
            ds[dim].attrs = {
                "standard_name": "depth",
                "long_name": "depth_below_land",
                "units": "cm",
                "positive": "down",
                "axis": "Z",
                "bounds": f"{dim}_bnds",
            }

        ds.to_netcdf(outfile)
        logging.info(f"Fetched ERA5 data and saved to: {outfile}")


def is_valid_zip(filepath):
    try:
        with zipfile.ZipFile(filepath, 'r') as zf:
            return zf.testzip() is None  # None if no corruption
    except Exception:
        return False


def fetch_cams_co2_months(year_months,
                          out_dir,
                          start_date=None,
                          end_date=None,
                          tmp_dir=None):
    """Fetch CAMS CO2 concentration data via ADS, restricted to the given
    (year, month) pairs, then split into per-timestep
    ``cams_co2_<YYYYMMDDTHH>.nc`` files in `out_dir`.

    Parameters
    ----------
    year_months : iterable of (int, int)
        (year, month) pairs to request. Grouped by year into one ADS
        request per year, requesting only the needed months.
    out_dir : Path
        Directory to write the per-timestep NetCDF files to.
    start_date, end_date : datetime, optional
        If given, used only to decide whether a given month can be
        skipped entirely (every timestep in [start_date, end_date] within
        that month already on disk). Once a month is actually downloaded,
        every timestep in it is written out, not just the ones inside
        [start_date, end_date] -- ADS only supports month-granularity
        requests, so the whole month is downloaded regardless; caching all
        of it means a later call touching a different slice of the same
        month won't need to re-request it.
    tmp_dir : Path, optional
        Scratch directory for the downloaded zip/extracted files. Defaults
        to `out_dir` / 'download'.
    """
    out_dir = Path(out_dir)
    tmp_dir = Path(tmp_dir) if tmp_dir else out_dir / 'download'
    tmp_dir.mkdir(parents=True, exist_ok=True)

    dataset = "cams-global-greenhouse-gas-inversion"
    client = cdsapi.Client()

    years = {}
    for year, month in year_months:
        years.setdefault(year, set()).add(month)

    def _naive_utc(dt):
        """Strip tzinfo (assumed already UTC, matching CAMS timestamps) so
        comparisons against the tz-naive date_range grid below don't
        raise."""
        ts = pd.Timestamp(dt)
        return ts.tz_localize(None) if ts.tzinfo is not None else ts

    def _missing_timesteps(year, month):
        """3-hourly timestamps in [start_date, end_date] within this month
        that don't already have a cams_co2_<stamp>.nc output file."""
        month_start = pd.Timestamp(year=year, month=month, day=1)
        month_end = month_start + pd.offsets.MonthEnd(0) + pd.Timedelta(
            hours=21)
        grid = pd.date_range(month_start, month_end, freq='3h')
        if start_date is not None:
            grid = grid[grid >= _naive_utc(start_date)]
        if end_date is not None:
            grid = grid[grid <= _naive_utc(end_date)]
        return [
            t for t in grid
            if not (out_dir /
                    f"cams_co2_{t.strftime('%Y%m%dT%H')}.nc").exists()
        ]

    for year, months in years.items():
        # A month is skipped only if every 3-hourly timestep of its
        # [start_date, end_date] slice is already on disk. Consecutive
        # chunks each touch only some days of a month, so the presence of
        # any one file for the month is not enough.
        months = sorted(m for m in months if _missing_timesteps(year, m))
        if not months:
            logging.info(
                f"CAMS data for {year} (needed range) already fetched, skipping"
            )
            continue

        target = tmp_dir / f"{dataset}_{year}_{'-'.join(f'{m:02d}' for m in months)}.zip"
        if not target.is_file() or not is_valid_zip(target):
            request = {
                "variable": "carbon_dioxide",
                "quantity": "concentration",
                "input_observations": "surface",
                "time_aggregation": "instantaneous",
                "version": "latest",
                "year": [str(year)],
                "month": [f"{m:02d}" for m in months],
            }
            client.retrieve(dataset, request).download(target)
            logging.info(
                f"Downloaded CAMS data for {year}-{months} to {target}")
        else:
            logging.info(f"CAMS zip already downloaded: {target}")

        with zipfile.ZipFile(target) as zf:
            for member in zf.infolist():
                date_str = member.filename.split('_')[-1].split('.')[0]
                local_name = f"CAMS_{date_str}"
                local_path = tmp_dir / local_name
                if not local_path.is_file():
                    member.filename = local_name
                    zf.extract(member, tmp_dir)

                ds_cams = xr.open_dataset(local_path)
                # Write out every timestep of the downloaded month, so later
                # chunks in the same month find their data already on disk.
                for time in ds_cams.time:
                    stamp = np.datetime_as_string(
                        time.values, unit="h").replace("-",
                                                       "").replace(":", "")
                    outpath = out_dir / f"cams_co2_{stamp}.nc"
                    if not outpath.exists():
                        logging.info(f"Writing CAMS data to {outpath}")
                        ds_cams.sel(time=time,
                                    drop=True).squeeze().to_netcdf(outpath)
                ds_cams.close()

    logging.info("Finished processing CAMS data.")
