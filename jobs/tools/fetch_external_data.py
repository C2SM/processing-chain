import os
import cdsapi
import zipfile
import logging
import xarray as xr
from icoscp.dobj import Dobj
from icoscp.sparql.runsparql import RunSparql
from icoscp_core.icos import bootstrap
from icoscp import cpauth
import numpy as np
import sys
import json
import datetime
import certifi
import urllib3
import requests
from time import sleep
from datetime import datetime, timedelta
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
from . import iter_hours, create_dir


def fetch_CDS(product, date, levels, params, resolution, area, outloc):
    # Obtain CDS authentification from file
    url_cmd = f"grep 'cds' ~/.cdsapirc"
    url = os.popen(url_cmd).read().strip().split(": ")[1]
    key_cmd = f"sed -n '/cds/ {{n;p}}' ~/.cdsapirc"
    key = os.popen(key_cmd).read().strip().split(": ")[1]
    c = cdsapi.Client(url=url, key=key)

    # Set temporal choices. ERA5 data on disk uses lists [2018-01-01, 2018-01-02, etc] while ERA5-complete uses strings with / as the separator
    if isinstance(date, datetime):
        datestr = date.strftime('%Y-%m-%d')
        timestr = date.strftime('%H:%M')
    elif isinstance(date, list):
        datestr = sorted({dt.date().strftime("%Y-%m-%d") for dt in date})
        datestr = datestr if product == 'reanalysis-era5-single-levels' else '/'.join(
            map(str, datestr))
        timestr = sorted({dt.time().strftime("%H:%M") for dt in date})
        timestr = timestr if product == 'reanalysis-era5-single-levels' else '/'.join(
            map(str, timestr))
    else:
        raise TypeError(
            f"Expected a datetime or list, but got {type(date).__name__}.")

    # Set level choices
    if isinstance(levels, str):
        levelstr = levels
    elif isinstance(levels, list):
        levelstr = '/'.join(map(str, levels))
    elif levels is None:
        pass
    else:
        raise TypeError(
            f"Expected a string or list, but got {type(levels).__name__}.")

    # Set parameters
    if isinstance(params, str):
        paramstr = params
    elif isinstance(params, list):
        paramstr = '/'.join(map(str, params))
    else:
        raise TypeError(
            f"Expected a string or list, but got {type(params).__name__}.")

    c.retrieve(
        product, {
            'date':
            datestr,
            'time':
            timestr,
            'param':
            paramstr,
            'grid':
            f'{resolution}/{resolution}',
            **({
                'area': area
            } if area is not None else {}),
            **({
                'class': 'ea',
                'type': 'an',
                'stream': 'oper',
                'levelist': levelstr,
                'levtype': 'ml',
                'expver': '1'
            } if product == 'reanalysis-era5-complete' else {}),
            **({
                'product_type': 'reanalysis'
            } if product == 'reanalysis-era5-single-levels' else {}),
        }, outloc)


def fetch_era5(date, dir2move, resolution=1.0, area=None):
    if isinstance(date, list):
        outfile_3D = dir2move / f"era5_ml_{date[0].strftime('%Y-%m-%d')}_{date[-1].strftime('%Y-%m-%d')}.grib"
    else:
        outfile_3D = dir2move / f"era5_ml_{date.strftime('%Y-%m-%d')}.grib"
    if not os.path.isfile(outfile_3D):
        # -- CRWC : Specific rain water content              - 75
        # -- CSWC : Specific snow water content              - 76
        # -- T    : Temperature                             - 130
        # -- U    : U component of wind                     - 131
        # -- V    : V component of wind                     - 132
        # -- Q    : Specific humidity                       - 133
        # -- W    : Vertical velocity                       - 135
        # -- CLWC : Specific cloud liquid water content     - 246
        # -- CIWC : Specific cloud ice water content        - 247
        fetch_CDS('reanalysis-era5-complete', date, '1/to/137',
                  [75, 76, 130, 131, 132, 133, 135, 246, 247], resolution,
                  area, outfile_3D)

    if isinstance(date, list):
        outfile_surface = dir2move / f"era5_surf_{date[0].strftime('%Y-%m-%d')}_{date[-1].strftime('%Y-%m-%d')}.grib"
    else:
        outfile_surface = dir2move / f"era5_surf_{date.strftime('%Y-%m-%d')}.grib"
    if not os.path.isfile(outfile_surface):
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
        fetch_CDS('reanalysis-era5-single-levels', date, None, [
            31, 32, 33, 34, 39, 40, 41, 42, 43, 129, 134, 139, 141, 170, 172,
            183, 198, 235, 236, 238
        ], resolution, area, outfile_surface)

    return outfile_3D, outfile_surface


def fetch_era5_nudging(date, dir2move, resolution=1.0, area=None):
    """Fetch ERA5 data from ECMWF for global nudging

    Parameters
    ----------
    date : initial date to fetch

    """
    if isinstance(date, list):
        outfile_3D = dir2move / f"era5_ml_nudging_{date[0].strftime('%Y-%m-%d')}_{date[-1].strftime('%Y-%m-%d')}.grib"
    else:
        outfile_3D = dir2move / f"era5_ml_nudging_{date.strftime('%Y-%m-%d')}.grib"
    if not os.path.isfile(outfile_3D):
        fetch_CDS('reanalysis-era5-complete', date, '1/to/137',
                  [75, 76, 130, 131, 132, 133, 135, 246, 247], resolution,
                  area, outfile_3D)

    if isinstance(date, list):
        outfile_surface = dir2move / f"era5_surf_nudging_{date[0].strftime('%Y-%m-%d')}_{date[-1].strftime('%Y-%m-%d')}.grib"
    else:
        outfile_surface = dir2move / f"era5_surf_nudging_{date.strftime('%Y-%m-%d')}.grib"
    if not os.path.isfile(outfile_surface):
        fetch_CDS('reanalysis-era5-single-levels', date, None, [129, 134],
                  resolution, area, outfile_surface)

    return outfile_3D, outfile_surface


def fetch_CAMS_CO2(start_date, end_date, dir2move):
    """Fetch CAMS CO2 data from ECMWF for initial and boundary conditions

    Parameters
    ----------
    start_date : initial date to fetch data for
    end_date   : final date to fetch data for
    dir2move   : directory to move to
    """

    # Set a temporary destionation
    tmpdir = os.path.join(os.getenv('SCRATCH'), 'CAMS_i')
    create_dir(tmpdir, 'Temporary output for CAMS data download')

    url_cmd = f"grep 'ads' ~/.cdsapirc"
    url = os.popen(url_cmd).read().strip().split(": ")[1]
    key_cmd = f"sed -n '/ads/ {{n;p}}' ~/.cdsapirc"
    key = os.popen(key_cmd).read().strip().split(": ")[1]
    c = cdsapi.Client(url=url, key=key)

    # Iterate over each year
    current_date = start_date
    while current_date.replace(tzinfo=None) <= end_date.replace(tzinfo=None):
        year = current_date.year
        start_month = current_date.month if current_date.year == start_date.year else 1
        end_month = end_date.month if current_date.year == end_date.year else 12
        months = [
            f"{month:02d}" for month in range(start_month, end_month + 1)
        ]

        # Define download file
        download = os.path.join(
            tmpdir, f'cams_GHG_{year}_{start_date.strftime("%Y%m%d")}.zip')
        if not os.path.isfile(download):
            c.retrieve(
                'cams-global-greenhouse-gas-inversion', {
                    'variable': 'carbon_dioxide',
                    'quantity': 'concentration',
                    'input_observations': 'surface',
                    'time_aggregation': 'instantaneous',
                    'version': 'latest',
                    'year': str(year),
                    'month': months,
                    'format': 'zip',
                }, download)
            logging.info(f'Downloaded CAMS data for year {year}!')
        else:
            logging.info(f'File already downloaded: {download}')

        # Unzip and process files
        with zipfile.ZipFile(download) as zf:
            for member in zf.infolist():
                date_str = member.filename.split('_')[-1].split('.')[0]
                member.filename = f"CAMS_{date_str}_{start_date.strftime('%Y%m%d')}"
                filename = os.path.join(tmpdir, member.filename)
                # Extract only files within the date range
                try:
                    if not os.path.isfile(filename):
                        zf.extract(member, tmpdir)
                except Exception as e:
                    logging.warning(f"Skipping file {member.filename}: {e}")
                # Extract individual dates
                try:
                    ds_CAMS = xr.open_dataset(filename)
                    for time in ds_CAMS.time:
                        if np.datetime64(
                                start_date) <= time.values <= np.datetime64(
                                    end_date):
                            outpath = os.path.join(
                                dir2move, 'cams_egg4_' + np.datetime_as_string(
                                    time.values, unit='h').replace(
                                        '-', '').replace(':', '') + '.nc')
                            if not os.path.isfile(outpath):
                                logging.info(f"Writing CAMS data to {outpath}")
                                ds_out = ds_CAMS.sel(time=time,
                                                     drop=True).squeeze()
                                ds_out.to_netcdf(outpath)
                except Exception as e:
                    logging.warning(f"Error processing file {filename}: {e}")

        # Move to the next year
        current_date = datetime(year + 1, 1, 1)

    logging.info("Finished processing CAMS data.")


def fetch_ICOS_data(cookie_token,
                    query_type='any',
                    start_date='01-01-2022',
                    end_date='31-12-2022',
                    save_path='',
                    species=['co', 'co2', 'ch4']):
    '''
    This script starts a SPARQL query for downloading ICOS-CP data. The query is based on searching at the ICOS-CP
    (e.g., https://data.icos-cp.eu/portal/#%7B%22filterCategories%22%3A%7B%22variable%22%3A%5B%22http%3A%2F%2Fmeta.icos-cp.eu%2Fresources%2Fcpmeta%2Fco2atcMoleFrac%22%5D%7D%2C%22filterTemporal%22%3A%7B%22df%22%3A%222017-12-31%22%2C%22dt%22%3A%222018-12-30%22%7D%7D)
    and then clicking the well-hidden SPARQL query button (situated right of "Data objects 1 to 20 of 167", consisting of an arrow.)

    cookie_token    str    cpauthToken=WzE3M....
    query_type      str    [release, growing, any] correspond to the different file products at the ICOS-CP
    start_date      str    dd-mm-yyyy
    end_date        str    dd-mm-yyyy
    save_path       str    e.g., /scratch/snx/[user]/ICOS_data/year/
    species         list   can be ['co', 'co2', 'ch4'] or any subset thereof
    '''
    meta, data = bootstrap.fromCookieToken(cookie_token)
    cpauth.init_by(data.auth)
    # --- Build up an SQL query for the different species
    qd = ""
    for specie in species:
        qd += f" <http://meta.icos-cp.eu/resources/cpmeta/atc{specie.capitalize()}"
        if query_type == 'release':
            qd += "L2DataObject>"
        elif query_type == 'growing':
            qd += "NrtGrowingDataObject>"
        elif query_type == 'any':
            qd += "Product>"

    query = '''
    prefix cpmeta: <http://meta.icos-cp.eu/ontologies/cpmeta/>
    prefix prov: <http://www.w3.org/ns/prov#>
    prefix xsd: <http://www.w3.org/2001/XMLSchema#>
    select ?dobj ?hasNextVersion ?spec ?fileName ?size ?submTime ?timeStart ?timeEnd
    where {{
        VALUES ?spec {{{0}}}
        ?dobj cpmeta:hasObjectSpec ?spec .
        BIND(EXISTS{{[] cpmeta:isNextVersionOf ?dobj}} AS ?hasNextVersion)
        ?dobj cpmeta:hasSizeInBytes ?size .
    ?dobj cpmeta:hasName ?fileName .
    ?dobj cpmeta:wasSubmittedBy/prov:endedAtTime ?submTime .
    ?dobj cpmeta:hasStartTime | (cpmeta:wasAcquiredBy / prov:startedAtTime) ?timeStart .
    ?dobj cpmeta:hasEndTime | (cpmeta:wasAcquiredBy / prov:endedAtTime) ?timeEnd .
        FILTER NOT EXISTS {{[] cpmeta:isNextVersionOf ?dobj}}
    FILTER( !(?timeStart > '{1}T23:00:00.000Z'^^xsd:dateTime || ?timeEnd < '2017-12-31T23:00:00.000Z'^^xsd:dateTime) ) 

    }}
    order by desc(?submTime)
    '''.format(qd, (datetime.strptime(start_date, '%d-%m-%Y').date() -
                    timedelta(days=1)).strftime('%Y-%m-%d'),
               (datetime.strptime(end_date,
                                  '%d-%m-%Y').date()).strftime('%Y-%m-%d'))

    # --- Run the SQL query
    result = RunSparql(query, 'pandas')
    result.run()
    result.data()

    # --- Loop over the different stations (see https://icos-carbon-portal.github.io/pylib/ for more details)
    if not os.path.exists(save_path):
        os.makedirs(save_path)

    for d in result.data()['dobj']:
        obj = Dobj(d).data

        shape = np.shape(obj)

        lon = Dobj(d).lon
        lat = Dobj(d).lat
        variables = Dobj(d).variables.to_numpy()
        Names = Dobj(d).colNames
        specie = set(Names) - set(Names).difference(species)
        meta = np.squeeze(
            [x for x in variables if set(species) - set(x) != set(species)])
        ds = xr.Dataset.from_dataframe(obj)  # This contains the data...
        # --- Cleanup of the dataframe...
        ds = ds.set_index(index='TIMESTAMP')
        ds = ds.sortby(ds.index)
        ds = ds.drop_duplicates(dim="index")
        # --- Subset to the timeframe of interest (this has no reason to fail, so you'll have to check these cases manually....)
        try:
            ds = ds.sel(index=slice(
                datetime.strptime(start_date, '%d-%m-%Y').date().strftime(
                    '%Y-%m-%d'),
                datetime.strptime(end_date, '%d-%m-%Y').date().strftime(
                    '%Y-%m-%d')))
        except:
            print('failure!')
            print(ds.index)
            print(f"Not doing {Dobj(d).station['id']}, then...?")
            break
        ds = ds.rename({'index': 'time'})
        # --- Write out further attributes
        ds.attrs['Description'] = meta[2]
        ds.attrs['Units'] = meta[1]
        ds.attrs['Station'] = Dobj(d).station['id']
        ds.attrs['Full name of the station'] = Dobj(d).station['org']['name']
        ds.attrs['Elevation above sea level'] = Dobj(d).alt
        ds.attrs['Sampling height over ground'] = Dobj(
            d).meta['specificInfo']['acquisition']['samplingHeight']
        ds.attrs['Sampling height over sea level'] = float(
            Dobj(d).meta['specificInfo']['acquisition']
            ['samplingHeight']) + float(Dobj(d).alt)
        ds.attrs['Longitude'] = Dobj(d).lon
        ds.attrs['Latitude'] = Dobj(d).lat
        ds.attrs['Name of the tracer'] = meta[0]
        name = 'ICOS_obs_' + str(specie)[2:-2] + '_' + query_type + '_' + str(
            Dobj(d).station['id']) + '_' + str(
                Dobj(d).meta['specificInfo']['acquisition']
                ['samplingHeight']) + '_' + start_date + '_' + end_date + '.nc'
        ds.to_netcdf(os.path.join(save_path, name))


def process_ICOS_data(ICOS_obs_folder,
                      start_date='01-01-2022',
                      end_date='31-12-2022',
                      output_folder='~/'):
    """Package the downloaded ICOS data into a single file

    Parameters
    ----------
    ICOS_obs_folder str    e.g., /scratch/snx/[user]/ICOS_data/year
    start_date      DateTime
    end_date        DateTime
    output_folder   str    e.g., /scratch/snx/[user]/ICOS_data/year/

    """
    # Future expected options (or retrieved from grid file); for now hardcoded
    lon_lims = [-8.3, 17.5]
    lat_lims = [40.9, 58.7]

    # Utility for converting units to PPMv
    toppm_dict = {'nmol mol-1': 1e-9 * 1e6, 'µmol mol-1': 1e-6 * 1e6}

    # Gather chosen dates
    delta = end_date - start_date
    chosen_dates = [
        np.datetime64((start_date + timedelta(
            days=i, hours=h)).strftime('%Y-%m-%dT%H:%M:%S.000000000'))
        for i in range(delta.days + 1) for h in range(24)
    ]
    number_of_hourly_measurements = len(chosen_dates)
    logging.info(
        f'A total of {number_of_hourly_measurements} hours are possible')

    # Gather files
    logging.info(
        f"Looking in folder {ICOS_obs_folder} for ICOS observation files with glob *{start_date.strftime('%d-%m-%Y')}_{end_date.strftime('%d-%m-%Y')}.nc"
    )
    files = list(
        Path(ICOS_obs_folder).glob(
            f"*{start_date.strftime('%d-%m-%Y')}_{end_date.strftime('%d-%m-%Y')}.nc"
        ))
    number_of_stations = len(files)
    logging.info(f'Will package data from {number_of_stations} files, {files}')

    # Prepare
    obs_cnc_matrix = np.zeros(
        (number_of_stations, number_of_hourly_measurements), dtype=np.float64)
    obs_dates_matrix = np.zeros(
        (number_of_stations, number_of_hourly_measurements),
        dtype=np.dtype('datetime64[ns]'))
    obs_std_matrix = np.zeros(
        (number_of_stations, number_of_hourly_measurements), dtype=np.float64)

    # Set-up a function that can be called in parallel
    def extract_obs_column(file):
        logging.info(f'Opened file {file}')
        try:
            # Open dataset and extract metadata
            ds = xr.open_dataset(file)
            name = f"{ds.attrs['Full name of the station']}_{file.name.split('_')[-3][:-2]}"
            id_st = ds.attrs['Station']
            units = ds.attrs['Units']
            masl = ds.attrs['Elevation above sea level']
            diff = (ds.time.values[1] - ds.time.values[0]
                    ) / 3600000000000  # Time difference in hours

            if diff != 1:
                logging.info(
                    f'Observation data at station {name} is not hourly averaged ({diff} hours)'
                )

            # Filter dataset to the desired time range
            ds['time'] = ds['time']
            ds_filtered = ds.sel(time=slice(start_date.replace(
                tzinfo=None), end_date.replace(tzinfo=None)))

            # Align `chosen_dates` with `ds_filtered.time`
            ds_aligned = ds_filtered.reindex(time=chosen_dates,
                                             method='nearest',
                                             tolerance='1h')

            # Update observation arrays
            obs_dates1 = ds_aligned.time.values
            obs_std1 = ds_aligned.Stdev.values * toppm_dict[units]
            obs_cnc1 = ds_aligned["co2"].values * toppm_dict[units]
            lons, lats = ds.attrs['Longitude'], ds.attrs['Latitude']

        except Exception as e:
            logging.info(f"Error processing file {file}: {e}")
            obs_cnc1 = np.full(number_of_hourly_measurements,
                               np.nan,
                               dtype=np.float64)
            obs_dates1 = np.full(number_of_hourly_measurements,
                                 np.datetime64("NaT"),
                                 dtype="datetime64[ns]")
            obs_std1 = np.full(number_of_hourly_measurements,
                               np.nan,
                               dtype=np.float64)
            name, id_st, masl, lons, lats = 'nan', 0, -999, np.nan, np.nan

        return name, obs_std1, obs_cnc1, obs_dates1, lons, lats, id_st, masl

    # Process all data concurrently
    with ThreadPoolExecutor(max_workers=1) as executor:
        results = list(executor.map(extract_obs_column, files))
    M = list(zip(*results))

    station_names = np.array(M[0])
    obs_cnc = np.array(M[2])
    obs_std = np.array(M[1])
    obs_times = np.array(M[3])
    obs_lons = np.array(M[4])
    obs_lats = np.array(M[5])
    obs_ids = np.array(M[6])
    obs_masl = np.array(M[7])

    # Initialize mask and removal list
    stations_to_keep = []
    mask_true = np.full_like(obs_cnc_matrix[0], True)

    # Filter and populate matrices
    for ix, (lon, lat, cnc, std, times) in enumerate(
            zip(obs_lons, obs_lats, obs_cnc, obs_std, obs_times)):
        if any(np.isfinite(cnc)) and (lon_lims[0] < lon < lon_lims[-1]) and (
                lat_lims[0] < lat < lat_lims[-1]):
            np.place(obs_cnc_matrix[ix], mask_true, cnc)
            np.place(obs_std_matrix[ix], mask_true, std)
            np.place(obs_dates_matrix[ix], mask_true, times)
            stations_to_keep.append(ix)

    # Convert keep list to numpy index array for slicing
    stations_to_keep = np.array(stations_to_keep)

    # Filter matrices and metadata
    obs_cnc_matrix = obs_cnc_matrix[stations_to_keep]
    obs_std_matrix = obs_std_matrix[stations_to_keep]
    obs_dates_matrix = obs_dates_matrix[stations_to_keep]
    station_names = station_names[stations_to_keep]
    obs_lons = obs_lons[stations_to_keep]
    obs_lats = obs_lats[stations_to_keep]
    obs_ids = obs_ids[stations_to_keep]
    obs_masl = obs_masl[stations_to_keep]
    station_idcs = np.arange(len(station_names))

    # Define data variables and attributes for xarray dataset
    data_vars = {
        "Concentration": (["station", "time"], obs_cnc_matrix, {
            "units": "ppm",
            "long_name": "CO2_concentration"
        }),
        "Std": (["station", "time"], obs_std_matrix, {
            "units": "ppm",
            "long_name": "CO2_concentrations_std"
        }),
        "Stations_names": (["station"], station_names, {
            "units": "-",
            "long_name": "Stations_names"
        }),
        "Stations_ids": (["station"], obs_ids, {
            "units": "-",
            "long_name": "Stations_names"
        }),
        "Stations_masl": (["station"], obs_masl, {
            "units": "-",
            "long_name": "Elevation_heights_above_sl"
        }),
        "Lon": (["station"], obs_lons, {
            "units": "degrees",
            "long_name": "Longitude"
        }),
        "Lat": (["station"], obs_lats, {
            "units": "degrees",
            "long_name": "Latitude"
        }),
        "Dates": (["station", "time"], obs_dates_matrix, {
            "long_name": "Dates"
        }),
    }

    # Define coordinates
    coords = {"station": (["station"], station_idcs)}
    attrs = {
        'creation_date': str(datetime.now()),
        'author': 'Processing Chain'
    }

    # Create xarray dataset
    ds_extracted_obs_matrix = xr.Dataset(data_vars=data_vars,
                                         coords=coords,
                                         attrs=attrs)

    # Save dataset to file
    output_filename = Path(
        output_folder
    ) / f"Extracted_{start_date.strftime('%Y%m%d')}_{end_date.strftime('%Y%m%d')}_alldates_masl.nc"
    ds_extracted_obs_matrix.to_netcdf(output_filename)

    logging.info(
        f"Finished extraction and stored obs_matrix for {len(obs_lons)} stations "
    )
    logging.info(
        f"(from {number_of_stations} available ICOS stations), which were operating "
    )
    logging.info(
        f"during the given period and are located inside the model domain, in the file: {output_filename}"
    )


def fetch_OCO2_data(starttime,
                    endtime,
                    minlon,
                    maxlon,
                    minlat,
                    maxlat,
                    output_folder,
                    product="OCO2_L2_Lite_FP_11r"):

    # Set the product (based on the list above!) and other output settings
    product = product  # Standard
    begTime = f'{starttime.strftime("%Y-%m-%d")}T00:00:00.000Z'
    endTime = f'{endtime.strftime("%Y-%m-%d")}T23:59:59.999Z'

    # Create a urllib PoolManager instance to make requests.
    http = urllib3.PoolManager(cert_reqs='CERT_REQUIRED',
                               ca_certs=certifi.where())

    # Set the URL for the GES DISC subset service endpoint
    svcurl = 'https://disc.gsfc.nasa.gov/service/subset/jsonwsp'

    # This method POSTs formatted JSON WSP requests to the GES DISC endpoint URL
    # It is created for convenience since this task will be repeated more than once
    def get_http_data(request):
        hdrs = {
            'Content-Type': 'application/json',
            'Accept': 'application/json'
        }
        data = json.dumps(request)
        r = http.request('POST', svcurl, body=data, headers=hdrs)
        response = json.loads(r.data)
        # Check for errors
        if response['type'] == 'jsonwsp/fault':
            print('API Error: faulty request')
            sys.exit(1)
        return response

    # Construct JSON WSP request for API method: subset
    subset_request = {
        'methodname': 'subset',
        'type': 'jsonwsp/request',
        'version': '1.0',
        'args': {
            'role': 'subset',
            'start': begTime,
            'end': endTime,
            'box': [minlon, minlat, maxlon, maxlat],
            'crop': False,
            'data': [{
                'datasetId': product
            }]
        }
    }

    # Submit the subset request to the GES DISC Server
    response = get_http_data(subset_request)

    # Report the JobID and initial status
    myJobId = response['result']['jobId']

    # Construct JSON WSP request for API method: GetStatus
    status_request = {
        'methodname': 'GetStatus',
        'version': '1.0',
        'type': 'jsonwsp/request',
        'args': {
            'jobId': myJobId
        }
    }

    # Check on the job status after a brief nap
    while response['result']['Status'] in ['Accepted', 'Running']:
        sleep(5)
        response = get_http_data(status_request)
        status = response['result']['Status']
        percent = response['result']['PercentCompleted']
        print('Job status: %s (%d%c complete)' % (status, percent, '%'))

    if response['result']['Status'] == 'Succeeded':
        print('Job Finished:  %s' % response['result']['message'])
    else:
        print('Job Failed: %s' % response['fault']['code'])
        sys.exit(1)

    # Construct JSON WSP request for API method: GetResult
    batchsize = 20
    results_request = {
        'methodname': 'GetResult',
        'version': '1.0',
        'type': 'jsonwsp/request',
        'args': {
            'jobId': myJobId,
            'count': batchsize,
            'startIndex': 0
        }
    }

    # Retrieve the results in JSON in multiple batches
    # Initialize variables, then submit the first GetResults request
    # Add the results from this batch to the list and increment the count
    results = []
    count = 0
    response = get_http_data(results_request)
    count = count + response['result']['itemsPerPage']
    results.extend(response['result']['items'])

    # Increment the startIndex and keep asking for more results until we have them all
    total = response['result']['totalResults']
    while count < total:
        results_request['args']['startIndex'] += batchsize
        response = get_http_data(results_request)
        count = count + response['result']['itemsPerPage']
        results.extend(response['result']['items'])

    # Check on the bookkeeping
    print('Retrieved %d out of %d expected items' % (len(results), total))

    # Sort the results into documents and URLs
    docs = []
    urls = []
    for item in results:
        try:
            if item['start'] and item['end']: urls.append(item)
        except:
            docs.append(item)

    # Print out the documentation links, but do not download them
    print('\nDocumentation:')
    for item in docs:
        print(item['label'] + ': ' + item['link'])

    # Use the requests library to submit the HTTP_Services URLs and write out the results.
    print('\nHTTP_services output:')
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)
    for item in urls:
        outfn = output_folder + '/' + item['label']
        if os.path.isfile(outfn):
            continue

        URL = item['link']
        result = requests.get(URL)
        try:
            result.raise_for_status()
            f = open(outfn, 'wb')
            f.write(result.content)
            f.close()
            print(outfn, URL)
        except:
            print('Error! Status code is %d for this URL:\n%s' %
                  (result.status.code, URL))
            print(
                'Help for downloading data is at https://disc.gsfc.nasa.gov/data-access'
            )

    print('Finished')


def process_OCO2_data(OCO2_obs_folder,
                      start_date='01-01-2022',
                      end_date='31-12-2022',
                      output_folder='~/'):
    """Package the downloaded ICOS data into a single file

    Parameters
    ----------
    OCO2_obs_folder str    e.g., /scratch/snx/[user]/OCO2_data/year
    start_date      DateTime
    end_date        DateTime
    output_folder   str    e.g., /scratch/snx/[user]/ICOS_data/year/

    """

    # # Process files
    for day in iter_hours(start_date, end_date, 24):
        # Gather files
        logging.info(
            f"Looking in folder {OCO2_obs_folder} for ICOS observation files with glob OCO2_L2_Lite*{day.strftime('%y%m%d')}*.nc4"
        )
        file = list(
            Path(OCO2_obs_folder).glob(
                f"OCO2_L2_Lite*{day.strftime('%y%m%d')}*.nc4"))
        if not file:
            continue
        elif len(file) > 0:
            IndexError("Error, more OCO-2 files exist than expected. Review.")
        else:
            logging.info(f'Will open data from {file}')

        # Open file
        s5p_data = xr.open_dataset(file[0])
        s5p_out = s5p_data[[
            "latitude", "longitude", "date", "xco2", "xco2_quality_flag",
            "xco2_averaging_kernel", "pressure_levels", "pressure_levels",
            "pressure_weight", "co2_profile_apriori", "xco2_apriori",
            "xco2_uncertainty"
        ]]
        s5p_out = s5p_out.rename({
            "levels": "layers",
            "sounding_id": "soundings",
            "xco2": "obs",
            "xco2_quality_flag": "quality_flag",
            "xco2_averaging_kernel": "averaging_kernel",
            "pressure_weight": "pressure_weighting_function",
            "co2_profile_apriori": "prior_profile",
            "xco2_apriori": "prior",
            "xco2_uncertainty": "uncertainty"
        })
        s5p_out["pressure_levels"] = s5p_out.pressure_levels[:, ::-1]
        s5p_out[
            "pressure_weighting_function"] = s5p_out.pressure_weighting_function[:, ::
                                                                                 -1]
        s5p_out["surface_pressure"] = s5p_out.pressure_levels[:, 0]

        # Process the 'time' variable: convert format, convert shape
        # pressure_levels (rename, reverse direction), pressure_weight (rename, reverse, select)
        # co2_profile_apriori (rename, reverse, select), xco2_apriori (rename, select)
        # xco2_uncertainty (rename, select)
        s5p_out = s5p_data[[
            "latitude", "longitude", "date", "xco2", "xco2_quality_flag",
            "xco2_averaging_kernel", "pressure_levels", "pressure_levels",
            "pressure_weight", "co2_profile_apriori", "xco2_apriori",
            "xco2_uncertainty"
        ]]
        s5p_out = s5p_out.rename({
            "levels": "layers",
            "sounding_id": "soundings",
            "xco2": "obs",
            "xco2_quality_flag": "quality_flag",
            "xco2_averaging_kernel": "averaging_kernel",
            "pressure_weight": "pressure_weighting_function",
            "co2_profile_apriori": "prior_profile",
            "xco2_apriori": "prior",
            "xco2_uncertainty": "uncertainty"
        })
        s5p_out["pressure_levels"] = s5p_out.pressure_levels[:, ::-1]
        s5p_out[
            "pressure_weighting_function"] = s5p_out.pressure_weighting_function[:, ::
                                                                                 -1]
        s5p_out["surface_pressure"] = s5p_out.pressure_levels[:, 0]
        s5p_out.attrs.update({
            'creation_date': str(datetime.now()),
            'author': 'Processing Chain',
            'level_def': 'pressure_boundaries',
            'retrieval_id': file[0].name
        })
        s5p_out.to_netcdf(output_folder /
                          f"OCO2_{day.strftime('%Y%m%d')}_ctdas.nc")
