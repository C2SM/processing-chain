import os
import shutil
import cdsapi


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
