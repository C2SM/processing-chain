import numpy as np
import xarray as xr
from sklearn.neighbors import BallTree
from scipy import argmin
import argparse


def get_horizontal_distances(longitude, latitude, icon_grid_path, k=5):
    """
    Get horizontal distances between points and their k nearest
    neighbours on the ICON grid using a quick BallTree algorithm

    Parameters
    ----------
    longitude : list or 1D np.array 
        e.g., [12] or np.array([12,13,14])

    latitude : list or 1D np.array 
        e.g., [52] or np.array([52,53,54])

    icon_grid_path : str
        Contains the path to the ICON grid

    k : int, default is 5
        Sets the number of nearest neighbours desired

    Returns
    -------
    distances: 2D np.array
        Contains the distance-on-a-sphere between the target point(s)
        and its nearest neighbours

    indices: 2D np.array
        Contains the indices to the ICON grid cells of the corresponding
        nearest neighbours
    """
    # Get ICON grid specifics
    icon_grid = xr.open_dataset(icon_grid_path)
    clon = icon_grid.clon.values
    clat = icon_grid.clat.values

    # Generate BallTree
    icon_lat_lon = np.column_stack([clat, clon])
    tree = BallTree(icon_lat_lon, metric='haversine')

    # Query BallTree
    target_lat_lon = np.column_stack(
        [np.deg2rad(latitude), np.deg2rad(longitude)])
    (distances, indices) = tree.query(target_lat_lon,
                                      k=k,
                                      return_distance=True)

    if np.any(distances == 0):
        print(
            'The longitude/latitude coincides identically with an ICON cell, which is an issue for the inverse distance weighting.'
        )
        print('I will slightly modify this value to avoid errors.')
        distances[distances == 0] = 1e-12

    if np.any(distances is np.nan):
        raise ValueError(
            'The distance between ICON and your lat/lon point could not be established...'
        )

    # NB: the 'distances' are in units of radians; i.e., it assumes the Earth is a unit sphere!
    # To get realistic distances, you need to multiply 'distances' with 6371e3 meters, i.e., the
    # radius of the earth. However, such a constant factor cancels out when we compute the
    # horizontal interpolation weights (which are normalized!), so there is no need to apply the
    # multiplication with 6371e3.

    return distances, indices


def get_nearest_vertical_distances(model_topography, model_levels,
                                   base_height_msl, inlet_height_agl,
                                   interpolation_strategy):
    """
    Get the 2 nearest distances between ICON grid points and specified
    station altitudes

    Parameters
    ----------
    model_topography : 1D np.array
        This is the elevation over mean sea level of the ICON grid
    
    model_levels : 2D np.array
        Dimensions [ICON_heights, number_of_samples]

    base_height_msl : list or 1D np.array
        e.g., [20,] or np.array([72,180,40])

    inlet_height_agl : list or 1D np.array
        e.g., [15,] or np.array([15, 21, 42])

    interpolation_strategy : list of strings
        e.g., ['ground',] or ['ground','mountain','ground']
        Can be 'ground' or 'mountain', or 'middle' (the latter is between the ground and mountain approach)
        'ground' uses the model topography + station altitude over ground
        'mountain' uses the absolute altitude over mean sea level

    Returns
    -------
    vertical_distances : 3D np.array
        Contains the absolute (!) distance between the target point(s)
        and its 2 nearest neighbour levels

    vertical_indices: 3D np.array
        Contains the indices to the ICON height levels of the corresponding 2
        nearest neighbour levels
    """
    # Get the target sampling altitude with a list comprehension
    target_altitude = [
        model_topography.isel({
            "station": i
        }).values + inlet_height_agl[i] if strategy == 'ground' else
        np.repeat(base_height_msl[i], model_topography.shape[1]) +
        inlet_height_agl[i] if strategy == 'mountain' else
        np.repeat(base_height_msl[i], model_topography.shape[1]) / 2 +
        model_topography.isel({
            "station": i
        }).values / 2 + inlet_height_agl[i]
        # if strategy=='middle'
        for (i, strategy) in enumerate(interpolation_strategy)
    ]
    target_altitude = xr.DataArray(target_altitude, dims=['station', 'ncells'])

    # Select 2 closest neighbouring levels
    first_negative = (model_levels <= target_altitude).argmax(
        dim=model_levels.dims[0])  # First index where model lies below target
    vertical_indices = np.stack(
        [first_negative, first_negative - 1],
        axis=0)  # Second index thus lies /above/ the target
    vertical_indices[:, first_negative == 0] = model_levels.values.shape[
        0] - 1  # If no result found: sample lies below lowest model level. Set it to the lowest model level

    # Sample the corresponding vertical distances between the target and the model levels
    vertical_distances = np.take_along_axis(
        (model_levels - target_altitude).values, vertical_indices, axis=0)

    return np.abs(vertical_distances).T, vertical_indices.T


def icon_to_point(longitude,
                  latitude,
                  inlet_height_agl,
                  base_height_msl,
                  icon_field_path,
                  icon_grid_path,
                  interpolation_strategy,
                  k=5,
                  field_name=None):
    """
    Function to interpolate ICON fields to point locations
    
    Parameters
    ----------
    longitude : list or 1D np.array 
        e.g., [12,] or np.array([12,13,14])

    latitude : list or 1D np.array 
        e.g., [52,] or np.array([52,53,54])

    inlet_height_agl : list or 1D np.array
        e.g., [20,] or np.array([72,180,40])
        This is the height of the *base station over mean sea level*
            (e.g., for Cabau: base_height_msl=0, 
                              inlet_height_agl=27)

    base_height_msl : list or 1D np.array
        e.g., [15,] or np.array([15, 21, 42])
        This is the altitude of the *station above the ground*
            (e.g., for Jungfraujoch: base_height_msl=3850, 
                                     inlet_height_agl=5)

    icon_field_path : str
        Contains the path to the unstructured ICON output

    icon_grid_path : str
        Contains the path to the ICON grid
    
    interpolation_strategy : list of strings
        e.g., ['ground',] or ['ground','mountain','ground']
        Can be 'ground' or 'mountain', or 'middle' (the latter is between the ground and mountain approach)
        'ground' uses the model topography + station altitude over ground
        'mountain' uses the absolute altitude over mean sea level

    k : int, default is 5
        Sets the number of horizontal nearest neighbours desired

    field_name : str, or list of strings, optional
        e.g. 'qv', or ['qv','temp'], or None
        If no field_name is set, the whole dataset is interpolated
        in the vertical and horizontal directions.

    Returns
    -------
    xr.Dataset
        An Xarray dataset organised by 'station', containing the original
        input specifications, and the vertically and horizontally interpolated
        values
    """

    # Load dataset
    icon_field = xr.open_dataset(icon_field_path)
    # Get dimension names
    icon_heights = icon_field.z_mc.dims[
        0]  # Dimension name (something like "heights_5")
    icon_cells = icon_field.z_mc.dims[
        1]  # Dimension name (something like "ncells")
    icon_field[icon_cells] = icon_field[
        icon_cells]  # Explicitly assign 'ncells'

    # --- Horizontal grid selection & interpolation weights
    # Get k nearest horizontal distances (for use in inverse distance weighing)
    horizontal_distances, icon_grid_indices = get_horizontal_distances(
        longitude, latitude, icon_grid_path, k=k)

    horizontal_interp = 1 / horizontal_distances / (
        1 / horizontal_distances).sum(axis=1, keepdims=True)
    weights_horizontal = xr.DataArray(horizontal_interp,
                                      dims=["station", icon_cells])
    ind_X = xr.DataArray(icon_grid_indices, dims=["station", icon_cells])
    icon_subset = icon_field.isel({icon_cells: ind_X})

    # --- Vertical level selection & interpolation weights
    # Get 2 nearest vertical distances (for use in linear interpolation)
    model_topography = icon_subset.z_ifc[-1]
    model_levels = icon_subset.z_mc
    vertical_distances, icon_level_indices = get_nearest_vertical_distances(
        model_topography, model_levels, inlet_height_agl, base_height_msl,
        interpolation_strategy)

    vertical_interp = vertical_distances[:, :, ::-1] / (vertical_distances.sum(
        axis=-1, keepdims=True))
    # Say, you have the point's vertical position, and the next two model layers are positioned at [-5, +15] meters offset.
    # Then linear interpolation between those two points is simply [15/(15+5), 5/(15+5)]=[3/4 1/4]. That is what the code does (and why it reverses the order on the last axis; and why I only need the absolute vertical distances).
    # (As a curiosity, linear interpolation is the same as inverse distance weighting with 2 points. But this formulation is more stable than the inverse distance weighting, as divisions with 0 may otherwise occur!)

    weights_vertical = xr.DataArray(vertical_interp,
                                    dims=["ncells", "station", icon_heights])
    ind_Z = xr.DataArray(icon_level_indices,
                         dims=["ncells", "station", icon_heights])

    # --- Generate output
    # Subset the ICON field if we want only a few fields of output
    if field_name is not None:
        icon_subset = icon_subset[field_name]
    # Include the input station parameters in the output
    ds = xr.Dataset({
        'longitude': (['station'], longitude),
        'latitude': (['station'], latitude),
        'inlet_height_agl': (['station'], inlet_height_agl),
        'base_height_msl': (['station'], base_height_msl),
        'interpolation_strategy': (['station'], interpolation_strategy)
    })
    # Perform the interpolations
    icon_subset = icon_subset.isel({icon_heights: ind_Z})
    icon_out = icon_subset.weighted(weights_vertical.fillna(0)).sum(
        dim=icon_heights,
        skipna=True).weighted(weights_horizontal).sum(dim=icon_cells)
    icon_out = icon_out.where(
        ~(weights_vertical.sum(dim=[icon_cells, icon_heights],
                               skipna=False)).isnull()
    )  # Remove out of bounds values where weights_vertical has NaNs
    return xr.merge([icon_out, ds])


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Interpolate ICON output to point locations.')
    parser.add_argument('-lon',
                        dest='longitude',
                        default=None,
                        type=float,
                        help='Longitude of interpolation target')
    parser.add_argument('-lat',
                        dest='latitude',
                        default=None,
                        type=float,
                        help='Latitude of interpolation target')
    parser.add_argument(
        '-asl',
        dest='elevation',
        default=None,
        type=float,
        help=
        'Station surface elevation above sea level [absolute height asl: elevation+altitude]'
    )
    parser.add_argument(
        '-alt',
        dest='altitude',
        default=None,
        type=float,
        help=
        'Station altitude over surface [absolute height asl: elevation+altitude]'
    )
    parser.add_argument('-fields',
                        dest='icon_field',
                        default=None,
                        type=str,
                        help='The ICON output fields')
    parser.add_argument('-grid',
                        dest='icon_grid',
                        default=None,
                        type=str,
                        help='The ICON grid dynamic grid file')
    parser.add_argument(
        '-strat',
        dest='strategy',
        default='ground',
        type=str,
        help=
        'The interpolation strategy (should be "mountain", "ground", or "middle")'
    )
    parser.add_argument(
        '-k',
        dest='k',
        default=4,
        type=int,
        help='Number of nearest neighbours to interpolate with (e.g., 4 or 5)')
    parser.add_argument(
        '-field_name',
        dest='field_name',
        default=None,
        type=str,
        help='Field name to extract (if left out, all variables are extracted)'
    )
    parser.add_argument('-output',
                        dest='output_dest',
                        default=None,
                        type=str,
                        help='Output NetCDF destination')
    args = parser.parse_args()

    # Example run (note: most inputs should be lists, and the performance is optimized for these lists!)
    output = icon_to_point(longitude=[
        args.longitude,
    ],
                           latitude=[
                               args.latitude,
                           ],
                           inlet_height_agl=[
                               args.elevation,
                           ],
                           base_height_msl=[
                               args.altitude,
                           ],
                           icon_field_path=args.icon_field,
                           icon_grid_path=args.icon_grid,
                           interpolation_strategy=[
                               args.strategy,
                           ],
                           k=args.k,
                           field_name=args.field_name)
    output.to_netcdf(args.output_dest)
