from sklearn.neighbors import BallTree
import numpy as np
from math import radians


def intp_icon_data(iloc, gridinfo, datainfo, latitudes, longitudes, asl, elev,
                   station_name):
    nn_sel = np.zeros(gridinfo.nn, dtype=int)
    u = np.zeros(gridinfo.nn)

    R = 6373.0  # Earth's radius in km

    if (radians(longitudes[iloc]) < np.nanmin(gridinfo.clon)) or (radians(
            longitudes[iloc]) > np.nanmax(gridinfo.clon)):
        return np.nan * np.ones((gridinfo.nn)), np.full(
            (gridinfo.nn), -1), np.full((gridinfo.nn), -1), nn_sel, u

    if (radians(latitudes[iloc]) < np.nanmin(gridinfo.clat)) or (radians(
            latitudes[iloc]) > np.nanmax(gridinfo.clat)):
        return np.nan * np.ones((gridinfo.nn)), np.full(
            (gridinfo.nn), -1), np.full((gridinfo.nn), -1), nn_sel, u

    lat1, lon1 = radians(latitudes[iloc]), radians(longitudes[iloc])

    # Use BallTree for fast nearest-neighbor search
    coords = np.deg2rad(np.column_stack((gridinfo.clat, gridinfo.clon)))
    tree = BallTree(coords, metric='haversine')
    dist, nn_sel = tree.query([[lat1, lon1]], k=gridinfo.nn)

    # Convert haversine distance (in radians) to km
    dist *= R

    u = 1.0 / dist.flatten()

    idx_above = -1 * np.ones(gridinfo.nn, dtype=int)
    idx_below = -1 * np.ones(gridinfo.nn, dtype=int)

    target_asl = datainfo.z_ifc[-1, nn_sel].flatten() + elev[iloc]

    for nnidx in range(gridinfo.nn):
        for i_mc, mc in enumerate(datainfo.z_mc[:, nn_sel[0, nnidx]]):
            if mc >= target_asl[nnidx]:
                idx_above[nnidx] = i_mc
            else:
                idx_below[nnidx] = i_mc
                break

        if idx_below[nnidx] == -1:
            idx_below[nnidx] = idx_above[nnidx]

    vert_scaling_fact = np.zeros(gridinfo.nn)

    for nnidx in range(gridinfo.nn):
        if idx_below[nnidx] != idx_above[nnidx]:
            vert_scaling_fact[nnidx] = (
                target_asl[nnidx] -
                datainfo.z_mc[idx_below[nnidx], nn_sel[0, nnidx]]) / (
                    datainfo.z_mc[idx_above[nnidx], nn_sel[0, nnidx]] -
                    datainfo.z_mc[idx_below[nnidx], nn_sel[0, nnidx]])

    return vert_scaling_fact, idx_below, idx_above, nn_sel.flatten(), u
