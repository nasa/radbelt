#
# Copyright © 2021 United States Government as represented by the Administrator
# of the National Aeronautics and Space Administration. No copyright is claimed
# in the United States under Title 17, U.S. Code. All Other Rights Reserved.
#
# SPDX-License-Identifier: NASA-1.3
#

from astropy import units as u
import numpy as np

from .core import igrf as _igrf, aep8 as _aep8
from .paths import IGRF_DATA_PATH, AEP8_DATA_PATH
from .util import working_directory


@working_directory(IGRF_DATA_PATH)
@np.vectorize
def igrf(lon, lat, height, year):
    return _igrf(lon, lat, height, year)


@working_directory(AEP8_DATA_PATH)
@np.vectorize
def aep8(energy, lvalue, bb0, particle, solar):
    if particle not in ('e', 'p'):
        raise ValueError('particle must be "e" or "p"')
    if solar not in ('min', 'max'):
        raise ValueError('solar must be "min" or "max"')

    modelnum = {
        ('e', 'min'): 1,
        ('e', 'max'): 2,
        ('p', 'min'): 3,
        ('p', 'max'): 4
    }[(particle, solar)]

    return _aep8(energy, lvalue, bb0, modelnum)


def get_flux(coords, time, energy, particle, solar):
    """Calculate the flux of trapped particles at a specific location and time.

    Parameters
    ----------
    coords : astropy.coordinates.EarthLocation
        The position relative to the Earth.
    time : astropy.time.Time
        The time (needed to account for drift of the Earth's magnetic field).
    energy : astropy.units.Quantity
        The minimum energy.
    particle : {'e', 'p'}
        The particle species: 'e' for electrons, 'p' for protons.
    solar : {'min', 'max'}
        The solar activity: solar minimum or solar maximum.

    Returns
    -------
    flux : astropy.units.Quantity
        The flux of particles above the given energy, in units of cm^-2 s^-1.

    Example
    -------

    >>> from radbelt import get_flux
    >>> from astropy import units as u
    >>> from astropy.coordinates import EarthLocation
    >>> from astropy.time import Time
    >>> coords = EarthLocation(-45 * u.deg, -30 * u.deg, 500 * u.km)
    >>> time = Time('2021-03-01')
    >>> energy = 20 * u.MeV
    >>> get_flux(coords, time, energy, 'p', 'max')  # doctest: +FLOAT_CMP
    <Quantity 2642.50268555 1 / (s cm2)>

    """
    lvalue, bb0 = igrf(coords.geodetic.lon.deg,
                       coords.geodetic.lat.deg,
                       coords.geodetic.height.to_value(u.km),
                       time.utc.decimalyear)
    flux = aep8(energy.to_value(u.MeV), lvalue, bb0, particle, solar)
    return flux * u.cm**-2 * u.s**-1
