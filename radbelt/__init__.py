from importlib import resources

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
