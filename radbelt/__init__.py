from contextlib import contextmanager
from importlib import resources
import os

from . import core
from .extern.ccmc import igrf as igrf_data
from .extern.ccmc import aep8 as aep8_data

with resources.path(igrf_data, 'dgrf1945.dat') as p:
    IGRF_DATA_PATH = str(p.parent.resolve())

with resources.path(aep8_data, 'ae8min.asc') as p:
    AEP8_DATA_PATH = str(p.parent.resolve())

del igrf_data, aep8_data


@contextmanager
def working_directory(path):
    old_dir = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(old_dir)


@working_directory(IGRF_DATA_PATH)
def igrf(lon, lat, height, year):
    return core.igrf(lon, lat, height, year)


@working_directory(AEP8_DATA_PATH)
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

    return core.aep8(energy, lvalue, bb0, modelnum)
