from contextlib import contextmanager
from importlib import resources
import os

from . import core
from .extern.ccmc import igrf as igrf_data

with resources.path(igrf_data, 'dgrf1945.dat') as p:
    IGRF_DATA_PATH = str(p.parent.resolve())

del igrf_data


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
