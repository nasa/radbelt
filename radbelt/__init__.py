from importlib import resources
import os

from .core import radbelt as _radbelt
from .extern.ccmc import igrf

with resources.path(igrf, 'dgrf1945.dat') as p:
    IGRF_DATA_PATH = str(p.parent.absolute())


def radbelt(lon, lat, height, year):
    old_dir = os.getcwd()
    os.chdir(IGRF_DATA_PATH)
    try:
        return _radbelt(lon, lat, height, year)
    finally:
        os.chdir(old_dir)
