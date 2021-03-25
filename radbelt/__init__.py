from importlib import resources
import os

from . import core
from .extern.ccmc import igrf

with resources.path(igrf, 'dgrf1945.dat') as p:
    IGRF_DATA_PATH = str(p.parent.resolve())


def radbelt(lon, lat, height, year):
    old_dir = os.getcwd()
    os.chdir(IGRF_DATA_PATH)
    try:
        return core.igrf(lon, lat, height, year)
    finally:
        os.chdir(old_dir)
