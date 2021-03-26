from importlib import resources

from .extern import igrf as igrf_data
from .extern import aep8 as aep8_data

with resources.path(igrf_data, 'dgrf1945.dat') as p:
    IGRF_DATA_PATH = str(p.parent.resolve())
with resources.path(aep8_data, 'ae8min.asc') as p:
    AEP8_DATA_PATH = str(p.parent.resolve())

del igrf_data, aep8_data
