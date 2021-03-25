from setuptools import find_packages
from numpy.distutils.core import Extension, setup

setup(
    name='radbelt',
    ext_modules=[
        Extension('radbelt.core',
            [
                'radbelt/core.pyf',
                'radbelt/core.f',
                'radbelt/extern/ccmc/igrf/shellig.f'
            ]
        )
    ],
    install_requires=['numpy'],
    packages=find_packages(),
    package_data={
        'radbelt.extern.ccmc.igrf': ['*.dat']
    }
)
