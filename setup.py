from numpy.distutils.core import Extension, setup

setup(
    ext_modules=[
        Extension('radbelt.core',
            [
                'radbelt/core.pyf',
                'radbelt/core.f',
                'radbelt/extern/igrf/shellig.f',
                'radbelt/extern/aep8/trmfun.f'
            ]
        )
    ]
)
