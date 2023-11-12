#
# Copyright © 2021 United States Government as represented by the Administrator
# of the National Aeronautics and Space Administration. No copyright is claimed
# in the United States under Title 17, U.S. Code. All Other Rights Reserved.
#
# SPDX-License-Identifier: NASA-1.3
#

from importlib.resources import files, as_file

with as_file(files(__package__)) as path:
    IGRF_DATA_PATH = str(path / 'extern' / 'igrf')
    AEP8_DATA_PATH = str(path / 'extern' / 'aep8')
