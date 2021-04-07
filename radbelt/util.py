#
# Copyright © 2021 United States Government as represented by the Administrator
# of the National Aeronautics and Space Administration. No copyright is claimed
# in the United States under Title 17, U.S. Code. All Other Rights Reserved.
#
# SPDX-License-Identifier: NASA-1.3
#

from contextlib import contextmanager
from os import getcwd, chdir


@contextmanager
def working_directory(path):
    old_dir = getcwd()
    chdir(path)
    try:
        yield
    finally:
        chdir(old_dir)
