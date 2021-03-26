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
