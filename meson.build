project('radbelt', 'c',
  version : run_command('python', '-m', 'setuptools_scm').stdout().strip(),
  meson_version : '>=0.64.0',
  default_options : ['warning_level=2'],
)

add_global_arguments(
  '-DNPY_NO_DEPRECATED_API=NPY_1_19_API_VERSION',
  language : 'c'
)

add_languages('fortran', native : false, required : true)

is_windows = host_machine.system() == 'windows'
if is_windows
  add_project_link_arguments('-lquadmath', language : 'c')
endif

py_mod = import('python')
py = py_mod.find_installation(pure: false)

incdir_numpy = run_command(py,
  ['-c', 'import os; os.chdir(".."); import numpy; print(numpy.get_include())'],
  check : true
).stdout().strip()

incdir_f2py = run_command(py,
  ['-c', 'import os; os.chdir(".."); import numpy.f2py; print(numpy.f2py.get_include())'],
  check : true
).stdout().strip()

core_source = custom_target('coremodule.c',
  input : ['radbelt/core.pyf'],
  output : ['coremodule.c', 'core-f2pywrappers.f'],
  command : [py, '-m', 'numpy.f2py', '@INPUT@', '--lower']
)

inc_np = include_directories(incdir_numpy, incdir_f2py)

py.extension_module('core',
  [
    'radbelt/core.f',
    'radbelt/extern/igrf/shellig.f',
    'radbelt/extern/aep8/trmfun.f',
    core_source
  ],
  incdir_f2py / 'fortranobject.c',
  include_directories : inc_np,
  install : true,
  subdir : 'radbelt'
)

py.install_sources(
  [
    'radbelt/__init__.py',
    'radbelt/paths.py',
    'radbelt/util.py',
  ],
  subdir : 'radbelt')

py.install_sources(
  [
    'radbelt/__init__.py',
    'radbelt/paths.py',
    'radbelt/util.py',
  ],
  subdir : 'radbelt'
)

py.install_sources(
  [
    'radbelt/__init__.py',
    'radbelt/paths.py',
    'radbelt/util.py',
  ],
  subdir : 'radbelt'
)

py.install_sources(
  [
    'radbelt/extern/aep8/ae8max.asc',
    'radbelt/extern/aep8/ae8min.asc',
    'radbelt/extern/aep8/ap8max.asc',
    'radbelt/extern/aep8/ap8min.asc',
  ],
  subdir : 'radbelt/extern/aep8'
)

py.install_sources(
  [
    'radbelt/extern/igrf/dgrf1945.dat',
    'radbelt/extern/igrf/dgrf1950.dat',
    'radbelt/extern/igrf/dgrf1955.dat',
    'radbelt/extern/igrf/dgrf1960.dat',
    'radbelt/extern/igrf/dgrf1965.dat',
    'radbelt/extern/igrf/dgrf1970.dat',
    'radbelt/extern/igrf/dgrf1975.dat',
    'radbelt/extern/igrf/dgrf1980.dat',
    'radbelt/extern/igrf/dgrf1985.dat',
    'radbelt/extern/igrf/dgrf1990.dat',
    'radbelt/extern/igrf/dgrf1995.dat',
    'radbelt/extern/igrf/dgrf2000.dat',
    'radbelt/extern/igrf/dgrf2005.dat',
    'radbelt/extern/igrf/dgrf2010.dat',
    'radbelt/extern/igrf/dgrf2015.dat',
    'radbelt/extern/igrf/igrf2020.dat',
    'radbelt/extern/igrf/igrf2020s.dat',
  ],
  subdir : 'radbelt/extern/igrf'
)
