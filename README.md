# radbelt: An Astropy-friendly wrapper for the AE-8/AP-8 Van Allen belt model

This is small Python library to model the fluxes of charged particles trapped
in the Van Allen belt. It provides a fast, simple, and convenient Python
interface to the [International Geomagnetic Reference Field (IGRF)] model and
NASA's AE-8/AP-8 models of electron and proton fluxes, which are both
implemented in Fortran. The package is integrated with the [Astropy] ecosystem
for easy conversion of coordinate systems, time scales, and units. With this
package, it is easy and fast to determine the flux of particles above any given
energy, at any position, at any time.

## Acknowledging radbelt

This package is wraps the following Fortran codes, which have been retrieved
from NASA Goddard Space Flight Center's (GSFC) [Community Coordinated Modeling
Center (CCMC)]:

- https://ccmc.gsfc.nasa.gov/models/modelinfo.php?model=IGRF
- https://ccmc.gsfc.nasa.gov/models/modelinfo.php?model=AE-8/AP-8%20RADBELT

When publishing results derived from this Python package, please cite the
following articles:

- [Vette, J.I., Lucero, A.B., Wright, J.A., et al. 1966, "Models of the Trapped Radiation Environment." NASA SP-3024.](https://ui.adsabs.harvard.edu/abs/1966NASSP3024.....V)
- [Sawyer, D.M. & Vette, J.I. 1976, "AP-8 trapped proton environment for solar maximum and solar minimum." NASA WDC-A-R&S 76-06, NASA-TM-X-72605.](https://ui.adsabs.harvard.edu/abs/1976STIN...7718983S)
- [Vette, J.I. 1991, "The AE-8 trapped electron model environment." NSSDC/WDC-A-R&S 91-24.](https://ui.adsabs.harvard.edu/abs/1991STIN...9224228V)
- [Thébault, E., Finlay, C.C., Beggan, C.D., et al. 2015, "International Geomagnetic Reference Field: the 12th generation." Earth, Planets, and Space, 67, 79.](https://ui.adsabs.harvard.edu/abs/2015EP&S...67...79T)

## To install

    $ pip install .

## Example

```pycon
>>> from radbelt import get_flux
>>> from astropy import units as u
>>> from astropy.coordinates import EarthLocation
>>> from astropy.time import Time
>>> coords = EarthLocation(-45 * u.deg, -30 * u.deg, 500 * u.km)
>>> time = Time('2021-03-01')
>>> energy = 20 * u.MeV
>>> get_flux(coords, time, energy, 'p', 'max')  # doctest: +FLOAT_CMP
<Quantity 2642.50268555 1 / (s cm2)>
```

## Known issues

* The CCMC IGRF code has spatially varying errors of a few percent, which will
  result in a striped pattern in the resulting particle flux.

[International Geomagnetic Reference Field (IGRF)]: https://www.ngdc.noaa.gov/IAGA/vmod/igrf.html
[Astropy]: https://www.astropy.org
[Community Coordinated Modeling Center (CCMC)]: https://ccmc.gsfc.nasa.gov/
