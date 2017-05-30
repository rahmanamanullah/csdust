Simulation package for photon scatter and absorption by a dust shell

This code can be used to study the effect on the light from circum stellar
dust.  Photons are transferred through a circum stellar dust layer where the
multiple scattering of each photon is taken into account.

A monochromatic light source is assumed.  The output can be convolved with
different (time-varying) spectral shapes in order to study diffent observed
spectral sequences.

Output is stored in FITS files, and includes the number of interactions as
well as the total integrated path for each photon before it exits the shell. 

Dust scattering and absorption properties are included for the Milky Way
and the Large and Small Magellanic clouds (as described in Draine 2003).

This code has been used in Amanullah et al. (2011) to study how 
circumstellar dust would affect observables of Type Ia supernovae.

https://arxiv.org/abs/1103.1960

The package depends on Christian Walck's random number generator
and CFITSIO (https://heasarc.gsfc.nasa.gov/fitsio/fitsio.html).
