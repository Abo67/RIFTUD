# RIFTUD
A Q1D code for radial turbines originally developed at NASA, now equipped with real gas features

We started from https://ntrs.nasa.gov/api/citations/19760010058/downloads/19760010058.pdf; a NASA technical note titled
"Computer program for design analysis of radial-inflow turbines" by Arthur J. Glassman
which describes "A computer program written in FORTRAN that may be used for the design analysis of radial-inflow turbines"

The orginal Fotran code has been modified so that the CoolProp library http://www.coolprop.org/
is used to compute the thermodynamic properties of the gas, instead of the "perfect gas" model available
in the original code.

The newly added code relies on the fzero() subroutine from http://www.netlib.org/slatec/
You therefore need to download and compile slatec; on linux also download http://www.netlib.org/slatec/slatec4linux.tgz
which provides replacements for r1mach and d1mach (machine constants) using lapack calls.

