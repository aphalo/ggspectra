
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggspectra

[![cran
version](http://www.r-pkg.org/badges/version/photobiology)](https://cran.r-project.org/package=photobiology)
[![cran
checks](https://cranchecks.info/badges/worst/photobiology)](https://cran.r-project.org/web/checks/check_results_photobiology.html)

The goal of ‘ggspectra’ is to make it easy to plot radiation spectra and
similar data, such and transmittance, absorbance and reflectance
spectra, producing fully annotated publication- and presentation-ready
plots. It is an extension to packages ‘ggplot2’ and ‘photobiology’, and
part of the ‘r4phototbiology’ suite.

Package ‘ggspectra’ provides stats, geoms, scales and functions to be
used for plotting radiation-related spectra and annotating the plots
with quantities derived from spectral data. All summary quantities
implemented in ‘photobiology’ can be used for annotations including
biologically effective irradiances and exposures.

Please, see the web site
[r4photobiology](https://www.r4photobiology.info) for details on other
packages available as part of the suite, and on how to install them.

## Example

This is a basic example which shows plotting using the most automatic
approach.

``` r
library(photobiology)
library(ggspectra)
plot(sun.spct)
```

![](man/figures/README-example-1.png)<!-- -->

This second example uses the grammar of graphics approach, as
implemented in ‘ggplot2’.

``` r
library(ggplot2)
photon_as_default()
ggplot(sun.spct) +
  geom_spct() +
  scale_y_s.q.irrad_continuous() +
  scale_x_wl_continuous(sec.axis = sec_axis_w_number()) +
  theme_bw()
```

![](man/figures/README-example2-1.png)<!-- -->

## Installation

Installation of the most recent stable version from CRAN:

``` r
install.packages("ggspectra")
```

Installation of the current unstable version from Bitbucket:

``` r
# install.packages("devtools")
devtools::install_bitbucket("aphalo/ggspectra")
```

## Documentation

HTML documentation is available at
(<http://docs.r4photobiology.info/ggspectra/>), including a *User
Guide*.

News on updates to the different packages of the ‘r4photobiology’ suite
are regularly posted at (<https://www.r4photobiology.info/>).

Two articles introduce the basic ideas behind the design of the suite
and its use: Aphalo P. J. (2015)
(<https://doi.org/10.19232/uv4pb.2015.1.14>) and Aphalo P. J. (2016)
(<https://doi.org/10.19232/uv4pb.2016.1.15>).

A book is under preparation, and the draft is currently available at
(<https://leanpub.com/r4photobiology/>). The book conatins many examples
of plots created with ‘ggspectra’.

A handbook written before the suite was developed contains useful
information on the quantification and manipulation of ultraviolet and
visible radiation: Aphalo, P. J., Albert, A., Björn, L. O., McLeod, A.
R., Robson, T. M., & Rosenqvist, E. (Eds.) (2012) Beyond the Visible: A
handbook of best practice in plant UV photobiology (1st ed., p. xxx +
174). Helsinki: University of Helsinki, Department of Biosciences,
Division of Plant Biology. ISBN 978-952-10-8363-1 (PDF),
978-952-10-8362-4 (paperback). PDF file available from
(<http://hdl.handle.net/10138/37558>).

## Contributing

Pull requests, bug reports, and feature requests are welcome at
(<https://bitbucket.org/aphalo/ggspectra>).

## Citation

If you use this package to produce scientific or commercial
publications, please cite according to:

``` r
citation("ggspectra")
#> 
#> To cite package 'ggspectra' in publications, please use:
#> 
#>   Aphalo, Pedro J. (2015) The r4photobiology suite. UV4Plants
#>   Bulletin, 2015:1, 21-29. DOI:10.19232/uv4pb.2015.1.14
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     author = {Pedro J. Aphalo},
#>     title = {The r4photobiology suite},
#>     journal = {UV4Plants Bulletin},
#>     volume = {2015},
#>     number = {1},
#>     pages = {21-29},
#>     year = {2015},
#>     doi = {10.19232/uv4pb.2015.1.14},
#>   }
```

## License

© 2015-2018 Pedro J. Aphalo (<pedro.aphalo@helsinki.fi>). Released under
the GPL, version 2 or greater. This software carries no warranty of any
kind.
