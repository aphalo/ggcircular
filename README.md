
# ggcircular <img src="man/figures/logo.png" align="right" width="150"/>

<!-- badges: start -->

[![cran
version](https://www.r-pkg.org/badges/version/ggcircular)](https://cran.r-project.org/package=ggcircular)
\[\[cran checks\](<https://badges.cranchecks.info/worst/ggcircular>
[![R-CMD-check](https://github.com/aphalo/ggcircular/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aphalo/ggcircular/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Purpose

Package ‘**ggcircular**’ extends the Grammar of Graphics implemented in
package ‘ggplot2’ (\>= 3.5.0). The extensions add support for circular
data with statistics that follow as much as possible the user interface
of the equivalent existing summary statistics layer functions for linear
and 2D linear data. It uses package ‘circular’ as a foundation.

## Statistics

My aim is to define circular versions of `stat_density()`,
`stat_density_2D()`, `stat_bin()` and `stat_bin_2D()`, and a scale
`scale_x_circular()`. A circular version of `stat_smooth()` could be
added later.

## Examples

## Installation (NOT YET POSSIBLE)

Installation of the most recent stable version from CRAN (sources, Mac
and Win binaries):

``` r
install.packages("ggcircular")
```

Installation of the current unstable version from R-Universe CRAN-like
repository (binaries for Mac, Win, Webassembly, and Linux, as well as
sources available):

``` r
install.packages('ggcircular', 
                 repos = c('https://aphalo.r-universe.dev', 
                           'https://cloud.r-project.org'))
```

Installation of the current unstable version from GitHub (from sources):

``` r
# install.packages("remotes")
remotes::install_github("aphalo/ggcircular")
```

## Documentation (NOT YET PUBLISHED)

HTML documentation for the package, including help pages and the *User
Guide*, is available at
(<https://docs.r4photobiology.info/ggcircular/>).

News about updates are regularly posted at
(<https://www.r4photobiology.info/>).

Chapter 7 in Aphalo (2020) explains both basic concepts of the grammar
of graphics as implemented in ‘ggplot2’ as well as extensions to this
grammar including several of those made available by packages ‘ggpp’ and
‘ggpmisc’.

## Contributing

Please report bugs and request new features at
(<https://github.com/aphalo/ggcircular/issues>). Pull requests are
welcome at (<https://github.com/aphalo/ggcircular>).

## Citation

If you use this package to produce scientific or commercial
publications, please cite according to:

``` r
citation("ggcircular")
```

## Acknowledgements

Being an extension to package ‘ggplot2’, some of the code in package
‘ggcircular’ has been created by using as a template that from layer
functions in ‘ggplot2’. The user interface of ‘ggcircular’ aims at being
as consistent as possible with ‘ggplot2’ and the layered grammar of
graphics (Wickham 2010). New features added in ‘ggplot2’ are added when
relevant to ‘ggcircular’. This package does consequently indirectly
include significant contributions from several of the authors and
maintainers of ‘ggplot2’, listed at (<https://ggplot2.tidyverse.org/>).

## References

Aphalo, Pedro J. (2020) *Learn R: As a Language.* The R Series. Boca
Raton and London: Chapman and Hall/CRC Press. ISBN: 978-0-367-18253-3.
350 pp.

Wickham, Hadley. 2010. “A Layered Grammar of Graphics.” Journal of
Computational and Graphical Statistics 19 (1): 3–28.
<https://doi.org/10.1198/jcgs.2009.07098>.

## License

© 2024 Pedro J. Aphalo (<pedro.aphalo@helsinki.fi>). Released under the
GPL, version 2 or greater. This software carries no warranty of any
kind.
