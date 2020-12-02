[![Build Status](https://travis-ci.org/cytomining/cytotools.png?branch=master)](https://travis-ci.org/cytomining/cytotools) [![Coverage Status](https://img.shields.io/codecov/c/github/cytomining/cytotools/master.svg)](https://codecov.io/github/cytomining/cytotools?branch=master)

cytotools
=========

`cytotools` provides command-line tools for processing morphological profiling 
datasets. Most of the functionality is provided using `cytominer`.


Installation
------------

Install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("cytomining/cytotools", dependencies = TRUE, build_vignettes = TRUE)
```

The command-line tools are stored in `inst/scripts`. After installing this 
package, run the snippet below, and add the output to your `PATH`:

```{r}
normalizePath(file.path(path.package("cytotools"), "scripts"))
```


