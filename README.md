[![Build Status](https://travis-ci.org/cytomining/cytotools.png?branch=master)](https://travis-ci.org/cytomining/cytotools) 

cytotools
=========

`cytotools` provides command-line tools for processing morphological profiling datasets. Most of the functionality is provided using `cytominer`.

`cytotools` is the new home for <https://github.com/broadinstitute/cytominer_scripts>. 

The command-line tools are stored in `inst/scripts`. After installing this package, 
add this folder to your `PATH`:

```{r}
normalizePath(file.path(path.package("cytotools"), "scripts"))
```

