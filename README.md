cytotools
=========

`cytotools` provides command-line tools for processing morphological profiling 
datasets. Most of the functionality is provided using `cytominer`.

The command-line tools are stored in `inst/scripts`. After installing this 
package, run the snippet below, and add the output to your `PATH`:

```{r}
normalizePath(file.path(path.package("cytotools"), "scripts"))
```
