#!/usr/bin/env Rscript

# https://github.com/tidyverse/dplyr/issues/1760
extends <- methods::extends

'regularize

Usage:
  cytotools_regularize -i <file> -o <file>

Options:
  -h --help                   Show this screen.
  -i <file> --input=<file>    Input file. CSV or SQLite only.
  -o <file> --output=<file>   Output file. Same format as input_file' -> doc

opts <- docopt::docopt(doc)

cytotools::regularize(input_file = opts[["input"]],
                      output_file = opts[["output"]])
