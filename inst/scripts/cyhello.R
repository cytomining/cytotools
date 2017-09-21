#!/usr/bin/env Rscript

'hello

Usage:
cyhello.R <msgtext>

Options:
-h --help Show this screen.' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(cytotools)))

opts <- docopt(doc)

msgtext <- opts[["msgtext"]]
