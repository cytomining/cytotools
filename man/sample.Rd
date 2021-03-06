% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample.R
\name{sample}
\alias{sample}
\title{Sample selects replicates across specified plates.}
\usage{
sample(batch_id, pattern, output, replicates = NULL, workspace_dir = ".")
}
\arguments{
\item{batch_id}{Batch ID.}

\item{pattern}{Regular expression specifying the filenames containing the profiles from which to sample. Only CSVs are allowed.}

\item{output}{Output file. The extension should be either csv, rds, or feather.}

\item{replicates}{Number of replicates to select per plate map. Selects all replicates if \code{NULL}. default: \code{NULL}.}

\item{workspace_dir}{Root directory containing backend and metadata subdirectories. Can be relative or absolute. default: \code{"."}.}
}
\description{
\code{sample} replicates across specified plates, and aggregates the
result in a single file. It assumes the following files exist:
\itemize{
  \item \code{<workspace_dir>/metadata/barcode_platemap.csv} which maps the
  plate barcodes to the corresponding platemaps,
  \item For each plate barcode, a CSV file
\code{<workspace_dir>/backend/<batch_id>/<file-matching-specified-pattern>}
which will be among the files included in the sample.
}
}
