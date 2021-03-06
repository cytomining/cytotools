% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate.R
\name{annotate}
\alias{annotate}
\title{Add plate and well metadata.}
\usage{
annotate(
  batch_id,
  plate_id,
  cell_id = NULL,
  external_metadata = NULL,
  format_broad_cmap = FALSE,
  output = NULL,
  perturbation_mode = "chemical",
  workspace_dir = "."
)
}
\arguments{
\item{batch_id}{Batch ID.}

\item{plate_id}{Plate ID.}

\item{cell_id}{Optional cell ID. default: \code{NULL}.}

\item{external_metadata}{Optional external metadata to join with, as a CSV file. default: \code{NULL}.}

\item{format_broad_cmap}{Add columns for compatibility with Broad CMap naming conventions. default: \code{FALSE}.}

\item{output}{Output file (.CSV) for annotated data. If \code{NULL}, writes to \code{workspace_dir/backend/batch_id/plate_id/plate_id_augmented.csv}. default: \code{NULL}.}

\item{perturbation_mode}{Perturbation mode. Must be one of \code{"chemical"} or \code{"genetic"}. default: \code{"chemical"}.}

\item{workspace_dir}{Root directory containing backend and metadata subdirectories. Can be relative or absolute. default: \code{"."}.}
}
\description{
\code{annotate} adds plate and well metadata, as well as optional external
metadata. It assumes the following files exist:
\itemize{
  \item \code{<workspace_dir>/metadata/<batch_id>/barcode_platemap.csv} which maps the
  plate barcodes to the corresponding platemaps,
  \item For each platemap, a file
\code{<workspace_dir>/metadata/<batch_id>/platemap/<plate_map_name>.txt} which
contains the metadata for each well position
}
Additional metadata can be appended to the output file via the optional
\code{external_metadata} parameter. \code{external_metadata} is a CSV
file. The following columns are required:
\itemize{
  \item \code{Metadata_Plate}
  \item \code{Metadata_Well}
}
The \code{Metadata_} prefix can be dropped from required columns if the
remaining metadata columns aren't also prefixed by \code{Metadata_}.
}
