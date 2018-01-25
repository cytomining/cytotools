#' Preselect...
#'
#' \code{preselect}
#'
#' @param batch_id            Batch ID.
#' @param input               Test data on which to perform variable selection operations. Must be CSV.
#' @param operations          List of operations to perform, as strings. Supported operations: \code{"correlation_threshold"}, \code{"variance_threshold"}, \code{"replicate_correlation"}.
#' @param replicates          Number of replicates to select per plate map. Required for the operation \code{"replicate_correlation"}. default: \code{NULL}.
#' @param subset              Query to create the training data by subsetting. Must be CSVs. default: \code{NULL}.
#' @param output_dir          Output directory for preselected feature names. If \code{NULL}, writes to \code{workspace_dir/parameters/batch_id/variable_selection/}. default: \code{NULL}.
#' @param workspace_dir       Root directory containing backend and metadata subdirectories. Can be relative or absolute. default: \code{"."}.
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom stats filter
#' @importFrom stats median
#' @importFrom stats na.omit
#' @export
preselect <- function(batch_id, input, operations,
                      replicates = NULL,
                      subset = NULL,
                      output_dir = NULL,
                      workspace_dir = ".") {
  if (is.null(output_dir)) {
    output_dir <- file.path(
      workspace_dir, "parameters",
      batch_id, "variable_selection"
    )
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  for (operation in operations) {
    variable_selections_file <- file.path(output_dir, paste0(operation, ".txt"))

    if (tools::file_ext(input) == "rds") {
      df <- readRDS(input)
    } else if (tools::file_ext(input) == "csv") {
      df <- suppressMessages(readr::read_csv(input))
    } else if (tools::file_ext(input) == "feather") {
      df <- suppressMessages(feather::read_feather(input))
    } else {
      stop(paste0("Unsupported file extension: ", tools::file_ext(input)))
    }

    variables <-
      colnames(df) %>%
      stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")

    futile.logger::flog.info("Dropping variables that have NA in any row...")

    df %<>%
      cytominer::variable_select(
        variables = variables,
        operation = "drop_na_columns",
        cutoff = 0.0
      )

    variables <-
      colnames(df) %>%
      stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")

    if (!is.null(subset)) {
      futile.logger::flog.info(sprintf("Subsetting using %s", subset))

      sample <- df %>% dplyr::filter_(subset)
    } else {
      sample <- df
    }

    futile.logger::flog.info(sprintf("Performing %s...", operation))

    if (operation %in% c("variance_threshold", "correlation_threshold")) {
      df <-
        cytominer::variable_select(
          population = df,
          variables = variables,
          sample = sample,
          operation = operation
        ) %>%
        dplyr::collect()
    } else if (operation == "replicate_correlation") {
      # This is handled differently because there is no direct
      # way yet to do filtering in cytominer.
      # TODO: rewrite this after cytominer has an appropriate
      # filtering function for this.
      if (is.null(replicates)) {
        stop("replicates is required for operation replicate_correlation")
      }

      feature_replicate_correlations <-
        df %>%
        cytominer::replicate_correlation(
          variables = variables,
          strata = c("Metadata_Plate_Map_Name", "Metadata_Well"),
          split_by = "Metadata_Plate_Map_Name",
          replicates = replicates
        )

      feature_replicate_correlations %>%
        readr::write_csv(file.path(output_dir, paste0(operation, ".csv")))

      variables <-
        feature_replicate_correlations %>%
        na.omit() %>%
        filter(median > 0.6) %>% # intentionally hard-coded to avoid confusion
        magrittr::extract2("variable")

      metadata <-
        colnames(df) %>%
        stringr::str_subset("^Metadata_")

      df %<>%
        dplyr::select_(.dots = c(metadata, variables))
    }

    variables <-
      colnames(df) %>%
      stringr::str_subset(
        "^Nuclei_|^Cells_|^Cytoplasm_"
      )

    futile.logger::flog.info(
      sprintf("Writing variable selections to %s", variable_selections_file)
    )

    dplyr::data_frame(variable = variables) %>%
      readr::write_csv(variable_selections_file)
  }
}
