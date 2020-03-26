#' Normalize profiles based on given control subset
#'
#' \code{normalize} normalizes data based on the specified normalization method.
#'
#' @param input_file                       Input file with profiles to be normalized. If \code{NULL}, reads from \code{workspace_dir/backend/batch_id/plate_id/plate_id_augmented.csv}. default: \code{NULL}.
#' @param output_file                      Output file for storing normalized profiles. If \code{NULL}, writes to \code{workspace_dir/backend/batch_id/plate_id/plate_id_normalized.csv}. default: \code{NULL}.
#' @param subset                           Query to define the control poulation used for normalization. Default value selects all wells. default: \code{("Metadata_Plate != 'dummy'")}.
#' @param sample_single_cell               Subset comprises single cell samples if \code{TRUE}. default: \code{FALSE}.
#' @param input_sqlite_file                Input file with single cell profiles. If \code{NULL} and \code{sample_single_cell} is \code{TRUE} then reads from \code{workspace_dir/backend/batch_id/plate_id/plate_id.sqlite}. default: \code{NULL}.
#' @param compartments                     Optional character vector specifying cellular compartments. default: \code{c("cells", "cytoplasm", "nuclei")}.
#' @param operation                        Method used for normalization. See \link[cytominer]{normalize}. default: \code{"robustize"}.
#' @param strata                           Character vector specifying grouping variables for normalization. default: \code{c("Metadata_Plate")}.
#' @param batch_id                         Batch ID. Used for generating input_file and output_file if either is not specified. default: \code{NULL}.
#' @param plate_id                         Plate ID. Used for generating input_file and output_file if either is not specified. default: \code{NULL}.
#' @param workspace_dir                    Root directory containing backend and metadata subdirectories. Can be relative or absolute. default: \code{"."}.
#' @param image_object_join_columns        Columns by which to join image table and object tables. default: \code{c("TableNumber", "ImageNumber")}.
#' @param well_unique_id_columns           Columns by which to uniquely identify a well. default: \code{c("Metadata_Plate", "Metadata_Well")}.
#' @param well_unique_id_columns_db_prefix Prefix for \code{well_unique_id_columns} in the SQLite db. default: \code{""}
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
normalize <- function(input_file = NULL,
                      output_file = NULL,
                      subset = NULL,
                      sample_single_cell = FALSE,
                      input_sqlite_file = NULL,
                      compartments = c("cells", "cytoplasm", "nuclei"),
                      operation = "robustize",
                      strata = c("Metadata_Plate"),
                      batch_id = NULL,
                      plate_id = NULL,
                      workspace_dir = ".",
                      image_object_join_columns = c("TableNumber", "ImageNumber"),
                      well_unique_id_columns = c("Metadata_Plate", "Metadata_Well"),
                      well_unique_id_columns_db_prefix = "") {
  if (is.null(input_file) && (is.null(batch_id) || is.null(plate_id))) {
    stop("Either input_file or batch_id and plate_id should be specified.")
  }

  if (is.null(input_file) && (is.null(batch_id) || is.null(plate_id))) {
    stop("Either input_file or batch_id and plate_id should be specified.")
  }

  if (sample_single_cell && is.null(input_sqlite_file) && (is.null(batch_id) || is.null(plate_id))) {
    stop("Either input_sqlite_file or batch_id and plate_id should be specified when sample_single_cell is TRUE.")
  }

  if (is.null(input_file)) {
    input_file <- file.path(workspace_dir, "backend", batch_id, plate_id, sprintf("%s_augmented.csv", plate_id))
  }

  if (is.null(input_sqlite_file)) {
    input_sqlite_file <- file.path(workspace_dir, "backend", batch_id, plate_id, sprintf("%s.sqlite", plate_id))
  }

  if (is.null(output_file)) {
    output_file <- file.path(workspace_dir, "backend", batch_id, plate_id, sprintf("%s_normalized.csv", plate_id))
  }

  if (is.null(subset)) {
    subset <- "Metadata_Plate != 'dummy'"
  }

  # cytominer-database ingest by default does not add Image_ prefix
  # https://github.com/cytomining/cytominer-database/blob/3926b55627e76a319460d6ac3ff4bdf65e1f5f98/cytominer_database/ingest.py#L128
  if (well_unique_id_columns_db_prefix != "") {
    stop("Prefixes for unique identifiers for wells in image table currently not supported.")
  }

  #well_unique_id_columns_db <- stringr::str_c(well_unique_id_columns_db_prefix, well_unique_id_columns)

  # compartment tag converts nuclei to ^Nuclei_
  compartment_tag <-
    function(compartment) paste0("^", stringr::str_to_title(compartment), "_")

  profiles <- readr::read_csv(input_file)

  # prepare to load objects by loading image table
  if (sample_single_cell) {
    if (!file.exists(input_sqlite_file)) {
      stop(paste0(input_sqlite_file, " does not exist"))
    }

    db <- DBI::dbConnect(RSQLite::SQLite(), input_sqlite_file,
                         loadable.extensions = TRUE)

    # get metadata and copy to db
    metadata <-
      profiles %>%
      dplyr::select(dplyr::matches("Metadata_")) %>%
      dplyr::distinct()

    metadata <- dplyr::copy_to(db, metadata)

    # cytominer-database ingest by default does not add Image_ prefix
    # https://github.com/cytomining/cytominer-database/blob/3926b55627e76a319460d6ac3ff4bdf65e1f5f98/cytominer_database/ingest.py#L128
    # This will fail if we have the prefix
    image <- dplyr::tbl(src = db, "image") %>%
      dplyr::select(c(image_object_join_columns, well_unique_id_columns)) %>%
      dplyr::inner_join(metadata, by = well_unique_id_columns)
  }

  load_objects <- function(compartment) {
    dplyr::tbl(src = db, compartment) %>%
      dplyr::inner_join(image, by = image_object_join_columns)
  }

  load_profiles <- function(compartment) {
    # select columns that have a prefix as Metadata, or one of the compartment
    # names
    profiles %>%
      dplyr::select(dplyr::matches(
        stringr::str_c("Metadata_", "|", compartment_tag(compartment))
      ))
  }

  normalize_profiles <- function(compartment) {
    if (sample_single_cell) {
      sample <- load_objects(compartment = compartment)
    } else {
      sample <- load_profiles(compartment = compartment)
    }

    # variables are columns with a prefix as one of the compartments
    # e.g. Nuclei_
    variables <- colnames(sample) %>%
      stringr::str_subset(compartment_tag(compartment))

    # get the sample on which to compute the normalization parameters
    sample %<>%
      dplyr::filter(!!rlang::parse_expr(subset)) %>%
      dplyr::collect(n = Inf) %>%
      dplyr::mutate_at(variables, as.double)

    # normalize
    normalized <-
      cytominer::normalize(
        population = load_profiles(compartment = compartment),
        variables = variables,
        strata = strata,
        sample = sample,
        operation = operation
      )
  }

  metadata <-
    colnames(profiles) %>%
    stringr::str_subset("^Metadata_")

  normalized <-
    compartments %>%
    purrr::map(normalize_profiles) %>%
    purrr::reduce(dplyr::inner_join, by = metadata)

  normalized %>% readr::write_csv(output_file)

  if (sample_single_cell) {
    DBI::dbDisconnect(db)
  }
}
