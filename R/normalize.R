#' Normalize profiles based on given control subset
#'
#' \code{normalize} normalizes data based on the specified normn method.
#'
#' @param batch_id             batch ID given as directory name.
#' @param plate_id             plate ID given as the directory.
#' @param subset               query to define the control poulation used for normalization \code{("Metadata_line_num == 5")}.
#' @param backend_directory    top-level directory of the experiment, default: \code{"../../backend/"}.
#' @param compartments         optional character vector specifying cellular compartments. default \code{c("cells", "cytoplasm", "nuclei")}.
#' @param operation            method used for normalization, default: \code{"robustize"}. See cytominer::normalize.
#' @param sample_single_cell   use single cell data for sampling, instead of per-well profiles, default \code{FALSE}.
#' @param strata               character vector specifying grouping variables for normalization. default \code{c("Metadata_Plate")}.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
normalize <- function(batch_id,
                      plate_id,
                      subset,
                      backend_directory = file.path("..", "..", "backend"),
                      compartments = c("cells", "cytoplasm", "nuclei"),
                      operation = "robustize",
                      sample_single_cell = FALSE,
                      strata = c("Metadata_Plate")
                      ) {

  full_backend_directory <- file.path(backend_directory, batch_id, plate_id)

  # columns by which to join image table and object tables
  image_object_join_columns <- c("TableNumber", "ImageNumber")

  # columns by which to uniquely identify a well
  well_unique_id_columns <- c("Metadata_Plate", "Metadata_Well")

  # the columns have a prefix of "Image_" in the SQLite db.
  well_unique_id_columns_db <- stringr::str_c("Image_", well_unique_id_columns)

  # compartment tag converts nuclei to ^Nuclei_
  compartment_tag <-
    function(compartment) paste0("^", stringr::str_to_title(compartment), "_")

  # TODO: parse file name as input argument of cytotools_normalize
  profiles <-
    readr::read_csv(paste(full_backend_directory,
                          paste0(plate_id, "_augmented.csv"), sep = "/"))

  # prepare to load objects by loading image table so we only have to load the
  # data once.

  if (sample_single_cell) {
    sqlite_file <- paste(full_backend_directory,
                         paste0(plate_id, ".sqlite"), sep = "/")

    # TODO: if file doesn't exist at path then copy it from S3 to a tmpdir
    if (!file.exists(sqlite_file)) {
      stop(paste0(sqlite_file, " does not exist"))
    }

    db <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)

    #https://github.com/tidyverse/dplyr/issues/3093
    RSQLite::initExtension(db)

    # get metadata and copy to db
    metadata <-
      profiles %>%
      dplyr::select(dplyr::matches("Metadata_")) %>%
      dplyr::distinct()

    metadata <- dplyr::copy_to(db, metadata)

    # TODO: change the renames to use tidyeval form
    image <- dplyr::tbl(src = db, "image") %>%
      dplyr::select(c(image_object_join_columns, well_unique_id_columns_db)) %>%
      dplyr::rename(Metadata_Plate = Image_Metadata_Plate) %>%
      dplyr::rename(Metadata_Well = Image_Metadata_Well) %>%
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
        stringr::str_c("Metadata_", "|", compartment_tag(compartment))))

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
      dplyr::filter_(subset) %>%
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

  normalized %>%
    readr::write_csv(paste(full_backend_directory,
                           paste0(plate_id, "_normalized.csv"), sep = "/"))

  if (sample_single_cell) {
    DBI::dbDisconnect(db)

  }

}
