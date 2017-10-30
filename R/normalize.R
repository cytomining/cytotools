#' Normalize profiles based on given control subset
#'
#'@param batch_id                  batch ID given as directory name
#'@param plate_id                  plate ID given as the directory
#'@param subset                    Query to define the control poulation used for normalization \code{("Metadata_line_num == 5")}
#'@param backend_directory         Top level directory of the experiment, default: \code{"../../backend/"},
#'@param operation                 Method used for normalization, default: \code{"robustize"}. See cytominer::normalize
#'@param sample_single_cell        Use single cell data for sampling, instead of per-well profiles, default \code{FALSE}.
#'@param tmpdir                    Temporary directory, default \code{"/tmp"}.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
normalize <- function(batch_id,
                      plate_id,
                      subset,
                      backend_directory = file.path("..", "..", "backend"),
                      tmpdir = '/tmp',
                      operation = "robustize",
                      sample_single_cell = FALSE
                      ){

full_backend_directory <- file.path(backend_directory, batch_id, plate_id)

# TODO: parse file name as input argument of cytotools_normalize
profiles <- readr::read_csv(paste(full_backend_directory, paste0(plate_id, "_augmented.csv"), sep = "/"))


# prepare to load objects by loading image table so we only have to load the data once.
if (sample_single_cell) {
  sqlite_file <- paste(full_backend_directory, paste0(plate_id, ".sqlite"), sep = "/")

  # TODO: if file doesn't exist at path then copy it from S3 to a tmpdir
  if (!file.exists(sqlite_file)) {
    stop(paste0(sqlite_file, " does not exist"))
  }

  ## cytominer scripts version
  # db <- dplyr::src_sqlite(path = db_path)

  db <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)

  #https://github.com/tidyverse/dplyr/issues/3093
  RSQLite::initExtension(db)


  # get metadata and copy to db
  metadata <-
    profiles %>%
    dplyr::select(dplyr::matches("Metadata_")) %>%
    dplyr::distinct()

  metadata <- copy_to(db, metadata)

  image <- dplyr::tbl(src = db, "image") %>%
    dplyr::select(TableNumber, ImageNumber, Image_Metadata_Plate, Image_Metadata_Well) %>%
    dplyr::rename(Metadata_Plate = Image_Metadata_Plate) %>%
    dplyr::rename(Metadata_Well = Image_Metadata_Well) %>%
    dplyr::inner_join(metadata, by = c("Metadata_Plate", "Metadata_Well"))
}


# compartment tag converts nuclei to ^Nuclei_
compartment_tag <- function(compartment) {
  stringr::str_c(
    "^",
    stringr::str_sub(compartment, 1, 1) %>%
      stringr::str_to_upper(),
    stringr::str_sub(compartment, 2),
    "_"
  )
}

load_objects <- function(compartment) {
  dplyr::tbl(src = db, compartment) %>%
    dplyr::inner_join(image, by = c("TableNumber", "ImageNumber"))

}

load_profiles <- function(compartment) {
  profiles %>%
    dplyr::select(dplyr::matches(stringr::str_c("Metadata_", "|", compartment_tag(compartment))))

}

normalize_profiles <- function(compartment) {
  if (sample_single_cell) {
    sample <- load_objects(compartment = compartment)

  } else {
    sample <- load_profiles(compartment = compartment)

  }

  variables <- colnames(sample) %>%
    stringr::str_subset(compartment_tag(compartment))

  sample %<>%
    dplyr::filter_(subset) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::mutate_at(variables, as.double)

  normalized <-
    cytominer::normalize(
      population = load_profiles(compartment = compartment),
      variables = variables,
      strata =  c("Metadata_Plate"),
      sample = sample,
      operation = operation
    )

}

metadata <-
  colnames(profiles) %>%
  stringr::str_subset("^Metadata_")

normalized <-
  normalize_profiles("cells") %>%
  dplyr::inner_join(normalize_profiles("cytoplasm"),
    by = metadata) %>%
  dplyr::inner_join(normalize_profiles("nuclei"),
    by = metadata)

normalized %>% readr::write_csv(paste(full_backend_directory, paste0(plate_id, "_normalized.csv"), sep = "/"))

}
