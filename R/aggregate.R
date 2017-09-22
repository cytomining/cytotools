#' Aggregate data based on given grouping.
#'
#' \code{aggregate} aggregates data based on the specified aggregation method.
#'
#' @param sqlite_file       SQLite database storing morphological profiles.
#' @param output_file       Output file for storing aggregated profiles.
#' @param operation         optional character string specifying method for aggregation. This must be one of the strings \code{"mean"}, \code{"median"}, \code{"mean+sd"}. default \code{"median"}.
#' @param strata            character vector specifying grouping variables for aggregation. default \code{c("Image_Metadata_Plate", "Image_Metadata_Well")}.
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
aggregate <- function(sqlite_file,
                      output_file,
                      strata = c("Image_Metadata_Plate", "Image_Metadata_Well"),
                      operation = "median") {

  db <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)

  #https://github.com/tidyverse/dplyr/issues/3093
  RSQLite::initExtension(db)

  # columns by which to join image table and objec tables
  image_object_join_columns <- c("TableNumber", "ImageNumber")

  image <- dplyr::tbl(src = db, "image") %>%
    dplyr::select(c(image_object_join_columns, strata))

  aggregate_objects <- function(compartment) {
    object <- dplyr::tbl(src = db, compartment)

    object %<>% dplyr::inner_join(image, by = image_object_join_columns)

    # compartment tag converts e.g. nuclei to ^Nuclei_
    compartment_tag <-
      paste0("^",
                     stringr::str_sub(compartment, 1, 1) %>%
                       stringr::str_to_upper(),
                     stringr::str_sub(compartment, 2), "_")

    variables <- colnames(object) %>% stringr::str_subset(compartment_tag)

    futile.logger::flog.info(paste0("Started aggregating ", compartment))

    cytominer::aggregate(
      population = object,
      variables = variables,
      strata = strata,
      operation = operation
    ) %>% dplyr::collect()

  }

  # consider making `compartment` a parameter and then replace the below with
  # a loop over the components.
  aggregated <-
    aggregate_objects("cells") %>%
    dplyr::inner_join(aggregate_objects("cytoplasm"),
      by = strata) %>%
    dplyr::inner_join(aggregate_objects("nuclei"),
      by = strata)

  futile.logger::flog.info(paste0("Writing aggregated to ", output_file))

  aggregated %>% readr::write_csv(output_file)

  DBI::dbDisconnect(db)
}
