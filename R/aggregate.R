#' Aggregate morphological profiling data per well.
#'
#' \code{aggregate} helps you say something.
#
#' @param output_file       Output file for aggregated profiles.
#' @param operation         Methods used for aggregation, mean operation = "median", see cytominer::aggregate
#' @param strata            grouping variable, default strata = c("Image_Metadata_Plate", "Image_Metadata_Well")
#' @param sqlite_file       SQLite data base storing morphological profiles.

#' @importFrom magrittr %>%
#' @export
aggregate <- function(sqlite_file, output_file, strata = c("Image_Metadata_Plate", "Image_Metadata_Well"),  operation = "median") {

  # open db
  db <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)
  RSQLite::initExtension(db)

  image <- dplyr::tbl(src = db, "Image") %>%
    dplyr::select(TableNumber, ImageNumber, Image_Metadata_Plate, Image_Metadata_Well)

  aggregate_objects <- function(compartment) {
    object <- dplyr::tbl(src = db, compartment)

    object %<>% dplyr::inner_join(image, by = c("TableNumber", "ImageNumber"))

    # compartment tag converts nuclei to ^Nuclei_
    compartment_tag <-
      stringr::str_c("^", stringr::str_sub(compartment, 1, 1) %>% stringr::str_to_upper(), stringr::str_sub(compartment, 2), "_")

    variables <- colnames(object) %>% stringr::str_subset(compartment_tag)

    futile.logger::flog.info(stringr::str_c("Started aggregating ", compartment))

    cytominer::aggregate(
      population = object,
      variables = variables,
      strata = strata,
      operation = operation
    ) %>% dplyr::collect()

  }


  aggregated <-
    aggregate_objects("cells") %>%
    dplyr::inner_join(aggregate_objects("cytoplasm"),
      by =  strata) %>%
    dplyr::inner_join(aggregate_objects("nuclei"),
      by = strata)

  futile.logger::flog.info(paste0("Writing aggregated to ", output_file))

  aggregated %>% readr::write_csv(output_file)

  DBI::dbDisconnect(db)
}
