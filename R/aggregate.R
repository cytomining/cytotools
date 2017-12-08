#' Aggregate data based on given grouping.
#'
#' \code{aggregate} aggregates data based on the specified aggregation method.
#'
#' @param sqlite_file       SQLite database storing morphological profiles.
#' @param output_file       Output file for storing aggregated profiles.
#' @param compartments      optional character vector specifying cellular compartments. default \code{c("cells", "cytoplasm", "nuclei")}.
#' @param operation         optional character string specifying method for aggregation, e.g. \code{"mean"}, \code{"median"}, \code{"mean+sd"}. default \code{"mean"}.
#' @param strata            character vector specifying grouping variables for aggregation. default \code{c("Image_Metadata_Plate", "Image_Metadata_Well")}.
#' @param variables         optional character vector specifying observation variables. default \code{"all"}.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
aggregate <- function(sqlite_file,
                      output_file,
                      compartments = c("cells", "cytoplasm", "nuclei"),
                      operation = "mean",
                      strata = c("Image_Metadata_Plate", "Image_Metadata_Well"),
                      variables = "all") {

  db <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)

  #https://github.com/tidyverse/dplyr/issues/3093
  RSQLite::initExtension(db)

  # columns by which to join image table and object tables
  image_object_join_columns <- c("TableNumber", "ImageNumber")

  image <- dplyr::tbl(src = db, "image") %>%
    dplyr::select(c(image_object_join_columns, strata))

  aggregate_objects <- function(compartment) {
    object <- dplyr::tbl(src = db, compartment)

    object %<>% dplyr::inner_join(image, by = image_object_join_columns)

    # compartment tag converts e.g. nuclei to ^Nuclei_
    compartment_tag <- paste0("^", stringr::str_to_title(compartment), "_")

    variables_ <- colnames(object) %>% stringr::str_subset(compartment_tag)

    if (variables != "all") {
      variables_ <-
        intersect(
          paste(stringr::str_to_title(compartment), variables, sep = "_"),
          variables_
          )
    }

    futile.logger::flog.info(paste0("Started aggregating ", compartment))

    cytominer::aggregate(
      population = object,
      variables = variables_,
      strata = strata,
      operation = operation
    ) %>%
      dplyr::collect()
  }

  aggregated <-
    compartments %>%
    purrr::map(aggregate_objects) %>%
    purrr::reduce(dplyr::inner_join, by = strata)

  futile.logger::flog.info(paste0("Writing aggregated to ", output_file))

  aggregated %>% readr::write_csv(output_file)

  DBI::dbDisconnect(db)
}
