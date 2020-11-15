#' Aggregate data based on given grouping.
#'
#' \code{aggregate} aggregates data based on the specified aggregation method.
#'
#' @param sqlite_file       SQLite database storing morphological profiles.
#' @param output_file       Output file for storing aggregated profiles.
#' @param compartments      Optional character vector specifying cellular compartments. default: \code{c("cells", "cytoplasm", "nuclei")}.
#' @param operation         Optional character string specifying method for aggregation, e.g. \code{"mean"}, \code{"median"}, \code{"mean+sd"}. See \link[cytominer]{aggregate}. default: \code{"mean"}.
#' @param strata            Character vector specifying grouping variables for aggregation. default: \code{c("Metadata_Plate", "Metadata_Well")}.
#' @param image_table       Optional character string image table name. default: \code{"image"}.
#' @param image_variables   Optional character vector specifying observation variables in image table. default: \code{NULL}.
#' @param variables         Optional character vector specifying observation variables. default: \code{"all"}.
#' @param univariate        Optional boolean specifying whether aggregation function is univariate (vs multivariate). default: \code{"TRUE"}.
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
aggregate <- function(sqlite_file,
                      output_file,
                      compartments = c("cells", "cytoplasm", "nuclei"),
                      operation = "mean",
                      strata = c("Metadata_Plate", "Metadata_Well"),
                      image_table = "image",
                      image_variables = NULL,
                      variables = "all",
                      univariate = TRUE) {
  db <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file,
                       loadable.extensions = TRUE)

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

    futile.logger::flog.info(paste0("Started aggregating ", compartment, " ..."))

    cytominer::aggregate(
      population = object,
      variables = variables_,
      strata = strata,
      operation = operation,
      univariate = univariate
    ) %>%
      dplyr::collect()
  }

  aggregated <-
    compartments %>%
    purrr::map(aggregate_objects) %>%
    purrr::reduce(dplyr::inner_join, by = strata)

  futile.logger::flog.info(paste0("Started aggregating images ..."))

  if(!is.null(image_variables)) {
    aggregate_images <-
      cytominer::aggregate(
        population = dplyr::tbl(src = db, image_table),
        variables = image_variables,
        strata = strata,
        operation = operation,
        univariate = univariate
      ) %>%
      dplyr::collect()

    aggregated %<>%
      dplyr::inner_join(aggregate_images)
  }

  futile.logger::flog.info(paste0("Writing aggregated to ", output_file))

  aggregated %>% readr::write_csv(output_file)

  DBI::dbDisconnect(db)
}
