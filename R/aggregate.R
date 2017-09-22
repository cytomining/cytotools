#' Aggregate morphological profiling data per well.
#'
#' \code{aggregate} helps you say something.
#
#' @param db                SQLite data base storing morphological profiles.
#' @param output_file       Output file for aggregated profiles.
#' @importFrom magrittr %>%
#' @export
aggregate <- function(db, output_file) {
  image <- tbl(src = db, "image") %>%
    select(TableNumber, ImageNumber, Image_Metadata_Plate, Image_Metadata_Well)

  aggregate_objects <- function(compartment) {
    object <- tbl(src = db, compartment)

    object %<>% inner_join(image, by = c("TableNumber", "ImageNumber"))

    # compartment tag converts nuclei to ^Nuclei_
    compartment_tag <-
      str_c("^", str_sub(compartment, 1, 1) %>% str_to_upper(), str_sub(compartment, 2), "_")

    variables <- colnames(object) %>% stringr::str_subset(compartment_tag)

    futile.logger::flog.info(str_c("Started aggregating ", compartment))

    cytominer::aggregate(
      population = object,
      variables = variables,
      strata = c("Image_Metadata_Plate", "Image_Metadata_Well"),
      operation = "median"
    ) %>% collect()
  }

  aggregated <-
    aggregate_objects("cells") %>%
    inner_join(aggregate_objects("cytoplasm"),
      by = c("Image_Metadata_Plate", "Image_Metadata_Well")) %>%
    inner_join(aggregate_objects("nuclei"),
      by = c("Image_Metadata_Plate", "Image_Metadata_Well"))

  futile.logger::flog.info(paste0("Writing aggregated to ", opts[["output"]]))

  aggregated %>% readr::write_csv(output_file)
}
