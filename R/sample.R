utils::globalVariables(c("Assay_Plate_Barcode", "replicate_id"))
#' Sample selects replicates across specified plates.
#'
#' \code{sample} replicates across specified plates, and aggregates the result in a single file.
#'
#' @param batch_id        Batch ID.
#' @param pattern         Regular expression specifying the filenames containing the profiles from which to sample. Only CSVs are allowed.
#' @param output          Output file. The extension should be either csv, rds, or feather.
#' @param replicates      Number of replicates to select per plate map. Selects all replicates if \code{NULL}. default: \code{NULL}.
#' @param workspace_dir   Root directory containing backend and metadata subdirectories. Can be relative or absolute. default: \code{"."}.
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @importFrom magrittr extract2
#' @importFrom rlang .data
#' @importFrom utils head
#' @importFrom utils tail
#' @export
sample <- function(batch_id, pattern, output,
                   replicates = NULL,
                   workspace_dir = ".") {
  backend_dir <- paste(workspace_dir, "backend", batch_id, sep = "/")

  metadata_dir <- paste(workspace_dir, "metadata", batch_id, sep = "/")

  file_list <- list.files(
    backend_dir,
    pattern = pattern,
    recursive = T, full.names = T
  )

  if (!is.null(replicates)) {
    # get the list of plates that retrieved using the pattern
    plate_list_retrieved <-
      dplyr::data_frame(
        Assay_Plate_Barcode = lapply(file_list, function(file) {
          head(tail(stringr::str_split(file, "/")[[1]], 2), 1)
        }) %>%
          unlist()
      )

    replicates <- as.integer(replicates)

    # create a plate_list based on number of replicates to be selected
    plate_list <-
      suppressMessages(
        readr::read_csv(
          paste(metadata_dir, "barcode_platemap.csv", sep = "/"),
          col_types = readr::cols(
            Assay_Plate_Barcode = readr::col_character(),
            Plate_Map_Name = readr::col_character()
          )
        )
      ) %>%
      dplyr::select("Assay_Plate_Barcode", "Plate_Map_Name") %>%
      dplyr::inner_join(plate_list_retrieved, by = "Assay_Plate_Barcode") %>%
      dplyr::group_by("Plate_Map_Name") %>%
      dplyr::arrange(Assay_Plate_Barcode) %>%
      dplyr::mutate(
        replicate_id = dplyr::dense_rank(.data$Assay_Plate_Barcode)
      ) %>%
      dplyr::filter(replicate_id %in% seq(replicates)) %>%
      dplyr::ungroup() %>%
      dplyr::select("Assay_Plate_Barcode") %>%
      magrittr::extract2("Assay_Plate_Barcode")

    # filter file_list based on plate_list
    file_list <-
      lapply(file_list, function(file) {
        if (length(unlist(lapply(
          plate_list,
          function(plate) grep(plate, file)
        )))) {
          file
        }
      }) %>%
      unlist()
  }

  futile.logger::flog.info(
    sprintf(
      "Reading %d files...:\n%s", length(file_list),
      paste(file_list, collapse = "\n")
    )
  )

  df <- file_list %>%
    lapply(function(x) suppressMessages(readr::read_csv(x))) %>%
    dplyr::bind_rows()

  futile.logger::flog.info(sprintf("Output contains %d rows.", nrow(df)))

  if (tools::file_ext(output) == "rds") {
    saveRDS(df, output)
  } else if (tools::file_ext(output) == "csv") {
    readr::write_csv(df, output)
  } else if (tools::file_ext(output) == "feather") {
    feather::write_feather(df, output)
  } else {
    stop(paste0("Unsupported file extension: ", tools::file_ext(output)))
  }
}
