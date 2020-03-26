#' Add plate and well metadata.
#'
#' \code{annotate} adds plate and well metadata, as well as optional external
#' metadata. It assumes the following files exist:
#' \itemize{
#'   \item \code{<workspace_dir>/metadata/barcode_platemap.csv} which maps the
#'   plate barcodes to the corresponding platemaps,
#'   \item For each platemap, a file
#' \code{<workspace_dir>/metadata/platemap/<plate_map_name>.txt} which
#' contains the metadata for each well position
#' }
#' Additional metadata can be appended to the output file via the optional
#' \code{external_metadata} parameter. \code{external_metadata} is a CSV
#' file. The following columns are required:
#' \itemize{
#'   \item \code{Metadata_Plate}
#'   \item \code{Metadata_Well}
#' }
#' The \code{Metadata_} prefix can be dropped from required columns if the
#' remaining metadata columns aren't also prefixed by \code{Metadata_}.
#'
#'
#' @param batch_id            Batch ID.
#' @param plate_id            Plate ID.
#' @param cell_id             Optional cell ID. default: \code{NULL}.
#' @param external_metadata   Optional external metadata to join with, as a CSV file. default: \code{NULL}.
#' @param format_broad_cmap   Add columns for compatibility with Broad CMap naming conventions. default: \code{FALSE}.
#' @param output              Output file (.CSV) for annotated data. If \code{NULL}, writes to \code{workspace_dir/backend/batch_id/plate_id/plate_id_augmented.csv}. default: \code{NULL}.
#' @param perturbation_mode   Perturbation mode. Must be one of \code{"chemical"} or \code{"genetic"}. default: \code{"chemical"}.
#' @param workspace_dir       Root directory containing backend and metadata subdirectories. Can be relative or absolute. default: \code{"."}.
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @importFrom magrittr extract2
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @export
annotate <- function(batch_id, plate_id,
                     cell_id = NULL,
                     external_metadata = NULL,
                     format_broad_cmap = FALSE,
                     output = NULL,
                     perturbation_mode = "chemical",
                     workspace_dir = ".") {
  metadata_dir <- file.path(workspace_dir, "metadata", batch_id)

  backend_dir <- file.path(workspace_dir, "backend", batch_id, plate_id)

  # read profiles and rename column names
  profiles <- suppressMessages(readr::read_csv(
    file.path(backend_dir, paste0(plate_id, ".csv"))
  ))

  profiles %<>%
    setNames(names(profiles) %>%
      stringr::str_replace_all("^Image_Metadata", "Metadata"))

  # read and join metadata map
  metadata_map <- suppressMessages(
    readr::read_csv(
      file.path(metadata_dir, "barcode_platemap.csv"),
      col_types = readr::cols(
        Assay_Plate_Barcode = readr::col_character(),
        Plate_Map_Name = readr::col_character()
      )
    )
  )

  stopifnot("Assay_Plate_Barcode" %in% colnames(metadata_map))

  metadata_map %<>%
    setNames(names(metadata_map) %>% stringr::str_replace_all("^", "Metadata_"))

  profiles %<>%
    dplyr::mutate(
      Metadata_Assay_Plate_Barcode =
        as.character(.data$Metadata_Plate)
    )

  profiles %<>%
    dplyr::inner_join(metadata_map, by = c("Metadata_Assay_Plate_Barcode"))

  # read and join platemap
  plate_map_name <- profiles %>%
    dplyr::select("Metadata_Plate_Map_Name") %>%
    dplyr::distinct() %>%
    magrittr::extract2("Metadata_Plate_Map_Name")

  stopifnot(length(plate_map_name) == 1)

  platemap <-
    suppressMessages(
      readr::read_tsv(
        file.path(metadata_dir, "platemap", paste0(plate_map_name, ".txt"))
      )
    )

  stopifnot("well_position" %in% colnames(platemap))

  if ("plate_map_name" %in% colnames(platemap)) {
    platemap %<>% dplyr::select(-plate_map_name)
  }

  platemap %<>%
    setNames(names(platemap) %>% stringr::str_replace_all("^", "Metadata_"))

  profiles %<>%
    dplyr::mutate(Metadata_well_position = .data$Metadata_Well)

  profiles %<>%
    dplyr::inner_join(platemap, by = c("Metadata_well_position"))

  # format_broad_cmap
  if (format_broad_cmap) {
    profiles %<>%
      dplyr::mutate(
        Metadata_pert_id = stringr::str_extract(
          .data$Metadata_broad_sample,
          "(BRD[-N][A-Z0-9]+)"
        ),
        Metadata_pert_mfc_id = .data$Metadata_broad_sample,
        Metadata_pert_well = .data$Metadata_Well,
        Metadata_pert_id_vendor = ""
      )

    if ("Metadata_cell_id" %in% names(profiles)) {
      message("`cell_id` column present in metadata, will not override.")
    } else {
      profiles %<>% dplyr::mutate(Metadata_cell_id = cell_id)
    }

    if (perturbation_mode == "chemical") {
      profiles %<>%
        dplyr::mutate(
          Metadata_broad_sample_type =
            ifelse(is.na(.data$Metadata_broad_sample) |
              .data$Metadata_broad_sample == "DMSO",
            "control",
            "trt"
            ),
          Metadata_broad_sample =
            ifelse(.data$Metadata_broad_sample_type == "control",
              "DMSO",
              .data$Metadata_broad_sample
            ),
          Metadata_mmoles_per_liter =
            ifelse(.data$Metadata_broad_sample_type == "control",
              0,
              .data$Metadata_mmoles_per_liter
            ),
          Metadata_pert_vehicle = .data$Metadata_solvent
        ) %>%
        dplyr::mutate(
          Metadata_broad_sample_type =
            ifelse(.data$Metadata_broad_sample == "empty",
              "empty",
              .data$Metadata_broad_sample_type
            )
        )

      if ("Metadata_mg_per_ml" %in% names(profiles)) {
        profiles %<>%
          dplyr::mutate(
            Metadata_mg_per_ml =
              ifelse(.data$Metadata_broad_sample_type == "control",
                0,
                .data$Metadata_mg_per_ml
              )
          )
      }
    }

    if (perturbation_mode == "genetic") {
      profiles %<>%
        dplyr::mutate(
          Metadata_broad_sample_type =
            ifelse(.data$Metadata_pert_name == "EMPTY",
              "control",
              "trt"
            )
        )
    }

    profiles %<>%
      dplyr::mutate(Metadata_pert_type = .data$Metadata_broad_sample_type)
  }

  # external_metadata
  if (!is.null(external_metadata)) {
    external_metadata_df <- suppressMessages(readr::read_csv(external_metadata))

    # Check whether the columns have "Metadata" prefix; if not, assume that all
    # columns need the suffix
    if (length(grep("Metadata_", colnames(external_metadata_df))) == 0) {
      external_metadata_df %<>%
        setNames(names(external_metadata_df) %>%
          stringr::str_replace_all("^", "Metadata_"))
    }

    profiles %<>%
      dplyr::left_join(
        external_metadata_df %>%
          dplyr::distinct()
      )
  }

  # format_broad_cmap: columns that may be added after joining with external
  # metadata
  if (format_broad_cmap) {
    if ("Metadata_pert_iname" %in% colnames(profiles)) {
      profiles %<>%
        dplyr::mutate(
          Metadata_pert_mfc_desc = .data$Metadata_pert_iname,
          Metadata_pert_name = .data$Metadata_pert_iname
        )
    }
  }

  # save
  if (is.null(output)) {
    output <- file.path(backend_dir, paste0(plate_id, "_augmented.csv"))
  }

  metadata_cols <- stringr::str_subset(names(profiles), "^Metadata_")

  feature_cols <- stringr::str_subset(
    names(profiles),
    "^Cells_|^Cytoplasm_|^Nuclei_"
  )

  all_cols <- c(metadata_cols, feature_cols)

  profiles[all_cols] %>% readr::write_csv(output)
}
