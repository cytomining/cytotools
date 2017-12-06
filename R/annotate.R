#' Add plate and well metadata.
#'
#' \code{annotate} adds plate and well metadata, as well as optional external metadata.
#'
#' @param batch_id            Batch ID.
#' @param plate_id            Plate ID.
#' @param cell_id             Optional cell ID. default: \code{NULL}.
#' @param external_metadata   Optional external metadata to join with, as a JSON file. default: \code{NULL}.
#' @param format_broad_cmap   Add columns for compatibility with Broad CMap naming conventions. default: \code{FALSE}.
#' @param output              Output file (.CSV) for annotated data. If \code{NULL}, writes to \code{workspace_dir/backend/batch_id/plate_id/plate_id_augmented.csv}. default: \code{NULL}.
#' @param perturbation_mode   Perturbation mode. Must be one of \code{"chemical"} or \code{"genetic"}. default: \code{"chemical"}.
#' @param workspace_dir       Root directory containing backend and metadata subdirectories. Can be relative or absolute. default: \code{"."}.
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @importFrom magrittr extract2
#' @importFrom stats setNames
#' @export
annotate <- function(batch_id, plate_id,
                     cell_id = NULL,
                     external_metadata = NULL,
                     format_broad_cmap = FALSE,
                     output = NULL,
                     perturbation_mode = "chemical",
                     workspace_dir = ".") {
  metadata_dir <- paste(workspace_dir, "metadata", batch_id, sep = "/")
  backend_dir <- paste(workspace_dir, "backend", batch_id, plate_id, sep = "/")

  # read profiles and rename column names
  profiles <- suppressMessages(readr::read_csv(paste(backend_dir, paste0(plate_id, ".csv"), sep = "/")))
  profiles %<>% setNames(names(profiles) %>% stringr::str_replace_all("^Image_Metadata", "Metadata"))

  # read and join metadata map
  metadata_map <- suppressMessages(
    readr::read_csv(
      paste(metadata_dir, "barcode_platemap.csv", sep = "/"),
      col_types = cols(Assay_Plate_Barcode = col_character(), Plate_Map_Name = col_character())))
  testthat::expect_true("Assay_Plate_Barcode" %in% colnames(metadata_map))
  metadata_map %<>% setNames(names(metadata_map) %>% stringr::str_replace_all("^", "Metadata_"))
  profiles %<>% mutate(Metadata_Assay_Plate_Barcode = as.character(Metadata_Plate))
  profiles %<>% inner_join(metadata_map, by = c("Metadata_Assay_Plate_Barcode"))

  # read and join platemap
  platemap_name <- profiles %>% select(Metadata_Plate_Map_Name) %>% distinct() %>% magrittr::extract2("Metadata_Plate_Map_Name")
  testthat::expect_equal(length(platemap_name), 1)
  platemap <- suppressMessages(readr::read_tsv(paste(metadata_dir, "platemap", paste0(platemap_name, ".txt"), sep = "/")))
  testthat::expect_true("well_position" %in% colnames(platemap))
  if ('plate_map_name' %in% colnames(platemap)) {
    platemap %<>% select(-plate_map_name)
  }
  platemap %<>% setNames(names(platemap) %>% stringr::str_replace_all("^", "Metadata_"))
  profiles %<>% mutate(Metadata_well_position = Metadata_Well)
  profiles %<>% inner_join(platemap, by = c("Metadata_well_position"))

  # format_broad_cmap
  if (format_broad_cmap) {
    profiles %<>%
      mutate(Metadata_pert_id = stringr::str_extract(Metadata_broad_sample, "(BRD[-N][A-Z0-9]+)"),
             Metadata_pert_mfc_id = Metadata_broad_sample,
             Metadata_pert_well = Metadata_Well,
             Metadata_pert_id_vendor = "")

    if ('Metadata_cell_id' %in% names(profiles)) {
      message('`cell_id` column present in metadata, will not override.')
    } else {
      profiles %<>% mutate(Metadata_cell_id = cell_id)
    }

    if (perturbation_mode == "chemical") {
      profiles %<>%
        mutate(Metadata_broad_sample_type = ifelse(is.na(Metadata_broad_sample) | Metadata_broad_sample == "DMSO", "control", "trt"),
               Metadata_broad_sample = ifelse(Metadata_broad_sample_type =="control", "DMSO", Metadata_broad_sample),
               Metadata_mmoles_per_liter = ifelse(Metadata_broad_sample_type =="control", 0, Metadata_mmoles_per_liter),
               Metadata_pert_vehicle = Metadata_solvent) %>%
        mutate(Metadata_broad_sample_type = ifelse(Metadata_broad_sample == "empty", "empty", Metadata_broad_sample_type))
      if ("Metadata_mg_per_ml" %in% names(profiles)) {
        profiles %<>% mutate(Metadata_mg_per_ml = ifelse(Metadata_broad_sample_type =="control", 0, Metadata_mg_per_ml))
      }
    }

    if (perturbation_mode == "genetic") {
      profiles %<>%
        mutate(Metadata_broad_sample_type = ifelse(Metadata_pert_name == "EMPTY", "control", "trt"))
    }

    profiles %<>%
      mutate(Metadata_pert_type = Metadata_broad_sample_type)
  }

  # external_metadata
  if(!is.null(external_metadata)) {
    external_metadata_df <- suppressMessages(readr::read_csv(external_metadata))

    # Check whether the columns have "Metadata" prefix; if not, assume that all columns need the suffix
    if (length(grep("Metadata_", colnames(external_metadata_df))) == 0) {
      external_metadata_df %<>% setNames(names(external_metadata_df) %>% stringr::str_replace_all("^", "Metadata_"))

    }

    profiles %<>%
      left_join(
        external_metadata_df %>%
          distinct()
      )
  }

  # format_broad_cmap: columns that may be added after joining with external metadata
  if (format_broad_cmap) {
    if ("Metadata_pert_iname" %in% colnames(profiles)) {
      profiles %<>%
        mutate(Metadata_pert_mfc_desc = Metadata_pert_iname,
               Metadata_pert_name = Metadata_pert_iname)
    }
  }

  # save
  if (is.null(output)) {
    output <- paste(backend_dir, paste0(plate_id, "_augmented.csv"), sep = "/")
  }
  metadata_cols <- stringr::str_subset(names(profiles), "^Metadata_")
  feature_cols <- stringr::str_subset(names(profiles), "^Cells_|^Cytoplasm_|^Nuclei_")
  all_cols <- c(metadata_cols, feature_cols)
  profiles[all_cols] %>% readr::write_csv(output)
}
