context("annotate")

test_that("`annotate` adds plate and well metadata", {
  annotated_csv <- tempfile("SQ00015116_augmented.csv")

  annotate("batch0", "SQ00015116",
           output = annotated_csv,
           workspace_dir = system.file("extdata", package = "cytotools"))

  expected_csv <-
    system.file("extdata", "backend", "batch0", "SQ00015116",
                "SQ00015116_augmented.csv",
                package = "cytotools")

  expected <- readr::read_csv(expected_csv)
  actual <- readr::read_csv(annotated_csv)
  expect_equal(actual, expected)

  file.remove(annotated_csv)
})

test_that("`annotate` with format_broad_cmap adds metadata columns", {
  annotated_csv <- tempfile("SQ00015116_augmented.csv")

  annotate("batch0", "SQ00015116",
           cell_id = "abc-456-zyx",
           format_broad_cmap = TRUE,
           output = annotated_csv,
           workspace_dir = system.file("extdata", package = "cytotools"))

  result <- readr::read_csv(annotated_csv)

  # Assert these columns were added:
  broad_cmap_colnames <- c(
    "Metadata_pert_id",
    "Metadata_pert_mfc_id",
    "Metadata_pert_well",
    "Metadata_pert_id_vendor",
    "Metadata_cell_id",
    "Metadata_broad_sample_type",
    "Metadata_pert_vehicle",
    "Metadata_pert_type"
  )

  expect_true(all(broad_cmap_colnames %in% colnames(result)))

  file.remove(annotated_csv)
})

test_that("`annotate` with external_metadata appends metadata", {
  annotated_csv <- tempfile("SQ00015116_augmented.csv")

  external_metadata_csv <- tempfile("external_m")

  external_metadata <- tibble(
    "Metadata_Plate" = c("SQ00015116", "SQ00015116"),
    "Metadata_Well" = c("A01", "B01"),
    "Metadata_pert_id" = c("BRD-K18895904", "BRD-K18895904"),
    "Metadata_pert_site" = c(5, 23),
    "Metadata_pert_mg_per_ml" = c(3.12432, 1.04143999999919)
  )

  readr::write_csv(external_metadata, external_metadata_csv)

  annotate("batch0", "SQ00015116",
           external_metadata = external_metadata_csv,
           output = annotated_csv,
           workspace_dir = system.file("extdata", package = "cytotools"))

  result <- readr::read_csv(annotated_csv)

  expect_equal(
    result[colnames(external_metadata)] %>% as.data.frame(),
    external_metadata %>% as.data.frame(),
    tolerance = 10e-7,
    check.attributes = FALSE
  )

  file.remove(annotated_csv)

  file.remove(external_metadata_csv)
})

test_that("`annotate` with external_metadata adds metadata prefix", {
  annotated_csv <- tempfile("SQ00015116_augmented.csv")

  external_metadata_csv <- tempfile("external_m")

  external_metadata <- tibble(
    "Plate" = c("SQ00015116", "SQ00015116"),
    "Well" = c("A01", "B01"),
    "pert_id" = c("BRD-K18895904", "BRD-K18895904"),
    "pert_site" = c(5, 23),
    "pert_mg_per_ml" = c(3.12432, 1.04143999999919)
  )

  readr::write_csv(external_metadata, external_metadata_csv)

  annotate("batch0", "SQ00015116",
           external_metadata = external_metadata_csv,
           output = annotated_csv,
           workspace_dir = system.file("extdata", package = "cytotools"))

  result <- readr::read_csv(annotated_csv)

  external_metadata_colnames <- c(
    "Metadata_pert_id",
    "Metadata_pert_site",
    "Metadata_pert_mg_per_ml"
  )

  expect_true(all(external_metadata_colnames %in% colnames(result)))

  file.remove(annotated_csv)

  file.remove(external_metadata_csv)
})
