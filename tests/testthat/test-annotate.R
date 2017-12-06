context("annotate")

test_that("`annotate` adds plate and well metadata", {
  annotated_csv <- tempfile("SQ00015116_augmented.csv")

  annotate("batch0", "SQ00015116",
           output = annotated_csv,
           workspace_dir = system.file("extdata", package = "cytotools"))

  expected_csv <-
    system.file("extdata", "backend", "batch0", "SQ00015116", "SQ00015116_augmented.csv",
                package = "cytotools")

  expected <- readr::read_csv(expected_csv)
  actual <- readr::read_csv(annotated_csv)
  expect_equal(actual, expected)

  file.remove(annotated_csv)
})

test_that("`annotate` with format_broad_cmap", {
  annotated_csv <- tempfile("SQ00015116_augmented.csv")

  annotate("batch0", "SQ00015116",
           cell_id = "abc-456-zyx",
           format_broad_cmap = TRUE,
           output = annotated_csv,
           workspace_dir = system.file("extdata", package = "cytotools"))

  result <- readr::read_csv(annotated_csv)

  # Assert these columns were added:
  broad_cmap_colnames = c(
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
