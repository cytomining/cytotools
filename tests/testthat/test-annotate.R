context("annotate")

test_that("`annotate` adds plate and well metadata", {
  annotated_csv <- tempfile("SQ00015116_augmented.csv")

  annotate(
    "batch0", "SQ00015116",
    cell_id = "unknown",
    format_broad_cmap = TRUE,
    output = annotated_csv,
    workspace_dir = system.file("extdata", package = "cytotools")
  )

  expected_csv <-
    system.file(
      "extdata", "backend", "batch0", "SQ00015116",
      "SQ00015116_augmented.csv",
      package = "cytotools"
    )

  expected <- readr::read_csv(expected_csv)

  actual <- readr::read_csv(annotated_csv)

  expect_equal(actual, expected)

  file.remove(annotated_csv)

  # to test the script, do this and verify that the output file
  # inst/extdata/backend/batch0/SQ00015116/SQ00015116_augmented.csv
  # does not change
  # inst/scripts/cytotools_annotate -b batch0 -p SQ00015116 -d -w inst/extdata
})

test_that("`annotate` with format_broad_cmap adds metadata columns", {
  annotated_csv <- tempfile("SQ00015116_augmented.csv")

  annotate(
    "batch0", "SQ00015116",
    cell_id = "unknown",
    format_broad_cmap = TRUE,
    output = annotated_csv,
    workspace_dir = system.file("extdata", package = "cytotools")
  )

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

  external_metadata_csv <-
    system.file(
      "extdata", "metadata", "batch0",
      "moa.csv",
      package = "cytotools"
    )

  external_metadata <- readr::read_csv(external_metadata_csv)

  annotate(
    "batch0", "SQ00015116",
    cell_id = "unknown",
    format_broad_cmap = TRUE,
    external_metadata = external_metadata_csv,
    output = annotated_csv,
    workspace_dir = system.file("extdata", package = "cytotools")
  )

  result <- readr::read_csv(annotated_csv)

  result_metadata <-
    result[paste("Metadata", colnames(external_metadata), sep = "_")] %>%
    na.omit() %>%
    dplyr::distinct() %>%
    as.data.frame()

  names(result_metadata) <- gsub("Metadata_", "", names(result_metadata))

  expect_equal(
    nrow(result_metadata %>%
      dplyr::setdiff(external_metadata)),
    0
  )

  file.remove(annotated_csv)
})

test_that("`annotate` with external_metadata adds metadata prefix", {
  annotated_csv <- tempfile("SQ00015116_augmented.csv")

  external_metadata_csv <-
    system.file(
      "extdata", "metadata", "batch0",
      "moa.csv",
      package = "cytotools"
    )

  external_metadata <- readr::read_csv(external_metadata_csv)

  annotate(
    "batch0", "SQ00015116",
    cell_id = "unknown",
    format_broad_cmap = TRUE,
    external_metadata = external_metadata_csv,
    output = annotated_csv,
    workspace_dir = system.file("extdata", package = "cytotools")
  )

  result <- readr::read_csv(annotated_csv)

  external_metadata_colnames <- c(
    "Metadata_pert_id",
    "Metadata_pert_iname",
    "Metadata_moa"
  )

  expect_true(all(external_metadata_colnames %in% colnames(result)))

  file.remove(annotated_csv)
})
