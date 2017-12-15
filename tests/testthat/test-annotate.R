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

  external_metadata <- dplyr::tibble(
    "Metadata_Plate" = rep(c("SQ00015116"), 30),
    "Metadata_Well" = c("A01", "A02", "A03", "A04", "A05", "A06",
                        "B01", "B02", "B03", "B04", "B05", "B06",
                        "C01", "C02", "C03", "C04", "C05", "C06",
                        "D01", "D02", "D03", "D04", "D05", "D06",
                        "E01", "E02", "E03", "E04", "E05", "E06"),
    "Metadata_pert_id" = rep(c("BRD-K18895904"), 30),
    "Metadata_pert_site" = seq(1, 30),
    "Metadata_pert_mg_per_ml" = rnorm(30)
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


  external_metadata <- dplyr::tibble(
    "Plate" = rep(c("SQ00015116"), 30),
    "Well" = c("A01", "A02", "A03", "A04", "A05", "A06",
                        "B01", "B02", "B03", "B04", "B05", "B06",
                        "C01", "C02", "C03", "C04", "C05", "C06",
                        "D01", "D02", "D03", "D04", "D05", "D06",
                        "E01", "E02", "E03", "E04", "E05", "E06"),
    "pert_id" = rep(c("BRD-K18895904"), 30),
    "pert_site" = seq(1, 30),
    "pert_mg_per_ml" = rnorm(30)
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
