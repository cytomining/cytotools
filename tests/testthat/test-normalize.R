context("normalize")

test_that("`normalize` normalizes data", {
  futile.logger::flog.threshold(futile.logger::WARN)

  normalized_csv <- tempfile("SQ00015116_normalized.csv")

  aggregated_csv <-
    system.file(
      "extdata", "backend", "batch0", "SQ00015116",
      "SQ00015116_augmented.csv",
      package = "cytotools"
    )

  normalize(input_file = aggregated_csv,
            output_file = normalized_csv,
            operation = "robustize",
            subset = "Metadata_broad_sample_type == 'control'")

  expected_csv <-
    system.file(
      "extdata", "backend", "batch0", "SQ00015116",
      "SQ00015116_normalized.csv",
      package = "cytotools"
    )

  expected <- readr::read_csv(expected_csv)

  actual <- readr::read_csv(normalized_csv)

  expect_equal(actual, expected)

  file.remove(normalized_csv)

  # to test the script, do this and verify that the output file
  # inst/extdata/backend/batch0/SQ00015116/SQ00015116_normalized_single_cell.csv
  # does not change
  # inst/scripts/cytotools_normalize -b batch0 -p SQ00015116 -w inst/extdata/ -o inst/extdata/backend/batch0/SQ00015116/SQ00015116_normalized.csv -s "Metadata_broad_sample_type == '''control'''"
})

test_that("`normalize` normalizes data sampling from single cell data", {
  futile.logger::flog.threshold(futile.logger::WARN)

  normalized_csv <- tempfile("SQ00015116_normalized_single_cell.csv")

  aggregated_csv <-
    system.file(
      "extdata", "backend", "batch0", "SQ00015116",
      "SQ00015116_augmented.csv",
      package = "cytotools"
    )

  sqlite_file <-
    system.file(
      "extdata", "backend", "batch0", "SQ00015116",
      "SQ00015116.sqlite",
      package = "cytotools"
    )

  normalize(input_file = aggregated_csv,
            sample_single_cell = TRUE,
            input_sqlite_file = sqlite_file,
            output_file = normalized_csv,
            operation = "robustize",
            subset = "Metadata_broad_sample_type == 'control'")

  expected_csv <-
    system.file(
      "extdata", "backend", "batch0", "SQ00015116",
      "SQ00015116_normalized_single_cell.csv",
      package = "cytotools"
    )

  expected <- readr::read_csv(expected_csv)

  actual <- readr::read_csv(normalized_csv)

  expect_equal(actual, expected)

  file.remove(normalized_csv)

  # to test the script, do this and verify that the output file
  # inst/extdata/backend/batch0/SQ00015116/SQ00015116_normalized_single_cell.csv
  # does not change
  # inst/scripts/cytotools_normalize -b batch0 -p SQ00015116 -w inst/extdata/ -g -o inst/extdata/backend/batch0/SQ00015116/SQ00015116_normalized_single_cell.csv -s "Metadata_broad_sample_type == '''control'''"

})
