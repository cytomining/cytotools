context("aggregate")

test_that("`aggregate` aggregates data per well", {
  futile.logger::flog.threshold(futile.logger::WARN)

  aggregated_csv <- tempfile("SQ00015116.csv")

  sqlite_file <-
    system.file(
      "extdata", "backend", "batch0", "SQ00015116",
      "SQ00015116.sqlite",
      package = "cytotools"
    )

  aggregate(sqlite_file, aggregated_csv, operation = "mean")

  expected_csv <-
    system.file(
      "extdata", "backend", "batch0", "SQ00015116",
      "SQ00015116.csv",
      package = "cytotools"
    )

  expected <- readr::read_csv(expected_csv)

  actual <- readr::read_csv(aggregated_csv)

  expect_equal(actual, expected)

  file.remove(aggregated_csv)

  # to test the script, do this and verify that the output file
  # inst/extdata/backend/batch0/SQ00015116/SQ00015116.csv
  # does not change
  # inst/scripts/cytotools_aggregate inst/extdata/backend/batch0/SQ00015116/SQ00015116.sqlite -o inst/extdata/backend/batch0/SQ00015116/SQ00015116.csv -m mean
})

test_that(
  "`aggregate` aggregates data per well only in specified compartments", {
    futile.logger::flog.threshold(futile.logger::WARN)

    aggregated_csv <- tempfile("SQ00015116.csv")

    sqlite_file <-
      system.file(
        "extdata", "backend", "batch0", "SQ00015116",
        "SQ00015116.sqlite",
        package = "cytotools"
      )

    aggregate(
      sqlite_file, aggregated_csv,
      operation = "mean",
      compartments = c("cells")
    )

    expected_csv <-
      system.file(
        "extdata", "backend", "batch0", "SQ00015116",
        "SQ00015116.csv",
        package = "cytotools"
      )

    expected <- readr::read_csv(expected_csv) %>%
      dplyr::select(
        "Metadata_Plate",
        "Metadata_Well",
        "Cells_AreaShape_Area",
        "Cells_Intensity_IntegratedIntensity_DNA"
      )

    actual <- readr::read_csv(aggregated_csv)

    expect_equal(actual, expected)

    file.remove(aggregated_csv)
  }
)
