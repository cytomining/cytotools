context("preselect")

test_that("`preselect` with correlation_threshold, variance_threshold, replicate_correlation", {
  sample_csv <-
    system.file(
      "extdata", "parameters", "batch0", "sample",
      "normalized_sample.csv",
      package = "cytotools"
    )

  operations <- c("correlation_threshold", "variance_threshold", "replicate_correlation")

  output_dir <- tempdir()

  # test replicate_correlation parameters
  expect_error(
    preselect(
      batch_id = "batch0",
      input = sample_csv,
      operations = c("replicate_correlation"),
      subset = "Metadata_Plate_Map_Name == 'C-7161-01-LM6-001'",
      output_dir = output_dir,
      workspace_dir = system.file("extdata", package = "cytotools")
    ),
    "replicates is required for operation replicate_correlation"
  )

  preselect(
    batch_id = "batch0",
    input = sample_csv,
    operations = operations,
    replicates = 2,
    subset = "Metadata_Plate_Map_Name == 'C-7161-01-LM6-001'",
    cores = 1,
    output_dir = output_dir,
    workspace_dir = system.file("extdata", package = "cytotools")
  )

  for (operation in operations) {
    expected <- system.file(
      "extdata", "parameters", "batch0", "variable_selection",
      paste0(operation, ".txt"),
      package = "cytotools"
    ) %>%
      readr::read_file() %>%
      strsplit("\n") %>%
      unlist()

    actual <- file.path(output_dir, paste0(operation, ".txt")) %>%
      readr::read_file() %>%
      strsplit("\n") %>%
      unlist()

    expect_equal(actual, expected)
  }
  # to test the script, do this and verify that the files in
  # inst/extdata/parameters/batch0/
  # do not change
  # inst/scripts/cytotools_preselect -i inst/extdata/parameters/batch0/sample/normalized_sample.csv -s "Metadata_Plate_Map_Name == '''C-7161-01-LM6-001'''" -r correlation_threshold,variance_threshold,replicate_correlation -b batch0 -n 2 -w inst/extdata
})
