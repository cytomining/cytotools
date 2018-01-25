context("preselect")

test_that("`preselect` with correlation_threshold, variance_threshold", {
  sample_csv <-
    system.file(
      "extdata", "parameters", "batch0", "sample",
      "normalized_sample.csv",
      package = "cytotools"
    )

  operations <- c("correlation_threshold", "variance_threshold")

  output_dir <- tempdir()

  preselect(
    batch_id = "batch0",
    input = sample_csv,
    operations = operations,
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
})
