context("sample")

test_that("`sample` pulls all replicates", {
  sampled_csv <- tempfile("normalized_sample", fileext = ".csv")

  sample(
    batch_id = "batch0",
    pattern = "_normalized.csv$",
    output = sampled_csv,
    workspace_dir = system.file("extdata", package = "cytotools")
  )

  expected_csv <-
    system.file(
      "extdata", "parameters", "batch0", "sample",
      "normalized_sample.csv",
      package = "cytotools"
    )

  expected <- readr::read_csv(expected_csv)

  actual <- readr::read_csv(sampled_csv)

  expect_equal(actual, expected)

  file.remove(sampled_csv)
})

test_that("`sample` pulls N replicates", {
  sampled_csv <- tempfile("normalized_sample", fileext = ".csv")

  sample(
    batch_id = "batch0",
    pattern = "_normalized.csv$",
    output = sampled_csv,
    replicates = 1,
    workspace_dir = system.file("extdata", package = "cytotools")
  )

  actual <- readr::read_csv(sampled_csv)

  # Assert we sampled one replicate (one plate)
  expect_equal(length(unique(actual$Metadata_Plate)), 1)

  # Assert we sampled the whole plate
  expect_equal(dim(actual)[1], 30)

  file.remove(sampled_csv)
})
