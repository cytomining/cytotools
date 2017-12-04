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
