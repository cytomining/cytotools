context("aggregate")

test_that("`aggregate` aggregates data per well", {

  futile.logger::flog.threshold(futile.logger::WARN)

  sqlite_file <-
    system.file("extdata", "fixture_cytodata_small.sqlite",
      package = "cytotools")

  temp_test_file = 'temp_test_file.csv'

  # aggregate data and load results
  cytotools::aggregate(sqlite_file, temp_test_file)

  # load results as data.frame. If you use read_csv use as.data.frame as the
  # test fails using tibble
  aggregated <- read.csv(file.path(temp_test_file))

  results <- data.frame(
    Image_Metadata_Plate = c("SQ00015116", "SQ00015116"),
    Image_Metadata_Well = c("A01", "B01"),
    Cells_AreaShape_Area = c(9390,14189.5),
    Cells_Intensity_IntegratedIntensity_DNA = c(239.51205701963, 263.166741275229),
    Cytoplasm_AreaShape_Area = c(6676, 10475),
    Cytoplasm_Intensity_IntegratedIntensity_DNA = c(65.8515734847169, 99.8703154409304),
    Nuclei_AreaShape_Area = c(2788.0, 2542),
    Nuclei_Intensity_IntegratedIntensity_DNA = c(144.428455086425, 170.052139769308)
  )

  expect_equal(
    aggregated,
    results
  )

  # clean up: remove temp file
  file.remove(temp_test_file)
})
