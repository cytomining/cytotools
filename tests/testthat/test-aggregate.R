context("aggregate")

test_that("`aggregate` aggregates data per well", {

  futile.logger::flog.threshold(futile.logger::WARN)

  sqlite_file <-
    system.file("extdata", "fixture_cytodata_small.sqlite",
      package = "cytotools")

  temp_test_file <- "temp_test_file.csv"

  # aggregate data and load results
  aggregate(sqlite_file, temp_test_file, operation = "median")

  aggregated <-
    readr::read_csv(file.path(temp_test_file),
                    col_types = readr::cols(
                      Cells_AreaShape_Area_median = readr::col_double(),
                      Cells_Intensity_IntegratedIntensity_DNA_median =
                        readr::col_double(),
                      Cytoplasm_AreaShape_Area_median = readr::col_double(),
                      Cytoplasm_Intensity_IntegratedIntensity_DNA_median =
                        readr::col_double(),
                      Nuclei_AreaShape_Area_median = readr::col_double(),
                      Nuclei_Intensity_IntegratedIntensity_DNA_median =
                        readr::col_double())
                    )

  results <- tibble::data_frame(
    Image_Metadata_Plate = c("SQ00015116", "SQ00015116"),
    Image_Metadata_Well = c("A01", "B01"),
    Cells_AreaShape_Area_median = c(9390, 14189.5),
    Cells_Intensity_IntegratedIntensity_DNA_median =
      c(239.51205701963, 263.166741275229),
    Cytoplasm_AreaShape_Area = c(6676.0, 10475.0),
    Cytoplasm_Intensity_IntegratedIntensity_DNA_median =
      c(65.8515734847169, 99.8703154409304),
    Nuclei_AreaShape_Area = c(2788.0, 2542.0),
    Nuclei_Intensity_IntegratedIntensity_DNA_median =
      c(144.428455086425, 170.052139769308)
  )

  # TODO: currently, need to cast as.data.frame because test fails using tibble
  expect_equal(
    aggregated %>% as.data.frame(),
    results %>% as.data.frame(),
    tolerance = 10e-12,
    check.attributes = FALSE
  )

  # clean up: remove temp file
  file.remove(temp_test_file)

})

test_that("`aggregate` aggregates data per well only in specified compartments",
          {

  futile.logger::flog.threshold(futile.logger::WARN)

  sqlite_file <-
    system.file("extdata", "fixture_cytodata_small.sqlite",
                package = "cytotools")

  temp_test_file <- "temp_test_file.csv"

  # aggregate data and load results
  aggregate(sqlite_file, temp_test_file, operation = "median",
            compartments = c("cells"))

  aggregated <-
    readr::read_csv(file.path(temp_test_file),
                    col_types = readr::cols(
                      Cells_AreaShape_Area_median = readr::col_double(),
                      Cells_Intensity_IntegratedIntensity_DNA_median =
                        readr::col_double())
    )

  results <- tibble::data_frame(
    Image_Metadata_Plate = c("SQ00015116", "SQ00015116"),
    Image_Metadata_Well = c("A01", "B01"),
    Cells_AreaShape_Area_median = c(9390, 14189.5),
    Cells_Intensity_IntegratedIntensity_DNA_median =
      c(239.51205701963, 263.166741275229)
    )

  # TODO: currently, need to cast as.data.frame because test fails using tibble
  expect_equal(
    aggregated %>% as.data.frame(),
    results %>% as.data.frame(),
    tolerance = 10e-12,
    check.attributes = FALSE
  )

  # clean up: remove temp file
  file.remove(temp_test_file)

})
