context("regularize")

test_that("`regularize` regularizes SQLite file", {
  futile.logger::flog.threshold(futile.logger::WARN)

  output_file <- tempfile("SQ00015116_regularized", fileext = ".sqlite")

  input_file <-
    system.file(
      "extdata", "backend", "batch0", "SQ00015116",
      "SQ00015116.sqlite",
      package = "cytotools"
    )

  regularize(input_file, output_file)

  db <- DBI::dbConnect(RSQLite::SQLite(), input_file)

  input_image <- DBI::dbGetQuery(db, "SELECT * from Image;")

  DBI::dbDisconnect(db)

  db <- DBI::dbConnect(RSQLite::SQLite(), output_file)

  output_image <- DBI::dbGetQuery(db, "SELECT * from Image;")

  DBI::dbDisconnect(db)

  input_image %<>%
    setNames(names(input_image) %>%
               stringr::str_remove_all("^Image_"))

  expect_equal(output_image, output_image)

  file.remove(output_file)

})

test_that("`regularize` regularizes CSV file", {
  futile.logger::flog.threshold(futile.logger::WARN)

  output_file <- tempfile("SQ00015116_regularized", fileext = ".csv")

  input_file <-
    system.file(
      "extdata", "backend", "batch0", "SQ00015116",
      "SQ00015116.csv",
      package = "cytotools"
    )

  regularize(input_file, output_file)

  input_data <- readr::read_csv(input_file)

  output_data <- readr::read_csv(output_file)

  input_data %<>%
    setNames(names(input_data) %>%
               stringr::str_remove_all("^Image_"))

  expect_equal(input_data, output_data)

  file.remove(output_file)

})
