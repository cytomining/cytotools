#' Regularize backend file
#'
#' \code{regularize} regularizes backend file.
#'
#' @param input_file     Input file. CSV or SQLite only.
#' @param output_file    Output file. Same format as input_file.
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
regularize <- function(input_file, output_file) {

  input_extension <- tools::file_ext(input_file)

  output_extension <- tools::file_ext(output_file)

  if(input_extension != output_extension) {
    stop("Input and output filetypes are different.")
  }

  if(!(input_extension %in% c("csv", "sqlite"))) {
    stop("Unsupported filetype. Only csv or sqlite is supported.")
  }

  if(input_extension == "sqlite") {
    file.copy(input_file, output_file)

    db <- DBI::dbConnect(RSQLite::SQLite(), output_file)

    image <- DBI::dbGetQuery(db, "SELECT * from Image;")

    futile.logger::flog.info(
      "Stripping Image_ from column names of Image table...")

    image %<>%
      setNames(names(image) %>%
                 stringr::str_remove_all("^Image_"))

    DBI::dbRemoveTable(db, "Image")

    DBI::dbWriteTable(db, "Image", image)

    image <- DBI::dbGetQuery(db, "SELECT * from Image;")

    DBI::dbDisconnect(db)
  } else {

    input_data <- suppressMessages(readr::read_csv(input_file))

    input_data %<>%
      setNames(names(input_data) %>%
                 stringr::str_replace_all("^Image_Metadata", "Metadata"))

    input_data %>% readr::write_csv(output_file)

  }

}
