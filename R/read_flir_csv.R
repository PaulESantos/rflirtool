#' Read FLIR csv
#'
#' Read csv files generated with \href{http://www.flir.com/instruments/display/?id=54865}{FLIR Tools} software.
#' Building a tidy version of the csv files, combining the metadata and data matrix.
#' `r lifecycle::badge("experimental")`
#'
#' @param file_path Name of the FLIR csv file to read from, as captured by the thermal camera. A character string.
#' @param folder_out Optional folder path to save tidy csv file.
#' @param degrees A character string defining which thermal unit will be displayed.
#'  Default us Fahrenheit degrees ("f), could be change to Celsius degrees ("c).
#'
#' @return A tibble.
#' @export
read_flir_csv <- function(file_path, folder_out = NULL, degrees = "f") {
  text <- readLines(file_path)
  text <- trimws(text[nchar(text) > 0 ])
  filename_row <- grep("^\"|File\\:", text)
  frame_row <- grep("^\"|Frame [0-9]{1,}", text)
  # metadata ------------------------------------------------------
  raw_meta <- text[(filename_row + 1):(frame_row - 1)]
  meta <- unique(gsub("^.", "", trimws(gsub(
    "File:|Parameters:", "",
    raw_meta[nchar(raw_meta) > 1]
  )))) |>
    unique()
  meta <- grep(":,", meta, value = TRUE)
  meta <- matrix(unlist(strsplit(meta, split = ":,")),
                 nrow=length(strsplit(meta, split = ":,")),
                 byrow=TRUE)
  meta_df <- t(meta[,2])
  colnames(meta_df) <- as.vector(t(meta[,1]))
  meta_df <- as.data.frame(meta_df)

  # matrix data ---------------------------------------------------

  raw_data <- text[c(frame_row: length(text))]
  raw_data <- gsub("Frame [0-9]{1,}", "", raw_data)
  raw_data <- gsub("^,|,$", "", raw_data)
  raw_data <- gsub(",", "\t", raw_data)
  clean_data <- readr::read_delim(paste(raw_data, collapse = "\n"),
                                  delim = "\t",
                                  col_names = FALSE,
                                  show_col_types = FALSE)

  #change from degres to celcius
  if(degrees == "f"){
    clean_data <- clean_data
  }
  else if(degrees == "c"){
    f_to_c <- function(x) {
      (x -32) *  0.5555556
    }
    clean_data <- f_to_c(clean_data)
  }

  matrix_dim <- paste0(dim(clean_data),
                       collapse = " - "
  )
  df <- dplyr::bind_cols(
    matrix_dim = matrix_dim,
    tibble::as_tibble(meta_df),
    clean_data
  )


  if (is.null(folder_out)) {
    return(df)
  }
  else if (!is.null(folder_out)) {

    write.csv(df,
              file = paste0(
                getwd(),"/",
                folder_out,
                "/",
                gsub(".*/", "", file_path)
              ),
              row.names = FALSE
    )
    return(df)
  }
}
