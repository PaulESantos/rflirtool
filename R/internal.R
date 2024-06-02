#' Get FLIR Image ID
#'
#' This function retrieves the FLIR image number from a CSV file path.
#'
#' @param flir_obj The path to the FLIR CSV file.
#'
#' @return The FLIR image number.
#'
#' @keywords internal


get_flir_img_id <- function(flir_obj) {
  flir_id_row <- grep("File|Archivo",
                      flir_obj)
  flir_id <- basename(flir_obj[flir_id_row])
  return(gsub("\\..*$", "", flir_id))
}


#' Get FLIR Parameters
#'
#' This function retrieves metadata and parameters from a FLIR image.
#'
#' @param flir_obj A character vector containing the contents of a FLIR CSV file.
#' @return A data frame containing the extracted metadata and parameters.
#'
#' @keywords internal
get_flir_parameters <- function(flir_obj) {

  filename_row <- grep("File|Archivo", flir_obj)

  metadata_row <- grep("Frame 1", flir_obj)

  metadata <- flir_obj[(filename_row + 1):(metadata_row - 1)]

  metadata <- gsub(",{2,}|;{2,}", "", metadata)

  metadata <- metadata[nchar(metadata) > 1]

  if (length(metadata) <= 1) {
    return(data.frame(parameters = "Missing parameters"))
  } else {
    metadata <- gsub("\\,{3,}$", "", metadata)

    metadata <- metadata[nchar(metadata) > 1]

    metadata <- unique(gsub("^.",
                            "",
                            trimws(gsub("File:|Parameters:|Par\u00e1metros:|Archivo:", "", metadata))))

    metadata_list <- strsplit(metadata, ":;|:,")

    metadata_df <- as.data.frame(do.call(rbind,
                                         metadata_list),
                                 stringsAsFactors = FALSE)
    colnames(metadata_df) <- c("parameter", "value")
    metadata_df$parameter <- gsub("\\.", "", metadata_df$parameter)
    metadata_df$parameter <- gsub("\\s", "_", metadata_df$parameter)
    metadata_df$parameter <- gsub("\\_$", "", metadata_df$parameter)
    metadata_df$parameter <- tolower(metadata_df$parameter)
    metadata_wide <- as.data.frame(matrix(metadata_df$value,
                                          ncol = length(metadata_df$parameter)))

    colnames(metadata_wide) <- iconv(metadata_df$parameter, to = "ASCII//TRANSLIT")
    parameters <- metadata_wide
    return(metadata_wide)
  }
}
