#' Get FLIR Parameters
#'
#' This function retrieves metadata and parameters from a FLIR image.
#'
#' @param flir_csv_path_path A character vector containing the contents of a FLIR CSV file.
#' @return A data frame containing the extracted metadata and parameters.
#'
#' @keywords internal
get_flir_parameters <- function(flir_obj) {
  filename_row <- grep("File", flir_obj)
  metadata_row <- grep("Frame 1", flir_obj)
  metadata <- flir_obj[(filename_row + 1):(metadata_row - 1)]
  metadata <- gsub(",{2,}|;{2,}", "", metadata)
  metadata <- metadata[nchar(metadata) > 1]
  if(length(metadata) <=1){
  return(data.frame(parameters = "Missing parameters"))
  }
  else if(length(metadata) > 1){
    metadata <- gsub("\\,{3,}$", "", metadata)
    metadata <- metadata[nchar(metadata) > 1]
    metadata <- unique(gsub("^.", "", trimws(gsub(
      "File:|Parameters:", "",
      metadata
    ))))
    metadata_list <- strsplit(metadata, ":;|:,")

    metadata_df <- as.data.frame(do.call(rbind, metadata_list),
                                 stringsAsFactors = FALSE)
    colnames(metadata_df) <- c("parameter", "value")
    metadata_df$parameter <- gsub("\\.", "", metadata_df$parameter)
    metadata_df$parameter <- gsub("\\s", "_", metadata_df$parameter)
    metadata_df$parameter <- gsub("\\_$", "", metadata_df$parameter)
    metadata_df$parameter <- tolower(metadata_df$parameter)
    metadata_wide <- as.data.frame(matrix(metadata_df$value,
                                          ncol = length(metadata_df$parameter)))
    colnames(metadata_wide) <- metadata_df$parameter
    return(metadata_wide)
  }
}
