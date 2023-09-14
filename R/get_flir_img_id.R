#' Get FLIR Image ID
#'
#' This function retrieves the FLIR image number from a CSV file path.
#'
#' @param flir_csv_path The path to the FLIR CSV file.
#'
#' @return The FLIR image number.
#'
#' @keywords internal
get_flir_img_id <- function(flir_obj) {
  flir_id_row <- grep("File", flir_obj)
  flir_id <- basename(flir_obj[flir_id_row])
  return(flir_id)
}
