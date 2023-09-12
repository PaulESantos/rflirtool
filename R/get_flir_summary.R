#' Summarise FLIR Data
#'
#' This function takes a FLIR object and summarises the data by calculating various statistics such as mean, variance, median, standard deviation, standard error, and interquartile range (IQR).
#'
#' @param flir_obj A FLIR object containing temperature data.
#'
#' @return A summary data frame with the calculated statistics for the FLIR data.
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr summarise
#'
#' @keywords FLIR data summary statistics summarise
#'
#' @export
get_flir_summary <- function(flir_obj) {
  flir_obj |>
    tidyr::pivot_longer(-flir_id,
                        names_to = "col_name",
                        values_to = "value") |>
    dplyr::summarise(mean = base::mean(value, na.rm = TRUE),
                     variance = stats::var(value, na.rm = TRUE),
                     median = stats::median(value, na.rm = TRUE),
                     standard_deviation = stats::sd(value, na.rm = TRUE),
                     standard_error = standard_deviation / (sqrt(length(value))),
                     iqr = stats::IQR(value)
    )
}
