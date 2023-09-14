#' Summarise FLIR Data
#'
#' This function takes a FLIR object and summarises the data by calculating various statistics such as mean, variance, median, standard deviation, standard error, and interquartile range (IQR).
#'
#' @param flir_obj A FLIR object containing temperature data.
#'
#' @return A summary data frame with the calculated statistics for the FLIR data.
#' @keywords FLIR data summary statistics summarise
#'
#' @export
get_flir_summary <- function(flir_obj) {
  id <- unique(flir_obj$flir_id)
  long_df <- flir_obj |>
    utils::stack(-flir_id)

    data.frame(
      id = id,
      mean = mean(long_df$values, na.rm = TRUE),
      variance = stats::var(long_df$values, na.rm = TRUE),
      median = stats::median(long_df$values, na.rm = TRUE),
      standard_deviation = stats::sd(long_df$values, na.rm = TRUE),
      standard_error = stats::sd(long_df$values, na.rm = TRUE) / sqrt(length(long_df$values)),
      iqr = stats::IQR(long_df$values)
    )
}

