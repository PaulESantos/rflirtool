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
  id <- flir_obj$flir_id |> unique()
  data_vars <- grep("temp_[0-9]{1,}$",
                    names(flir_obj),
                    value = TRUE)

  long_df <- utils::stack(flir_obj[,data_vars])

  data.frame(
    flir_id = id,
    mean = mean(long_df$values, na.rm = TRUE),
    variance = stats::var(long_df$values, na.rm = TRUE),
    median = stats::median(long_df$values, na.rm = TRUE),
    standard_deviation = stats::sd(long_df$values, na.rm = TRUE),
    standard_error = stats::sd(long_df$values, na.rm = TRUE) / sqrt(length(long_df$values)),
    iqr = stats::IQR(long_df$values),
    min = min(long_df$values, na.rm = TRUE),
    max = max(long_df$values, na.rm = TRUE),
    kurtosis = moments::kurtosis(long_df$values, na.rm = TRUE),
    skewness = moments::skewness(long_df$values, na.rm = TRUE)
  ) |>
    unique()
}
