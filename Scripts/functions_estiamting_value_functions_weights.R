
# make_domin_lookup ----
#' Create a lookup table for DOMIN values based on ranges
make_domin_lookup <- function(value_data, summary_func = median) {
  # Define fixed DOMIN ranges
  domin_ranges <- list(
    "0% Absent"                  = function(x) x == 0,
    "< 4% ( Rare - 1 Few individuals)"         = function(x) x < 4,
    "< 4% ( Rare - 2 Several individuals)"     = function(x) x < 4,
    "< 4% ( Rare - 3 Many individuals)"        = function(x) x < 4,
    "4 - 10% (Rare - 4)"           = function(x) x >= 4 & x < 10,
    "10 – 25% ( Occasional - 5)"          = function(x) x >= 10 & x < 25,
    "26 – 33% (Frequent - 6)"          = function(x) x >= 25 & x < 33,
    "33 – 50% (Frequent - 7)"          = function(x) x >= 33 & x < 50,
    "50 – 75% (Abundant - 8)"          = function(x) x >= 50 & x < 75,
    "75 – 90% (Dominant - 9)"          = function(x) x >= 75 & x < 90,
    "90 – 100% (Dominant - 10)"        = function(x) x >= 90
  )
  
  # Apply the summary function (default = median) to each DOMIN class range
  domin_values <- sapply(names(domin_ranges), function(class) {
    range_filter <- domin_ranges[[class]]
    values <- value_data$value_plot[range_filter(value_data$measure)]
    summary_func(values, na.rm = TRUE)
  })
  
  # Return lookup table
  data.frame(
    domin = names(domin_ranges),
    value = domin_values,
    row.names = NULL
  )
}
