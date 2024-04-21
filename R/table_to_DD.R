#' Convert Table to Data Frame suitable for DD modelling
#'
#' This function takes three numeric vectors representing date, seriesA, 
#' and seriesB values and converts them into a data frame suitable for modelling
#' with sigDD package
#'
#' @param period A numeric vector representing dates or periods. Can be either
#' in a date format or a simple numeric vector, with consecutive numbers representing
#' consecutive periods
#' @param seriesA A numeric vector representing values for series A.
#' @param seriesB A numeric vector representing values for series B.
#' @param dateX An optional numeric value representing the reference date 
#' for splitting the data into "before" and "after" categories. Defaults 
#' to the median of the `date` vector.
#'
#' @return A data frame with three columns:
#'   - metric: A combined vector containing values from both seriesA and seriesB.
#'   - affected: A factor indicating series (1 for seriesA, 0 for seriesB).
#'   - time: A factor indicating time period (0 for before `dateX`, 1 for after `dateX`).
#'
#' @export
table_to_DD <- function(period, seriesA, seriesB, dateX=median(1:floor((length(period))))) {
  library(dplyr)
  # Check data types
  if (!is.numeric(seriesA)) stop("Argument 'seriesA' must be numeric")
  if (!is.numeric(seriesB)) stop("Argument 'seriesB' must be numeric")
  
  # Combine data into a data frame
  df <- data.frame(period = 1:floor((length(period))), seriesA = seriesA, seriesB = seriesB)
  
  # Sort by date
  df <- df %>% dplyr::arrange(date)
  # Get the number of rows
  n <- nrow(df)
  
  # Create the result matrix
  matrix <- matrix(nrow = 2 * n, ncol = 3, byrow = FALSE)
  
  # Fill the first column with dates
  matrix[, 1] <- c(df$seriesA, df$seriesB)
  
  # Fill the second column with Boolean values (A/B)
  matrix[, 2] <- c(rep(1, n), rep(0, n))
  
  # Fill the third column with Boolean values (before/after X)
  matrix[, 3] <- c(ifelse(df$period < dateX, 0, 1), ifelse(df$period < dateX, 0, 1))
  
  colnames(matrix)<-c('metric', 'affected','time')
  
  # Return the result matrix
  return(as.data.frame(matrix))
}