#' Parallel Trend Plot
#'
#' Create a parallel trend plot to visualize trends
#' between groups over time.
#'
#' @inheritParams DD_e
#' @param period A vector of dates (or a number sequence) representing the period (f.e. day) when the observation was taken
#' @param dateX An optional numeric value representing the time point for
#' the vertical line. Defaults to the median of `period`.
#' @param is_date A logical value (TRUE/FALSE) indicating if `period` is a date
#' vector. If TRUE, `period` should be a character vector in a recognized date
#' format. The starting date is assumed to be the first element.
#' @param control_mod A numeric value to modify the metric for the control group.
#' Defaults to 0 (no modification).
#' @param affected_mod A numeric value to modify the metric for the affected group.
#' Defaults to 0 (no modification).
#' @param equalize A logical value (TRUE/FALSE) indicating whether to equalize
#' the mean metric values of the groups.
#' @param unitize_period A logical value (TRUE/FALSE) indicating whether to unitize
#' the period axis (scale to range 0-1).
#' @param title The main title of the plot. Defaults to "Parallel Trend Plot".
#' @param xlabel The label for the x-axis. Defaults to "Time".
#' @param ylabel The label for the y-axis. Defaults to "Metric".
#' @param control_name The name for the control group, '0' by default
#' @param affected_name The name for the affected group, '1' by default
#' @param control_series_color The color for the control group line. Defaults to "red".
#' @param affected_series_color The color for the affected group line. Defaults to "blue".
#' @param legend_color_label The label for the color legend. Defaults to "Affected".
#'
#' @return A ggplot object representing the parallel trend plot.
#'
#' @export
parallel_trends_plot <- function(metric, period, affected,dateX = ceiling(median(period)),is_date = FALSE,control_mod = 0,affected_mod = 0,equalize = FALSE,unitize_period = FALSE,title = "Parallel Trend Plot",xlabel = "Time",ylabel = "Metric",control_series_color = "red",affected_series_color = "blue",legend_color_label = "Affected", control_name = '0', affected_name = '1'){
  
  # Check arguments
  if (!is.data.frame(data.frame(metric, period, affected))) {
    stop("metric, period, and affected must be vectors of equal length")
  }
  
  if(!is.numeric(period) && is_date == FALSE){
    stop("period must be a numeric vector, unless is_date is set to TRUE")
  }
  
  if (!is.numeric(control_mod) | !is.numeric(affected_mod)) {
    stop("control_mod and affected_mod must be numeric values for group modifications")
  }
  
  if (!is.logical(equalize)) {
    stop("equalize must be logical (TRUE/FALSE) for group alignment")
  }
  
  if (!is.logical(unitize_period)) {
    stop("unitize_period must be logical (TRUE/FALSE) for time unitization")
  }
  
  if(is_date==TRUE){
    period <- 1:floor((length(period)/2))
  }
  
  data <- data.frame(metric, period, affected)
  
  # Apply group modifications
  data$metric[affected == 1] <- data$metric[affected == 1] + affected_mod
  data$metric[affected == 0] <- data$metric[affected == 0] + control_mod
  
  if (equalize) {
    control_mean <- mean(data$metric[affected==0])
    affected_mean <- mean(data$metric[affected==0])
    
    data$metric <- (data$metric - (affected_mean - control_mean) * data$affected)
  }
  
  if (unitize_period) {
    data$period <- as.numeric(data$period) / max(as.numeric(data$period))
  }
  
  affected_colors <- c(control_series_color, affected_series_color)
  
  par_trend_plot <- ggplot2::ggplot(data, ggplot2::aes(x = period, y = metric, color = factor(affected))) +
    ggplot2::geom_point(size = 1) +
    ggplot2::geom_line(ggplot2::aes(linetype = factor(affected)), linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = dateX, linetype = "dashed", color = "black", linewidth = 1) +
    ggplot2::labs(title = title,
                  x = xlabel,
                  y = ylabel,
                  color = legend_color_label) +
    ggplot2::scale_color_manual(values = affected_colors, labels = c(control_name, affected_name)) +
    ggplot2::scale_linetype_manual(values = c("solid", "solid")) +
    ggplot2::guides(linetype="none") +
    ggplot2::theme_bw()
  
  par_trend_plot
  
  return(par_trend_plot)
}
