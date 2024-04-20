#' Parallel Trend Plot
#'
#' This function creates a parallel trend plot to visualize trends
#' between groups over time.
#'
#' @inheritParams DD_e
#'
#' @param dateX An optional numeric value representing the time point for
#' the vertical line. Defaults to the median of `time`.
#' @param is_date A logical value (TRUE/FALSE) indicating if `time` is a date
#' vector. If TRUE, `time` should be a character vector in a recognized date
#' format. The starting date is assumed to be the first element.
#' @param control_mod A numeric value to modify the metric for the control group.
#' Defaults to 0 (no modification).
#' @param affected_mod A numeric value to modify the metric for the affected group.
#' Defaults to 0 (no modification).
#' @param equalize A logical value (TRUE/FALSE) indicating whether to equalize
#' the mean metric values of the groups.
#' @param unitize_time A logical value (TRUE/FALSE) indicating whether to unitize
#' the time axis (scale to range 0-1).
#' @param title The main title of the plot. Defaults to "Parallel Trend Plot".
#' @param xlabel The label for the x-axis. Defaults to "Time".
#' @param ylabel The label for the y-axis. Defaults to "Metric".
#' @param control_series_color The color for the control group line. Defaults to "red".
#' @param affected_series_color The color for the affected group line. Defaults to "blue".
#' @param legend_color_label The label for the color legend. Defaults to "Affected".
#'
#' @return A ggplot object representing the parallel trend plot.
#'
#' @export
parallel_trend_plot <- function(metric, time, affected,
                                dateX = median(time),
                                is_date = FALSE,
                                control_mod = 0, affected_mod = 0, 
                                equalize = FALSE, unitize_time = FALSE,
                                title = "Parallel Trend Plot",
                                xlabel = "Time",
                                ylabel = "Metric",
                                control_series_color = "red",
                                affected_series_color = "blue",
                                legend_color_label = "Affected") {
  
  # Check arguments
  if (!is.data.frame(data.frame(metric, time, affected))) {
    stop("metric, time, and affected must be vectors of equal length")
  }
  
  if(!is.numeric(time) && is_date[1] == FALSE){
    stop("time must be a numeric vector, unless is_date is set to (TRUE, starting date)")
  }
  
  if (!is.numeric(control_mod) | !is.numeric(affected_mod)) {
    stop("control_mod and affected_mod must be numeric values for group modifications")
  }
  
  if (!is.logical(equalize)) {
    stop("equalize must be logical (TRUE/FALSE) for group alignment")
  }
  
  if (!is.logical(unitize_time)) {
    stop("unitize_time must be logical (TRUE/FALSE) for time unitization")
  }
  
  if(is_date[1]==TRUE){
    time <- 1:floor((length(time)/2))
  }
  
  data <- data.frame(metric, time, affected)
  
  # Apply group modifications
  data$metric[affected == 1] <- data$metric[affected == 1] + affected_mod
  data$metric[affected == 0] <- data$metric[affected == 0] + control_mod
  
  if (equalize) {
    control_mean <- mean(data$metric[affected==0])
    affected_mean <- mean(data$metric[affected==0])
    
    data$metric <- (data$metric - (affected_mean - control_mean) * data$affected)
  }
  
  if (unitize_time) {
    data$time <- as.numeric(data$time) / max(as.numeric(data$time))
  }
  
  affected_colors <- c("red", "blue")
  
  ggplot2::ggplot(data, aes(x = time, y = metric, color = factor(affected))) +
    geom_point(size = 1) +
    geom_line(aes(linetype = factor(affected)), linewidth = 0.7) +
    geom_vline(xintercept = dateX, linetype = "dashed", color = "black", linewidth = 1) +  # Add vertical line (optional)
    labs(title = title,
         x = xlabel,
         y = ylabel,
         color = legend_color_label) +
    scale_color_manual(values = affected_colors) +
    scale_linetype_manual(values = c("solid", "solid")) +
    theme_bw()
};