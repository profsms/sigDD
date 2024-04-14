#' Calculate the rejection interval's bounds on known test distribution
#' 
#' Using the previously generated test distribution, calculate the rejection interval, generate the test distribution's plot and print out the lower and upper bounds
#'
#'@param test_data Previously generated test distribution, can be generated using 'test_distr()' function 
#'@param generate_plot Boolean variable - set to TRUE if you want to show the density plot of test distribution, FALSE if you do not want to generate the plot; by default set to TRUE
#'@param alpha The significance level for calculating the rejection interval, by default set to 0.1
#'@param print_bounds Boolean variable - set to TRUE if you want to print the calculated bounds, FALSE if you don't want to print the bounds; by default set to TRUE
#'@return Upper and lower bound of rejection interval, use 'DD_bounds(...)$lower' to print only the lower bound, 'DD_bounds(...)$upper' to print only the upper bound.
#'@export
ktd_DD_bounds<-function(test_data, generate_plot=TRUE, alpha=0.1, print_bounds=TRUE){

  if(generate_plot){
    # Plot the density of the test distribution
    plot(density(test_data), main = "", xlab = "")
  }
  
  # Calculate the upper bound of rejection interval
  quantile_upper <- quantile(test_data, 1-alpha/2)
  
  # Calculate the lower bound of rejection interval
  quantile_lower <- quantile(test_data, alpha/2)
  
  if(print_bounds){
    print(paste("Upper bound of rejection interval:", quantile_upper))
    print(paste("Lower bound of rejection interval:", quantile_lower))
  }
  
  bounds <- data.frame(
    lower = quantile_lower,
    upper = quantile_upper
  )
  
  rownames(bounds) <- paste(alpha*100, "% bounds")
  
  return(bounds)
}