#' Calculate the rejection interval's bounds
#' 
#' Using the metric's values, calculate the rejection interval, generate the test distribution's plot and print out the lower and upper bounds
#' 
#'@inheritParams test_distr
#'@param alpha The significance level for calculating the rejection interval, by default set to 0.1
#'@param print_bounds Boolean variable - set to TRUE if you want to print the calculated bounds, FALSE if you don't want to print the bounds; by default set to TRUE
#'@return Upper and lower bound of rejection interval, use 'DD_bounds(...)$lower' to print only the lower bound, 'DD_bounds(...)$upper' to print only the upper bound.
#'@export
DD_bounds<-function(metric, iterations=15000, generate_plot=TRUE, alpha=0.1, print_bounds=TRUE){
  
  # Set n to number of metric's observations 
  n <- length(metric)
  
  # Initialize an empty vector to store DD coefficients
  testDDs <- numeric(iterations)
  
  # Generate Time and Affected vectors in a loop
  for (i in 1:iterations) {
    # Generate values from Bernoulli distribution with p=0.5
    Time <- rbinom(n, 1, 0.5)
    Affected <- rbinom(n, 1, 0.5)
    
    # Calculate DD coefficient using the differences method
    DD_coefficient <- (mean(metric[Time == 1 & Affected == 1]) - mean(metric[Time == 0 & Affected == 1])) - (mean(metric[Time == 1 & Affected == 0]) - mean(metric[Time == 0 & Affected == 0]))
    
    # Append the DD coefficient to a vector
    testDDs[i] <- DD_coefficient
  }
  
  if(generate_plot){
    # Plot the density of the test distribution
    plot(density(testDDs), main = "", xlab = "")
  }

  # Calculate the upper bound of rejection interval
  quantile_upper <- quantile(testDDs, 1-alpha/2)
  
  # Calculate the lower bound of rejection interval
  quantile_lower <- quantile(testDDs, alpha/2)
  
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