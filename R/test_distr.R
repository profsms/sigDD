#' Generate the test distribution on given metric
#' 
#' Generate test distribution on given metric with given number if iteration
#'@param metric Vector containing metric values of each observation
#'@param iterations Number of iterations in generating the test distribution - the more, the better, but it is recommended to do at least 15'000 iterations, by default set to 15'000
#'@param generate_plot Boolean variable - set to TRUE if you want to show the density plot of test distribution, FALSE if you do not want to generate the plot; by default set to TRUE
#'@param title Title for the generated plot, empty by default
#'@param xlabel Label for the x-axis of the generated plot, empty by default
#'@param ylabel Label for the y-axis of the generated plot, 'Density' by default
#'@return Test distribution built on given metric as a numeric vector
#'@export
test_distr<-function(metric, iterations=15000, generate_plot=TRUE, title = "", xlabel = "", ylabel = "Density"){
  
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
    plot(density(testDDs), main = title, xlab = xlabel, ylab=ylabel)
  }
  
  return(testDDs)
}