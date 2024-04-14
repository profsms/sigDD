#'Determine whether the empirical DD coefficient is significant
#'
#'Using the simulated method, calculate the p-value and determine whether empirical DD coefficient is statistically different than 0
#'@inheritParams DD_bounds
#'@param DDe The empirical value of DD coefficient, can be calculated by DDe() function
#'@export
DD_sig <- function(metric, DDe, iterations=15000, alpha=0.1, generate_plot=TRUE, print_bounds=FALSE){
  
  test_data <- test_distr(metric=metric, iterations=iterations, generate_plot=generate_plot)
  bounds <- ktd_DD_bounds(test_data=test_data, alpha=alpha, print_bounds=print_bounds, generate_plot=FALSE)
  
  if(DDe<bounds$lower[1] || DDe>bounds$upper[1]){
    sig_message <- "Empirical DD estimate is statistically significant, hence different than 0"
  }else{
    sig_message <- "Empirical DD estimate is not statistically significant, hence not different than 0"
  }
  
  
  DD_ecdf<-ecdf(test_data)
  pval <- round(2*DD_ecdf(-abs(DDe)), digits=6)
  
  print(sig_message)
  print(paste("p-value: ", pval))
  return(pval)
}