#'Determine whether the empirical DD coefficient is significant
#'
#'Using the simulated method, calculate the p-value and determine whether empirical DD coefficient is statistically different than 0
#'@inheritParams ktd_DD_bounds
#'@param DDe The empirical value of DD coefficient, can be calculated by DDe() function
#'@export
ktd_DD_sig <- function(test_data, DDe, alpha = 0.1, generate_plot=TRUE, print_bounds=TRUE){
  
  bounds <- ktd_DD_bounds(test_data = test_data, alpha=alpha, generate_plot=generate_plot, print_bounds=print_bounds)
  
  if(DDe<bounds$lower || DDe>bounds$upper){
    sig_message <- "Empirical DD estimate is statistically significant, hence different than 0"
  }else{
    sig_message <- "Empirical DD estimate is not statistically significant, hence not different than 0"
  }
  
  DD_ecdf<-ecdf(test_data)
  
  pval <- round(2*DD_ecdf(-abs(DDe)), digits=6)
  
  print(paste("p-value: ", pval))
  
  return(pval)
}