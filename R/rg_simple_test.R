#' Perform a Simple Parallel Trend Test
#'
#' This function performs a simple statistical test to assess whether 
#' parallel trends exist between groups over time.
#'
#' @param period A numeric vector representing the period (time point) for 
#' each observation.
#' @param dateX A numeric value representing the reference date (time point) 
#' for splitting the data into "pre-treatment" and "post-treatment" periods.
#' @inheritParams DD_e
#' @param alpha A numeric value (between 0 and 1) representing the significance 
#' level for the test. Defaults to 0.1.
#' @param additional_variable_matrix An optional matrix (default is a 1x1 
#' empty matrix) allowing for inclusion of additional independent variables 
#' in the test model. The number of rows must either be 1 (empty matrix) or 
#' equal to the length of the `metric` vector.
#'
#' @return A data frame with two columns:
#'   - t test: The t-statistic value from the linear model.
#'   - p-value: The p-value associated with the t-statistic.
#'
#' @export
rg_simple_test<- function(period,dateX,metric,time,affected,alpha = 0.1,additional_variable_matrix=matrix(nrow=1,ncol=1), include_trend=FALSE){
  
  
  if(nrow(additional_variable_matrix)!=1 && nrow(additional_variable_matrix)!=length(metric)){
    stop("additional_variable_matrix must be either an empty (default) matrix with only 1 row, if there are no other independent variables to be included in the test, or a matrix number of rows equal to number of observation in metric vector")
  }
  
  if(include_trend){
    if(nrow(additional_variable_matrix)==1){
      additional_variable_matrix<-data.frame(period)
    }else{
      additional_variable_matrix<-as.data.frame(additional_variable_matrix)
      additional_variable_matrix$trend <- period    
    }
  }
  
  pretrend <- (abs(time - 1))*affected
  posttrend <- time*affected
  notaffected <- abs(affected-1)
  if(nrow(additional_variable_matrix)==1){
    data<-data.frame(metric,pretrend,posttrend)
  } else{
    data<-data.frame(metric,pretrend,posttrend,as.data.frame(additional_variable_matrix))
  }
  
  data$pretrend[period==dateX-1]<-0
  data$posttrend[period==dateX-1]<-0
  test_lm<-lm(formula='metric~ .',data=data)
  
  ttest<-(summary(test_lm)$coefficients['pretrend','t value'])
  pvalue<-(summary(test_lm)$coefficients['pretrend','Pr(>|t|)'])
  results <-data.frame(ttest,pvalue)
  colnames(results)<-c('t test', 'p-value')
  if(results$'p-value' < alpha){
    print("The null hypothesis is rejected and the alternative hypothesis is confirmed, the trends are not parallel")
  } else{
    print("The null hypothesis is confirmed, the trends are parallel")
  }
  
  print(paste("p-value: ", results$'p-value'))
  
  return(results)
}
