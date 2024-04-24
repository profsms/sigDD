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
#' @param include_trend A Boolean variable that specifies whether trend should be added to the procedure's ols model, dafaults to FALSE
#'
#' @return A data frame with two columns:
#'   - t test: The t-statistic value from the linear model.
#'   - p-value: The p-value associated with the t-statistic.
#'
#' @export
rg_simple_test<- function(period,dateX,metric,affected,alpha = 0.1, include_trend=FALSE){
  
  
  data <- data.frame(period,(as.numeric(period) - dateX))
  colnames(data)<-c("period","distance")
  data$post<-0
  data$post<-ifelse(data$distance>=0,1,0)
  data$pre<-0
  data$pre<-ifelse(data$distance<=-2,1,0)
  data$unit<-ifelse(affected==1,"A","B")
  data$metric<-metric
  data$pretreat<-data$pre*affected
  data$posttreat<-data$post*affected
  
  
  if(include_trend){
    test_lm<-fixest::feols(metric~period+pretreat+posttreat|unit+period,data=data,cluster="unit")
  }else{
    test_lm<-fixest::feols(metric~pretreat+posttreat|unit+period,data=data,cluster="unit")
  }
  
  
  ttest<-(summary(test_lm)$coefficients['pretreat','t value'])
  pvalue<-(summary(test_lm)$coefficients['pretreat','Pr(>|t|)'])
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
