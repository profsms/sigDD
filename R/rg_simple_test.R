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
#' @param ex_t A numeric value less than 0, representing the earliest period excluded for testing purposes, where 0 is the moment of the event
#' @param additional_vars A data frame (optional) containing additional variables to be included in the regression model. 
#' Columns of the data frame should correspond to numeric or factor variables in the main data used for the 
#' analysis. Note that string variables are generally not suitable for direct 
#' inclusion in regression models without appropriate transformation (e.g., factor conversion).
#'
#' @return A data frame with two columns:
#'   - t test: The t-statistic value from the linear model.
#'   - p-value: The p-value associated with the t-statistic.
#'
#' @export
rg_simple_test<- function(period,dateX,metric,affected,alpha = 0.1, ex_t = -1, additional_vars = NULL){
  
  
  data <- data.frame(period,(as.numeric(period) - dateX))
  colnames(data)<-c("period","distance")
  data$post<-0
  data$post<-ifelse(data$distance>=0,1,0)
  data$pre<-0
  data$pre<-ifelse(data$distance<=ex_t-1,1,0)
  data$unit<-ifelse(affected==1,"A","B")
  data$metric<-metric
  data$pretreat<-data$pre*affected
  data$posttreat<-data$post*affected
  
  if (!is.null(additional_vars) && !is.data.frame(additional_vars)) {
    stop("additional_vars argument must be a data frame")
  }
  
  if (!is.null(additional_vars) && nrow(additional_vars) != nrow(data)) {
    stop("Number of rows in additional_vars must match the main data frame")
  }
  
  data_mod<-data.frame(metric=data$metric,
                       pretreat=data$pretreat,
                       posttreat=data$posttreat,
                       unit = data$unit,
                       period = data$period)
  
  if (!is.null(additional_vars)){
    data_mod <- cbind(data_mod, additional_vars)
  }
  
  formula_string <- paste("metric ~ pretreat + posttreat", sep=" + ")
  
  if (!is.null(additional_vars)) {
    for (var in names(additional_vars)) {
      formula_string <- paste(formula_string, paste(var, sep = "+"), sep = "+")
    }
  }
  
  formula_string <- paste(formula_string, "|unit + period", sep = "")
  formula_x <- as.formula(formula_string)
  
  test_lm<-fixest::feols(fml=formula_x,data=data_mod,cluster="unit")
  
  
  ttest<-(summary(test_lm)$coeftable['pretreat','t value'])
  pvalue<-(summary(test_lm)$coeftable['pretreat','Pr(>|t|)'])
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
