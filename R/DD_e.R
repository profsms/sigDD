#'Calculate the empirical Difference-in-Differences coefficient
#'
#'Given metric values, a Boolean vector indicating whether an observation belongs to control or experimental group and another Boolean vector indicating whether an observation was taken pre-event or post-event, calculate the empirical DD coefficient.
#'@param metric Vector containing metric values of each observation
#'@param time Boolean vector indicating whether an observation was taken pre-event or post-event - FALSE (or 0) if the observation was taken before the event took place, TRUE (or 1) if the observation was taken after the event took place.
#'@param affected Boolean vector indicating whether an observation was to be affected by the event - FALSE (or 0) for control group, TRUE (or 1) for experimental (affected) group.
#'@return The numerical value of Difference-in-Differences coefficient
#'@export
DD_e <- function(metric, time, affected){

  if(length(metric) != length(time) || length(metric) != length(affected)) {
    stop("All input vectors must be of the same length.")
  }
  
  DD_coefficient <- (mean(metric[!time & affected]) - mean(metric[time & affected]))-(mean(metric[!time & !affected]) - mean(metric[time & !affected]))
  
  return(DD_coefficient)
}