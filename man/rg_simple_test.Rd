% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rg_simple_test.R
\name{rg_simple_test}
\alias{rg_simple_test}
\title{Perform a Simple Parallel Trend Test}
\usage{
rg_simple_test(
  period,
  dateX,
  metric,
  affected,
  alpha = 0.1,
  ex_t = -1,
  additional_vars = NULL
)
}
\arguments{
\item{period}{A numeric vector representing the period (time point) for 
each observation.}

\item{dateX}{A numeric value representing the reference date (time point) 
for splitting the data into "pre-treatment" and "post-treatment" periods.}

\item{metric}{Vector containing metric values of each observation}

\item{affected}{Boolean vector indicating whether an observation was to be affected by the event - FALSE (or 0) for control group, TRUE (or 1) for experimental (affected) group.}

\item{alpha}{A numeric value (between 0 and 1) representing the significance 
level for the test. Defaults to 0.1.}

\item{ex_t}{A numeric value less than 0, representing the earliest period excluded for testing purposes, where 0 is the moment of the event}

\item{additional_vars}{A data frame (optional) containing additional variables to be included in the regression model. 
Columns of the data frame should correspond to numeric or factor variables in the main data used for the 
analysis. Note that string variables are generally not suitable for direct 
inclusion in regression models without appropriate transformation (e.g., factor conversion).}
}
\value{
A data frame with two columns:
  - t test: The t-statistic value from the linear model.
  - p-value: The p-value associated with the t-statistic.
}
\description{
This function performs a simple statistical test to assess whether 
parallel trends exist between groups over time.
}
