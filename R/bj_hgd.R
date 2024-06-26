#' Google searches of Ben & Jerry's and Haagen-Dazs
#' 
#' Data scrapped from Google Trends, describing daily search counts of "Ben & Jerry's" and "Haagen-Dazs" in Google Search.
#' It is used to demonstrate Difference-in-Difference modelling using sigDD package and to demonstrate a correct data
#' format for DD modelling. It covers time from 1 March 2020 to 1 Sep 2020. The chosen event has taken place at 3rd of June 2020.
#' The affected group are "Ben & Jerry's" searches, "Haagen-Dazs" being the control.
#' 
#' @docType data
#' 
#' @usage data(bj_hgd)
#' 
#' @keywords dataset
#' 
#' @format A DataFrame with three columns - a numeric 'metric' and Boolean 'affected' and 'time'.
#' 
#' @source \href{https://trends.google.com/trends/explore?date=2020-03-01%202020-09-01&geo=US&q=%2Fm%2F016_pl,%2Fm%2F02zy15&hl=pl}{Google Trends}