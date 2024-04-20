#' Google searches of Ben & Jerry's and Haagen-Dazs in non-DD (raw) form
#' 
#' Data scrapped from Google Trends, describing daily search counts of "Ben & Jerry's" and "Haagen-Dazs" in Google Search.
#' It covers time from 1 March 2020 to 1 Sep 2020. The chosen event has taken place at 3rd of June 2020.
#' The affected group are "Ben & Jerry's" searches, "Haagen-Dazs" being the control.
#' It differs from bj_hgd data, because the bj_hgd data is already converted to form ready for DD modelling using sigDD package.
#' bj_hgd_raw data can be useful for generating parallel trends plot using either sigDD package or other packages.
#' 
#' @docType data
#' 
#' @usage data(bj_hgd_raw)
#' 
#' @keywords dataset
#' 
#' @format A DataFrame with three columns - 'Date', 'BJ' and 'HaagenDazs'.
#' 
#' @source \href{https://trends.google.com/trends/explore?date=2020-03-01%202020-09-01&geo=US&q=%2Fm%2F016_pl,%2Fm%2F02zy15&hl=pl}{Google Trends}