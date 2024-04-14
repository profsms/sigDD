#'Generate the differences table as a 'gt' object
#'
#'Generate the visualization of calculation process using differences table with customized headings and group names
#'@param metric Vector containing metric values of each observation
#'@param time Boolean vector indicating whether an observation was taken pre-event or post-event - FALSE (or 0) if the observation was taken before the event took place, TRUE (or 1) if the observation was taken after the event took place.
#'@param affected Boolean vector indicating whether an observation was to be affected by the event - FALSE (or 0) for control group, TRUE (or 1) for experimental (affected) group.
#'@param post_event_name Name for post-event subgroup in the differences table, set to 'post-event' by default
#'@param pre_event_name Name for pre-event subgroup in the differences table, set to 'pre-event' by default
#'@param affected_group_name Name for affected subgroup in the differences table, set to 'experimental' by default
#'@param control_group_name Name for control subgroup in the differences table, set to 'control' by default
#'@param time_header Header name for the variable describing whether an observation was taken pre-event or post-event, set to 'Time' as default
#'@param affected_header Header name for the variable describing whether an observation belongs to control group or affected group, set to 'Affected' as default
#'@return 'gt' object containing publication-ready visualization of DD calculations using differences table
#'@export
DD_table<-function(metric,
                time,
                affected,
                post_event_name = "post-event",
                pre_event_name="pre-event",
                affected_group_name="Experimental",
                control_group_name="Control",
                time_header = "Time",
                affected_hader = "Affected"
                ){
  
  dd_matrix <- matrix(nrow = 3, ncol = 3)
  
  rownames(dd_matrix) <-c(post_event_name, pre_event_name, "Difference")
  
  colnames(dd_matrix) <- c(affected_group_name, control_group_name, 'Difference')
  
  dd_matrix[2, 2] <- mean(metric[!time & !affected])
  
  dd_matrix[2, 1] <- mean(metric[!time & affected])
  
  dd_matrix[1, 1] <-mean(metric[time & affected])

  dd_matrix[1, 2] <- mean(metric[time & !affected])

  dd_matrix[, 3] <- dd_matrix[, 1] - dd_matrix[, 2]
  dd_matrix[3, ] <- dd_matrix[1, ] - dd_matrix[2, ]
  
  ddtibble1 <- as_tibble(dd_matrix[1:2,])
  
  ddtibble1["RowNames"] = c(post_event_name,
                            pre_event_name)
  
  dd_table <- gt::gt(ddtibble1, rowname_col = "RowNames")
  dd_table <- 
    dd_table |>
    tab_header(
      title = md("**DD Estimation**")
    )
  
  dd_table <-
    dd_table |>
    tab_row_group(
      label = time_header,
      rows = everything()
    )
  
  dd_table <- 
    dd_table |>
    tab_spanner(
      label = affected_hader,
      columns = c(Wysoka, Niska)
    )
  
  
  dd_table <- 
    dd_table |>
    grand_summary_rows(
      columns = everything(),
      fns = list(
        "Difference" = ~ .[1] - .[2]
      )
    )

  return(dd_table)
}