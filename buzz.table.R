'buzz.table' <- function(dataset, prefix, title) {
   
   # Display raw data table for Buzzard's Bay app
   # Arguments: 
   #    dataset      selected rows of dataset for sensor or grab sample data
   #    prefix       prefix to sample variable names ('' for sensor, 'Grab_' for grab samples)
   #    title        table title
   # B. Compton, 28 Aug 2024
   
 
   
   row.names(dataset) <- 1:dim(dataset)[1]
   
   datatable(dataset[, c('Date_Time', paste0(prefix, c('DO', 'DO_Pct_Sat', 'Temp_CondLog')))], 
             colnames = c('Date and time', 'DO (mg/L)', 'Temperature (C)'), 
             caption = htmltools::tags$caption(style = 'caption-side: top', HTML(paste0('<h5><b>', title, '</b></h5>'))),
             options = list(dom = 'ltipr')) |>
      formatDate('Date_Time', 'toLocaleString') |>
      formatRound(paste0(prefix, 'DO'), 2) |>
      formatRound(paste0(prefix, 'Temp_CondLog'), 1)
}