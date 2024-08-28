'buzz.table' <- function(dataset, title) {
   
   # Display raw data table for Buzzard's Bay app
   # Arguments: 
   #    dataset      selected rows of dataset for sensor or grab sample data
   #    title        table title
   # B. Compton, 28 Aug 2024
   
   
   
   datatable(dataset[, c('Date_Time', 'DO', 'DO_Pct_Sat', 'Temp_CondLog')], 
             colnames = c('Date and time', 'DO (mg/L)', 'DO (% sat)', 'Temperature (C)'), 
             caption = htmltools::tags$caption(style = 'caption-side: top', HTML(paste0('<h5><b>', title, '</b></h5>'))),
             options = list(dom = 'ltipr')) |>
      formatDate('Date_Time', 'toLocaleString') |>
      formatRound('DO', 2) |>
      formatRound('DO_Pct_Sat', 2) |>
      formatRound('Temp_CondLog', 1)
}