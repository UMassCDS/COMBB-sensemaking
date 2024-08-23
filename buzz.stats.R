'buzz.stats' <- function(dataset) {
   
   # Statistic <- c('Number of Data Points/Aggregated Metrics in Time Period', 'Minimum Value', 'Maximum Value', 'Mean Value', 'Standard Deviation',
   #                'Number of Data Points/Aggregated Metrics Below Comparison Threshold', 'Percent of Data Points/Aggregated Metrics Below Comparison Threshold', 
   #                'Mean Value of Data/Aggregated Metrics Below Comparison Threshold', 'Standard Deviation of Data/Aggregated Metrics Below Comparison Threshold', 
   #                'Shortest Duration Below Comparison Threshold', 'Longest Duration Below Comparison Threshold', 'Mean Time Below Comparison Threshold')
   
   # Statistic <- c('Number of points', 'Minimum', 'Maximum', 'Mean', 'Standard deviation',
   #                'Number of points below Comparison Threshold', 
   #                'Percent of points below Comparison Threshold', 
   #                'Mean below Comparison Threshold', 
   #                'Standard deviation below Comparison Threshold', 
   #                'Shortest duration below Comparison Threshold', 
   #                'Longest duration below Comparison Threshold', 
   #                'Mean time below Comparison Threshold')
   
   Statistic <- c('Number of points', 'Minimum', 'Maximum', 'Mean', 'Standard deviation',
                  'Number of points below CT', 
                  'Percent below CT', 
                  'Mean below CT', 
                  'Standard deviation below CT', 
                  'Shortest duration below CT', 
                  'Longest duration below CT', 
                  'Mean time below CT')
   
   
   
   #Unit <- c('count', rep('mg/L (% saturation)', 4), 'count', '%', rep('mg/L (% saturation)', 2), rep('minutes', 3))
   Unit <- c('count', rep('mg/L (% sat.)', 4), 'count', '%', rep('mg/L (% sat.)', 2), rep('minutes', 3))
   Sensor <- Grab <- rep('', 12)
   
   Sensor[1] <- 150
   Sensor[2] <- '2.1 (56%)'
   Sensor[7] <- '35%'
   
   Grab[1] <- 15
   Grab[2] <- '4.7 (85%)'
   Grab[7] <- '20%'
   Grab[10:12] <- 'N/A'
   
   stats <- data.frame(Statistic, Unit, Sensor, Grab)
   
   
   
   gt(stats) |>
     # tab_header(title = 'Summary Data Shown in the Plot Above') |>
      #  fmt_date(columns = date, date_style = "wd_m_day_year") |>
      # fmt_number(columns = volume, suffixing = TRUE) |>
      cols_label(Grab = 'Grab-bag') |>
      cols_align('center', columns = 'Unit') |>
      cols_align('right', columns = c('Sensor', 'Grab')) |>
      tab_row_group(label = 'Low DO events', rows = 6:12) |>
      tab_row_group(label = 'All aggregated datapoints', rows = 1:5) |>
      data_color(columns = 'Sensor', palette = '#BCB2FF', apply_to = 'fill') |>
      data_color(columns = 'Grab', palette = '#FDC17B', apply_to = 'fill')
   
}