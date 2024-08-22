'buzz.aggregate' <- function(data, Site_Year, period, interval, method, moving.window, threshold) {
   
   # update aggregation for Buzzard's Bay app
   # Arguments: 
   #     data              full dataset
   #     Site_Year         selected site and year
   #     period            selected period
   #     interval          time interval, set with lubridate
   #     method            function to call (uses shorthand that's easy to test for; resolved in switch)
   #     moving.window     TRUE if moving window
   #     threshold         comparison threshold, used for percent exceedance
   # Result:
   #     dataset                 fresh dataset, aggregated for all 4 DO variables
   # B. Compton, 20 Aug 2024
   
   
   
   
   # now have full dataset, ready for aggregating
   # we have 
   #  input$interval          time period, nicely set with lubridate
   #  
   #  input$method            function
   #     R functions          min, max, mean, median, sd (all with na.rm = TRUE)
   #     percentiles          quantile(x, 0.05, na.rm = TRUE), quantile(x, 0.1, na.rm = TRUE)
   #     percent exceedance   sum(x <= input$threshold, na.rm = TRUE) / sum(!is.na(x)) * 100
   #     
   #     include functions like this: ~eval(parse(text = fn))
   #     
   #  input$moving.window     moving window or summarizing
   #     for moving window, we're taking an idiosyncratic subsample to display (regularize this?)
   #     
   #     moving window: slide_index(x, dates, function, .before = , after = )
   #     non-window: slide_period(
   #     
   #     all functions are _dbl. Probably don't need to include it, as data are already dbl
   #     it looks like I can apply it to data frames, so maybe can hit all 4 values at once?
   #     better to use built-in functions...... 
   
   
   zzz <<- list(data, Site_Year, period, interval, method, moving.window, threshold)
   
   
   dataset <- data[data$Site_Year == Site_Year &                                  # get fresh dataset
                      data$Date_Time >= period[1] & data$Date_Time <= period[2], ]

   if(interval == 0 | dim(dataset)[1] == 0)                                      # if no aggregation (or no data, thanks to recent site change), return full dataset
      return(dataset)
   
   
   method.choices = c('Mean' = 'mean', 'Minimum' = 'min', '5th percentile' = 'p5', '10th percentile' = 'p10', 'Median' = 'median', 
                      'Maximum' = 'max', 'Standard deviation' = 'sd')        #, 'Percent exceedance' = 'pe')
   
   fn <- switch(method,                                  # set function call                       
                'mean' = 'mean(.x, na.rm = TRUE)',
                'min' = 'min(.x, na.rm = TRUE)',
                'p5' = 'quantile(.x, 0.05, na.rm = TRUE)',
                'p10' = 'quantile(.x, 0.10, na.rm = TRUE)',
                'median' = 'median(.x, na.rm = TRUE)',
                'max' = 'max(.x, na.rm = TRUE)',
                'sd' = 'sd(.x, na.rm = TRUE)')
   
   halfwin <- as.period(as.duration(eval(parse(text = interval))) / 2)                    # slider wants half-windows

   vars <- c('DO', 'DO_Pct_Sat', 'Grab_DO', 'Grab_DO_Pct_Sat')
   d <- dataset
   d[d$source == 2, vars[1:2]] <- NA                                                      # nuke the imputed sensor data we added to make plots work
   
   if(moving.window) {                 # if moving window aggregation,
      for(i in vars)
         switch(method, 
                mean = {
                   d[, i] <- unlist(slide_index_mean(d[, i], dataset$Date_Time, na_rm = TRUE, before = halfwin, after = halfwin))
                },
                min = {
                   d[, i] <- unlist(slide_index_min(d[, i], dataset$Date_Time, na_rm = TRUE, before = halfwin, after = halfwin))
                },
                max = {
                   d[, i] <- unlist(slide_index_max(d[, i], dataset$Date_Time, na_rm = TRUE, before = halfwin, after = halfwin))
                },
                {
                   d[, i] <- unlist(slide_index(d[, i], dataset$Date_Time, ~eval(parse(text = fn)), .before = halfwin, .after = halfwin))
                })
   }
   else {                                    # else, aggregation summarizes
      
   }
   
   d
}
