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
   
   
   
   #  zzz <<- list(data, Site_Year, period, interval, method, moving.window, threshold)
   #  x <- buzz.aggregate(data = zzz[[1]], Site_Year = zzz[[2]], period = zzz[[3]], interval = zzz[[4]], method = zzz[[5]], moving.window = zzz[[6]], threshold = zzz[[7]])
   
   
   dataset <- data[data$Site_Year == Site_Year &                                    # get fresh dataset
                      data$Date_Time >= period[1] & data$Date_Time <= period[2], ]
   
   if(interval == 'None' | dim(dataset)[1] == 0)                                    # if no aggregation (or no data, thanks to recent site change), return full dataset
      return(dataset)
   
   
   
   fn <- switch(method,                                                             # get function call from method (smoothing uses built-in mean, min, max)                    
                'mean' = 'mean(.x, na.rm = TRUE)',
                'min' = 'suppressWarnings(min(.x, na.rm = TRUE))',
                'max' = 'suppressWarnings(max(.x, na.rm = TRUE))',
                'median' = 'median(.x, na.rm = TRUE)',
                'sd' = 'sd(.x, na.rm = TRUE)')
   
   
   intervals <- switch(interval,                                                    # get 2 ways of defining interval from interval (thanks, annoyingly inconsistent slider!) 
                       'Hourly' = list(hours(1), 'hour', 1),
                       '4 hours' = list(hours(4), 'hour', 4),
                       '8 hours' = list(hours(8), 'hour', 8),
                       '12 hours' = list(hours(12), 'hour', 12),
                       'Daily' = list(days(1), 'day', 1),
                       'Weekly' = list(weeks(1), 'week', 1),
                       'Bi-weekly' = list(weeks(2), 'week', 2),
                       'Monthly' = list(months(1), 'month', 1),
                       'Entire period' = list(years(2), 'year', 2)
   )
   
   halfwin <- as.period(as.duration(intervals[[1]]) / 2)                            # split out all of the (still annoying) interval parameters
   per <- intervals[[2]]
   every <- intervals[[3]]
   
    
   vars <- c('DO', 'DO_Pct_Sat', 'Temp_CondLog', 'Grab_DO', 'Grab_DO_Pct_Sat', 'Grab_Temp_CondLog')      # all vars to aggregate
   dataset[dataset$source == 2, vars[1:2]] <- NA                                    # nuke the imputed sensor data we added to make plots work. We don't want it contributing to aggregation; it'll be lost afterwards
   
   
   if(moving.window) {                                                              # if we're doing moving window,
      vars.summary <- vars[4:6]                                                     #    we'll do summary aggregation for grab sample data
      vars.mw <- vars[1:3]                                                          #    and then moving window for sensor data
   }
   else                                                                             # else,
      vars.summary <- vars                                                          #    we'll do summary aggregation for all data
   
   
   # summary aggregation for grab sample data always, and all data when not moving window
   data.summary <- data.frame(Date_Time = as.POSIXct(unlist(slide_period(dataset$Date_Time, dataset$Date_Time, ~mean(.x), 
                                                                         .period = per, .every = every)), tz = 'America/New_York'))         # get mean of date and time in interval
   
   data.summary$Site_Year <- dataset$Site_Year[1]
   data.summary$Source <- 1                                                         # no more imputation, so source can always be 1
   for(i in vars.summary) {
      data.summary[, i] <- unlist(slide_period(dataset[, i], dataset$Date_Time, .f = ~eval(parse(text = fn)), .period = per, .every = every))
      data.summary[data.summary[, i] %in% c(-Inf, Inf), i] <- NA                    # so min and max don't cause trouble
   }
   ds1 <<- data.summary
   data.summary <- data.summary[apply(data.summary[, vars.summary], 1, function(x) any(!is.na(x))), ]    # no point in keeping a lot of empty rows 
   ds2 <<- data.summary
   
   # moving window aggregation for sensor data if selected
   if(moving.window) {                                                              # if moving window aggregation,
      for(i in vars.mw)
         switch(method,
                mean = {
                   dataset[, i] <- unlist(slide_index_mean(dataset[, i], dataset$Date_Time, na_rm = TRUE, before = halfwin, after = halfwin))
                },
                min = {
                   dataset[, i] <- unlist(suppressWarnings(slide_index_min(dataset[, i], dataset$Date_Time, na_rm = TRUE, before = halfwin, after = halfwin)))
                   dataset[dataset[, i] == Inf, i] <- NA
                },
                max = {
                   dataset[, i] <- unlist(suppressWarnings(slide_index_max(dataset[, i], dataset$Date_Time, na_rm = TRUE, before = halfwin, after = halfwin)))
                   dataset[dataset[, i] == -Inf, i] <- NA
                },
                {
                   dataset[, i] <- unlist(slide_index(dataset[, i], dataset$Date_Time, ~eval(parse(text = fn)), .before = halfwin, .after = halfwin))
                })
      
      
      
      data.summary <- cbind(data.summary, DO = NA, DO_Pct_Sat = NA, Temp_CondLog = NA)
      dataset[vars.summary] <- NA
      dataset <- rbind(dataset, data.summary)
   }
   else
      dataset <- data.summary
   
   done.dataset <<- dataset
   
   dataset
}
