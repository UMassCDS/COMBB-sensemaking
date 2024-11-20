'buzz.stats' <- function(dataset, threshold, grab) {
   
   # Produce summary stats table for Buzzard's Bay app
   # Arguments: 
   #    dataset      sensor and grab sample data frame
   #    threshold    comparison threshold
   #    grab         if showing grab samples
   # B. Compton, 23 Aug 2024
   
   
   
   'fmt.stats' <- function(x)                                                             # format moment stats
      paste0(format(round(x, 1), nsmall = 1))
   
   'fmt.hm' <- function(x) {                                                              # format minutes as h:mm
      x <- round(x, 0)
      paste(floor(x / 60), sprintf('%02d', x - floor(x / 60) * 60), sep = ':')
   }
   
   
   Statistic <- c('Number of data points', 'Minimum', 'Maximum', 'Mean', 'Standard deviation',
                  'Number of points below CT', 
                  'Percent below CT', 
                  'Mean below CT', 
                  'Standard deviation below CT', 
                  'Longest duration below CT')
   
   Unit <- c('count', rep('mg/L', 4), 'count', '%', rep('mg/L', 2), 'hours:mins')
   Sensor <- Grab <- rep('', 10)
   
   
   
   # Calculate sensor stats
   sense.data <- dataset[dataset$Source == 1,]
   sense.units <- sense.data[, 'DO']                                                                                 # units to use for exceedance
   sense.exceed <- sense.units < threshold                                                                           # cases below comparison threshold
   
   Sensor[1] <- format(sum(!is.na(sense.data$DO)), big.mark = ',')                                                   # sample size
   Sensor[2] <- fmt.stats(min(sense.data$DO, na.rm = TRUE))                                                          # min
   Sensor[3] <- fmt.stats(max(sense.data$DO, na.rm = TRUE))                                                          # max
   Sensor[4] <- fmt.stats(mean(sense.data$DO, na.rm = TRUE))                                                         # mean
   Sensor[5] <- fmt.stats(sd(sense.data$DO, na.rm = TRUE))                                                           # sd
   
   Sensor[6] <- format(sum(sense.exceed, na.rm = TRUE), big.mark = ',')                                              # n below CT
   Sensor[7] <- paste0(round(sum(sense.exceed, na.rm = TRUE) / sum(!is.na(sense.exceed)) * 100, 0), '%')             # % below CT
   if(any(sense.exceed, na.rm = TRUE)) {                                                                             # if we're below CT,
      Sensor[8] <- format(round(mean(sense.units[sense.exceed], na.rm = TRUE), 1), nsmall = 1)                       #    mean below CT
      Sensor[9] <- ifelse(sum(sense.exceed, na.rm = TRUE) == 1, '',                                                  #    standard deviation below CT (only if n > 1)
                          format(round(sd(sense.units[sense.exceed], na.rm = TRUE), 1), nsmall = 1))                  
      
      if(dim(sense.data)[1] > 1) {
         se <- sense.exceed
         se[is.na(se)] <- FALSE                                                                                      # treat missing values as NOT below CT
         
         g <- cumsum(se & !c(FALSE, se[-length(se)])) * se                                                           # make grouping variable of runs of TRUEs
         for(i in length(g):2)                                                                                       # extend each group to the next sample so we get proper periods
            if(g[i] == 0)
               g[i] <- g[i - 1]
         d <- cbind(aggregate(sense.data$Date_Time, by = list(g), FUN = 'max'), 
                    aggregate(sense.data$Date_Time, by = list(g), FUN = 'min'))                                      # deltas in sec
         d <- d[d$Group.1 != 0,]                                                                                     # drop group 0
         m <- as.numeric(as.duration(d[, 2] - d[, 4])) / 60                                                          # duration below threshold in minutes
         m <- m[m != 0]                                                                                              # this can happen if the last point is below the threshold
         
         if(length(m) > 0) {
            Sensor[10] <- fmt.hm(max(m))                                                                            # max minutes below CT threshold
         }
      }
   }
   
   
   # Calculate grab sample stats (if selected)
   if(grab) {                          
      grab.units <- dataset$Grab_DO
      grab.exceed <- grab.units < threshold
      
      Grab[1] <- format(sum(!is.na(dataset$Grab_DO)), big.mark = ',')                                                # sample size
      Grab[2] <- fmt.stats(min(dataset$Grab_DO, na.rm = TRUE))                                                       # min
      Grab[3] <- fmt.stats(max(dataset$Grab_DO, na.rm = TRUE))                                                       # max
      Grab[4] <- fmt.stats(mean(dataset$Grab_DO, na.rm = TRUE))                                                      # mean
      Grab[5] <- fmt.stats(sd(dataset$Grab_DO, na.rm = TRUE))                                                        # sd
      
      Grab[6] <- format(sum(grab.exceed, na.rm = TRUE), big.mark = ',')                                              # n below CT
      Grab[7] <- paste0(round(sum(grab.exceed, na.rm = TRUE) / sum(!is.na(grab.exceed)) * 100, 0), '%')              # % below CT
      if(any(grab.exceed, na.rm = TRUE)) {                                                                           # if we're below CT,
         Grab[8] <- format(round(mean(grab.units[grab.exceed], na.rm = TRUE), 1), nsmall = 1)                        #    mean below CT
         Grab[9] <- ifelse(sum(grab.exceed, na.rm = TRUE) == 1, '',                                                  #    standard deviation below CT (only if n > 1)
                           format(round(sd(grab.units[grab.exceed], na.rm = TRUE), 1), nsmall = 1))                  
      }
      Grab[10] <- ''                                                                                                 # no durations below CT for grab-bag
   }
   
   group <- c(rep('All aggregated datapoints', 5), rep('DO events below Comparison Threshold (CT)', 5))
   
   
   xxx <<- list(group = group, Statistic = Statistic, Unit = Unit, Sensor = Sensor)
   stats <- data.frame(group, Statistic, Unit, Sensor)
   if(grab)
      stats$Grab <- Grab
   stats <- group_by(stats, group)
   
   st <- gt(stats) |>
      tab_header(title = 'Summary statistics for chosen Time Period') |>
      cols_width(group ~ px(100)) |>
      tab_options(row_group.as_column = TRUE) |>
      cols_align('center', columns = c('group', 'Unit')) |>
      cols_align('right', columns = c('Sensor')) |>
      data_color(columns = 'Sensor', palette = '#BCB2FF', apply_to = 'fill')
   
   if(grab) {
      st <- cols_label(st, Grab = 'Grab samples') |>
         cols_align('right', columns = 'Grab') |>
         data_color(columns = 'Grab', palette = '#FDC17B', apply_to = 'fill')
   }
   
   st
}
