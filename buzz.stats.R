'buzz.stats' <- function(dataset, threshold, units, grab) {
   
   xxx <<- dataset; xxthreshold <<- threshold; xxunits <<- units
   
   'fmt.stats' <- function(x, y)                                                          # format moment stats
      paste0(format(round(x, 1), nsmall = 1), ' (', round(y, 0), '%)')
   
   'fmt.hm' <- function(x) {                                                                # format minutes as h:mm
      x <- round(x, 0)
      paste(floor(x / 60), sprintf('%02d', x - floor(x / 60) * 60), sep = ':')
   }
   
   
   Statistic <- c('Number of points', 'Minimum', 'Maximum', 'Mean', 'Standard deviation',
                  'Number of points below CT', 
                  'Percent below CT', 
                  'Mean below CT', 
                  'Standard deviation below CT', 
                  'Shortest duration below CT', 
                  'Longest duration below CT', 
                  'Mean time below CT')
   
   Unit <- c('count', rep('mg/L (% sat.)', 4), 'count', '%', rep('mg/L (% sat.)', 2), rep('hours:mins', 3))
   Sensor <- Grab <- rep('', 12)
   
   sense.units <- dataset[, unit.vars[as.integer(units)]]                                                         # units to use for exceedance
   sense.exceed <- sense.units < threshold                                                                        # cases below comparison threshold
   
   
   # Calculate sensor stats
   Sensor[1] <- format(sum(!is.na(dataset$DO)), big.mark = ',')                                                   # sample size
   Sensor[2] <- fmt.stats(min(dataset$DO, na.rm = TRUE), min(dataset$DO_Pct_Sat, na.rm = TRUE))                   # min
   Sensor[3] <- fmt.stats(max(dataset$DO, na.rm = TRUE), max(dataset$DO_Pct_Sat, na.rm = TRUE))                   # max
   Sensor[4] <- fmt.stats(mean(dataset$DO, na.rm = TRUE), mean(dataset$DO_Pct_Sat, na.rm = TRUE))                 # mean
   Sensor[5] <- fmt.stats(sd(dataset$DO, na.rm = TRUE), sd(dataset$DO_Pct_Sat, na.rm = TRUE))                     # sd
   
   Sensor[6] <- format(sum(sense.exceed, na.rm = TRUE), big.mark = ',')                                           # n below CT
   Sensor[7] <- paste0(round(sum(sense.exceed, na.rm = TRUE) / sum(!is.na(sense.exceed)) * 100, 0), '%')          # % below CT
   if(any(sense.exceed, na.rm = TRUE)) {                                                                          # if we're below CT,
      Sensor[8] <- format(round(mean(sense.units[sense.exceed], na.rm = TRUE), 1), nsmall = 1)                    #    mean below CT
      Sensor[9] <- ifelse(sum(sense.exceed, na.rm = TRUE) == 1, '',                                               #    standard deviation below CT (only if n > 1)
                          format(round(sd(sense.units[sense.exceed], na.rm = TRUE), 1), nsmall = 1))                  
      
      se <- sense.exceed
      se[is.na(se)] <- FALSE                                                                                      # treat missing values as NOT below CT
      
      ##     sense.exceed <<- sense.exceed; dataset<<-dataset; se<<-se
      
      g <- cumsum(se & !c(FALSE, se[-length(se)])) * se                                                           # make grouping variable of runs of TRUEs
      for(i in length(g):2)                                                                                       # extend each group to the next sample so we get proper periods
         if(g[i] == 0)
            g[i] <- g[i - 1]
      d <- cbind(aggregate(dataset$Date_Time, by = list(g), FUN = 'max'), 
                 aggregate(dataset$Date_Time, by = list(g), FUN = 'min'))                                         # deltas in sec
      d <- d[d$Group.1 != 0,]                                                                                     # drop group 0
      m <- as.numeric(as.duration(d[, 2] - d[, 4])) / 60                                                          # duration below threshold in minutes
      
      Sensor[10] <- fmt.hm(min(m))                                                                                # min, max, and mean minutes below CT threshold
      Sensor[11] <- fmt.hm(max(m))
      Sensor[12] <- fmt.hm(mean(m))
   }
   
   
   # Calculate grab sample stats (if selected)
   if(grab) {                          
      grab.units <- dataset[, paste0('Grab_', unit.vars[as.integer(units)])]
      grab.exceed <- grab.units < threshold
      
      Grab[1] <- format(sum(!is.na(dataset$Grab_DO)), big.mark = ',')                                                # sample size
      Grab[2] <- fmt.stats(min(dataset$Grab_DO, na.rm = TRUE), min(dataset$Grab_DO_Pct_Sat, na.rm = TRUE))           # min
      Grab[3] <- fmt.stats(max(dataset$Grab_DO, na.rm = TRUE), max(dataset$Grab_DO_Pct_Sat, na.rm = TRUE))           # max
      Grab[4] <- fmt.stats(mean(dataset$Grab_DO, na.rm = TRUE), mean(dataset$Grab_DO_Pct_Sat, na.rm = TRUE))         # mean
      Grab[5] <- fmt.stats(sd(dataset$Grab_DO, na.rm = TRUE), sd(dataset$Grab_DO_Pct_Sat, na.rm = TRUE))             # sd
      
      Grab[6] <- format(sum(grab.exceed, na.rm = TRUE), big.mark = ',')                                              # n below CT
      Grab[7] <- paste0(round(sum(grab.exceed, na.rm = TRUE) / sum(!is.na(grab.exceed)) * 100, 0), '%')              # % below CT
      if(any(grab.exceed, na.rm = TRUE)) {                                                                           # if we're below CT,
         Grab[8] <- format(round(mean(grab.units[grab.exceed], na.rm = TRUE), 1), nsmall = 1)                        #    mean below CT
         Grab[9] <- ifelse(sum(grab.exceed, na.rm = TRUE) == 1, '',                                                  #    standard deviation below CT (only if n > 1)
                           format(round(sd(grab.units[grab.exceed], na.rm = TRUE), 1), nsmall = 1))                  
      }
      Grab[10:12] <- 'N/A'                                                                                           # no durations below CT for grab-bag
   }
   
   group <- c(rep('All aggregated datapoints', 5), rep('DO events below Comparison Threshold (CT)', 7))
   
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
