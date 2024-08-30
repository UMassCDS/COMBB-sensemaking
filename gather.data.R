'gather.data' <- function(source = 'c:/Work/DataCore/BuzzBay/SeemasData/') {
   
   # collect data for buzzbay and build a single file
   # Arguments:
   #     source               source directory
   #     'inst/sitenames.csv' Seema's sitenames, with additional include (TRUE or FALSE) and
   #                          years (comma-separated vector of available years). Excel file
   #                          is constructed from site and year.
   # Source data:
   #     '*.xlsx'             sensor or grab sensor data files. We assume that data are in the
   #                          first sheet of each file. Column names are a mess. We rename 
   #                          them to result names.
   # Result:
   #     'inst/data.RDS'      file containing a list with:
   #                             (1) a data frame with target variables for each site 
   #                             included in sitenames.csv and each year listed
   #        Result columns:      Site_Year         site & year (site description - year)
   #                             Date_Time         date & time of each obs (year-month-day h:m:s)
   #                             DO                DO from sensors (mg/L)
   #                             DO_Pct_Sat        DO from sensors (% saturation)
   #                             Temp_CondLog      temperature (C)
   #                             Grab_DO           DO from grab sensors (mg/L)
   #                             Grab_DO_Pct_Sat   DO from grab sensors (% saturation)
   # Notes: (1) if a sensor/grab sample file matches more than one column name variant, the first one
   #            listed in col.names will be chosen. (This is what happens when your data are a mess. Grr.)
   #        (2) sensor files are .xlsx, grab sensor files are .xls
   #        (3) grab sensor files have two TIME columns. I'm just using the first - I think they're the same.   
   #        (4) grab sensor data don't have negative values, so I'm not cleaning these up
   #        (5) sensor and grab sensor data are combined, with shared site/year and date/time columns. Sensor DO 
   #            values are imputed to prevent breaks in sensor data lines when plotted; will be dropped when calculating
   #        (6) if run in the winter, we might lose DST and be off by an hour
   # B. Compton, 31 Jul 2024
   
   
   
   library(readxl)
   library(dplyr)
   library(lubridate)
   
   
   col.names <- list(list('Site_Year', 'Site_Year'),                            # standard col name, variants
                     list('Date_Time', c('SAMP_DATE_TIME', 'Date Time')), 
                     list('DO', c('DO_MGL2', 'DO_mgl', 'DO_MGL')),
                     list('DO_Pct_Sat', c('DP_SAT', 'DO_SAT')),
                     list('Temp_CondLog', c('Temp_C_condlogger', 'TEMP_C')))
   
   
   'fmt.date' <- function(x) substr(format(as.POSIXct(x)), 1, 10)
   
   
   sites <- read.csv('inst/sitenames.csv')
   sites$years <- format(sites$years)                             # because years might be in the form "2021, 2022"
   sites <- sites[sites$include == TRUE,]
   sites <- sites[order(sites$description),]
   
   sy <- data.frame(matrix(NA, 0, 4))
   z <- g <- matrix(NA, 0, 5)                                     # empty site and grab sensor matrices
   q <- rep(NA, dim(z)[2])
   for(i in 1:length(col.names)) q[i] <- col.names[[i]][[1]]
   colnames(z) <- q
   
   
   for(i in 1:dim(sites)[1]) {                                    # for each site,
      y <- as.integer(unlist(strsplit(sites$years[i], ',')))      #    years
      
      
      # --- First, get sensor data ---
      for(j in y) {                                               #    for each year
         sy <- rbind(sy, c(sites$site[i], sites$description[i], j, paste0(sites$description[i], ' - ', j)))
         
         path <- paste0(source, 'location ', sites$site[i], '/')
         f <- paste0(sites$site[i], '_sensor_', j, '.xlsx')       #    sensor filename
         cat('\nSensor data file: ', f, '\n\n', sep = '')
         x <- suppressWarnings(read_excel(paste0(path, f), sheet = 1))
         x$Site_Year <- paste0(sites$description[i], ' - ', j)
         
         n <- names(x)                                            #       find columns
         c <- rep(NA, length(col.names))
         
         for(k in 1:length(col.names)) {
            d <- match(col.names[[k]][[2]], n)                    #       match possible name variants
            d <- d[!is.na(d)]
            if(length(d) == 0) stop('Column ', col.names[[k]][[1]], ' not found\n')
            c[k] <- d[1]                                          #       first matching column name
         }
         
         t <- data.frame(standard = q, found.column = names(x)[c])
         print.data.frame(t[-1,], row.names = FALSE)
         
         x <- x[, c]                   # don't let rbind's miserable name matching mess us up
         names(x) <- q
         
         b <- sum(x$DO < 0, na.rm = TRUE) + sum(x$DO_Pct_Sat < 0, na.rm = TRUE)  # set negative values to NA
         x$DO[x$DO < 0] <- NA
         x$DO_Pct_Sat[x$DO_Pct_Sat < 0] <- NA
         
         cat('\n', format(dim(x)[1], big.mark = ','), ' cases; date range = ', fmt.date(min(x$Date_Time)), ' - ', fmt.date(max(x$Date_Time)), sep = '')
         if(b > 0)
            cat(' ', b, ' negative DO value', 's'[b != 1], ' set to missing.', sep = '')
         cat('\n\n')
         
         date.range <- c(min(x$Date_Time), max(x$Date_Time))      #       save date range to trim grab sensor data
         z <- rbind(z, x)
         
         
         # --- Now, get grab sensor data ---
         f <- paste0(sites$site[i], '_grab_', j, '.xls')          #     grab sensor filename. grab sensor files have consistent column names (so far)
         cat('grab sensor data file: ', f, '\n', sep = '')
         x <- suppressWarnings(read_excel(paste0(path, f), sheet = 1, .name_repair = 'minimal'))  #       there are 2 time columns. We just want the first one
         x$Site_Year <- paste0(sites$description[i], ' - ', j)
         
         x$Date_Time <- paste(unlist(lapply(x$SAMP_DATE, substr, 1, 10)), unlist(lapply(x$TIME, substr, 12, 99))) #       combine date and time
         
         x <- x[,c('Site_Year', 'Date_Time', 'DO_MGL', 'DO_SAT', 'TEMP_C')] #       pull columns in correct order
         names(x) <- unlist(lapply(col.names, `[[`, 1))       #       and apply standard names
         
         suppressWarnings(x$DO <- as.numeric(x$DO))               #       these come in as character
         x$DO_Pct_Sat <- x$DO_Pct_Sat * 100                       #       remove annoying % from DO_Pct_Sat (it gets interpreted as proportion, not percent)
         
         cat(format(dim(x)[1], big.mark = ','), ' cases; date range = ', fmt.date(min(x$Date_Time)), ' - ', fmt.date(max(x$Date_Time)), '\n', sep = '')
         
         x <- x[x$Date_Time >= date.range[1] & x$Date_Time <= date.range[2], ]
         cat('grab sensor dates trimmed to ', fmt.date(date.range[1]), ' - ', fmt.date(date.range[2]), '; ', format(dim(x)[1], big.mark = ','), ' cases.\n\n', sep = '')
         cat('---\n')
         
         g <- rbind(g, x)
      }
   }
   
   
   colnames(sy) <- c('site', 'description', 'year', 'Site_Year')
   saveRDS(sy, 'inst/Site_Year.RDS')
   
   
   # Combine sensor and grab sensor data into a single data frame, with Site_Year, Date_Time, DO, DO_Pct_Sat, Temp_CondLog, Grab_DO, Grab_DO_Pct_Sat, and Source
   # Source is 1 for sensors, and 2 for grab sensors
   
   z$Source <- 1
   g$Source <- 2
  
   z$Date_Time <- z$Date_Time <- force_tz(z$Date_Time, 'America/New_York')          # make sure we're in EDT
   g$Date_Time <- as.POSIXct(g$Date_Time, tz = 'America/New_York')
   
   names(g)[c(3:5)] <- paste0('Grab_', names(g)[c(3:5)])          # rename grab sensor DO and temp columns
   
   z <- bind_rows(z, g)                                           # combine the two datasets
   z$Site_Year <- as.factor(z$Site_Year)                          # Site_Year as factor, of course
   
   z <-z |> 
      group_by(Site_Year, Date_Time, Source) |>                   # sort (include source so sensor data is imputed properly when grab sample data is collected at
      arrange(.by_group = TRUE)                                   # exact same time as sensor data, which does happen at least once)
   
   for(i in 1:dim(z)[1]) {                                        # impute sensor rows where NA is inserted to represent grab sensor data by copying previous value
      if(z$Source[i] == 2 & is.na(z$DO[i]))                       # (we're assuming 1st row isn't grab sensor data; it isn't now)
         z$DO[i] <- z$DO[i - 1]
      if(z$Source[i] == 2 & is.na(z$DO_Pct_Sat[i]))
         z$DO_Pct_Sat[i] <- z$DO_Pct_Sat[i - 1]
   }
   
   saveRDS(data.frame(z), 'inst/data.RDS')                        # un-tibble the results so we don't have problems later
}
