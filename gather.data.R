'gather.data' <- function(source = 'c:/Work/DataCore/BuzzBay/SeemasData/') {
   
   # collect data for buzzbay and build a single file
   # Arguments:
   #     source               source directory
   #     'inst/sitenames.csv' Seema's sitenames, with additional include (TRUE or FALSE) and
   #                          years (comma-separated vector of available years). Excel file
   #                          is constructed from site and year.
   # Source data:
   #     '*.xlsx'             sensor or grab bag data files. We assume that data are in the
   #                          first sheet of each file. Column names are a mess. We rename 
   #                          them to result names.
   # Result:
   #     'inst/sensors.RDS'   file containing a data frame with target variables for each 
   #                          site included in sitenames.csv and each year listed
   #        Result columns:   siteYear       site & year (site description - year)
   #                          Date_Time      date & time of each obs (year-month-day h:m:s)
   #                          DO             DO (in mg/L)
   #                          DO_Pct_Sat     DO (in % saturation)
   #                          Temp_CondLog   temperature (C)
   # Note:  if a sensor/grab bag file matches more than one column name variant, the first one
   #        listed in col.names will be chosen. (This is what happens when your data are a mess. Grr.)
   # B. Compton, 31 Jul 2024
   
   
   
   library(readxl)
   
   col.names <- list(list('siteYear', 'siteYear'),                            # standard col name, variants
                     list('Date_Time', c('SAMP_DATE_TIME', 'Date Time')), 
                     list('DO', c('DO_MGL2', 'DO_mgl', 'DO_MGL')),
                     list('DO_Pct_Sat', c('DP_SAT', 'DO_SAT')),
                     list('Temp_CondLog', c('Temp_C_condlogger', 'TEMP_C')))
   
   
   sites <- read.csv('inst/sitenames.csv')
   sites <- sites[sites$include == TRUE,]
   sy <- data.frame(matrix(NA, 0, 4))
   z <- matrix(NA, 0, 5)
   q <- rep(NA, dim(z)[2])
   for(i in 1:length(col.names)) q[i] <- col.names[[i]][[1]]
   colnames(z) <- q
   
   
   for(i in 1:dim(sites)[1]) {
      y <- as.integer(unlist(strsplit(sites$years[i], ',')))      # years
      
      for(j in y) {
         sy <- rbind(sy, c(sites$site[i], sites$description[i], j, paste0(sites$description[i], ' - ', j)))
         
         d <- paste0(source, 'location ', sites$site[i], '/')
         f <- paste0(sites$site[i], '_sensor_', j, '.xlsx')
         cat('\nData file: ', f, '\n\n', sep = '')
         x <- suppressWarnings(read_excel(paste0(d, f), sheet = 1))
         x$siteYear <- paste0(sites$description[i], ' - ', j)
         
         n <- names(x)                                            # find columns
         c <- rep(NA, length(col.names))
         
         for(k in 1:length(col.names)) {
            d <- match(col.names[[k]][[2]], n)                     # match possible name variants
            d <- d[!is.na(d)]
            if(length(d) == 0) stop('Column ', col.names[[k]][[1]], ' not found\n')
            c[k] <- d[1]                                          # first matching column name
         }
         
         t <- data.frame(standard = q, found.column = names(x)[c])
         print.data.frame(t[-1,], row.names = FALSE)
         
         x <- x[, c]                   # don't let rbind's miserable name matching mess us up
         names(x) <- q
         
         b <- sum(x$DO < 0, na.rm = TRUE) + sum(x$DO_Pct_Sat < 0, na.rm = TRUE)  # set negative values to NA
         x$DO[x$DO < 0] <- NA
         x$DO_Pct_Sat[x$DO_Pct_Sat < 0] <- NA
         
         cat('\n', format(dim(x)[1], big.mark = ','), ' cases.', sep = '')
         if(b > 0)
            cat(' ', b, ' negative DO value', 's'[b != 1], ' set to missing.', sep = '')
         cat('\n\n')
         
         z <- rbind(z, x)
      }
   }
   colnames(sy) <- c('site', 'description', 'year', 'siteYear')
   saveRDS(sy, 'inst/siteYear.RDS')
   
   z$siteYear <- as.factor(z$siteYear)                          # siteYear as factor, of course
   z$Date_Time <- as.POSIXct(z$Date_Time, tz = 'America/New_York') + 4 * 60 * 60    # kludge up the time so dygraphs gives us times in EDI
   
   saveRDS(z, 'inst/sensors.RDS')
}
