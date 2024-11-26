'buzz.plots' <- function(input, output, session) {
   
   # update time series and distribution plots, as well as data tables and summary stats for Buzzard's Bay app
   # Arguments: 
   #  input, output, session - Shiny variables
   # B. Compton, 14 Aug 2024 (pulled from buzzbay.app)
   
   
   
   ### dataset <<- session$userData$dataset; sensor <<- input$sensor; grab <<- input$grab
   
   plot.data <- session$userData$dataset[, c('Date_Time', c('DO', 'Grab_DO')[c(input$sensor, input$grab)]), drop = FALSE]    # plot.data is used throughout
   
   
   if(dim(plot.data)[1] > 0)                                                              # don't do anything if no data
   {
      plot.data <- plot.data                                                              # plot.data is just for time series
      
      if(input$grab & !input$sensor)                                                      # if only grab samples, drop all missing sensor rows to make identify easier
         plot.data <- plot.data[!is.na(plot.data$Grab_DO), ]
      
      
      if(input$interval == 'Entire period' & (input$sensor | input$grab)) {               # if interval is entire period, go to a lot of trouble 
         plot.data <- rbind(plot.data[c(1, 1), ], plot.data)                              #    add 2 new leading rows for ends of sensor data
         plot.data$Date_Time[c(1, 2)] <- session$userData$x.range
         if(input$grab)
            plot.data$Grab_DO[c(1, 2)] <- NA
      }
      
      
      if(input$sensor & input$grab & !input$moving.window) {                              # Ugh! If grab sample is taken at the same time as sensor data, I get two rows
         b <- rep(TRUE, dim(plot.data)[1])                                                # with diagonal NAs, and identify doesn't work. Fix that here.
         for(i in 1:(dim(plot.data)[1] - 1))
            if(plot.data$Date_Time[i] == plot.data$Date_Time[i + 1]) {
               plot.data$Grab_DO[i] <- max(plot.data$Grab_DO[c(i, i + 1)], na.rm = TRUE)
               b[i + 1] <- FALSE
            }
         plot.data <- plot.data[b, ]
      }
      
      
      if(!session$userData$x.range[1] %in% plot.data$Date_Time)
         plot.data <- rbind(c(session$userData$x.range[1], rep(NA, dim(plot.data)[2] - 1)), plot.data)     # force x-axis to conform to x.range when showing only grab samples (boo dygraphs!)
      
      if(!session$userData$x.range[2] %in% plot.data$Date_Time)
         plot.data <- rbind(plot.data, c(session$userData$x.range[2], rep(NA, dim(plot.data)[2] - 1)))
      
      
      show.threshold <- input$plot.threshold & (input$interval == 'None' | (!input$method %in% c('sd', 'pe')))   # plot threshold if on and not aggregating by SD or % exceedance
      
      if(!input$sensor & !input$grab)                                                     # if no data selected
         plot.data$null <- NA                                                             #    this blank column forces x-axis to display properly 
      
      output$plot <- renderDygraph({                                                      # --- time series plot
         graph <- dygraph(plot.data, ylab = 'mg/L') |>
            dyOptions(useDataTimezone = TRUE, connectSeparatedPoints = input$interval != 'None') |>
            dyAxis('x', gridLineColor = '#D0D0D0', rangePad = 5) |>
            dyAxis('y', gridLineColor = '#D0D0D0', valueRange = session$userData$y.range) |>
            dyRangeSelector(retainDateWindow = session$userData$keep.date.window) |>
            dyCrosshair(direction = "vertical") 
         
         if(input$sensor)
            graph <- dySeries(graph, 'DO', color = '#3C2692', label = 'Sensor DO')
         
         if(input$grab)
            graph <- dySeries(graph, 'Grab_DO', drawPoints = TRUE, pointSize = 3, color = '#DB5920', strokeWidth = 0, label = 'Grab Sample DO')
         
         if(input$sensor | input$grab)
            graph <- dyLegend(graph, show = 'always')
         
         if(show.threshold)
            graph <- dyLimit(graph, input$threshold, color = 'green')
         
         graph
      })
      
      
      
      if(input$dist.plot) {                                                               # --- distribution plot
         x <- list(plot.data[session$userData$dataset$Source == 1, 2], plot.data[, c(-1, -2)])      # response variables (sensors and maybe grab samples) as list; drop imputed sensor data
         x <- lapply(x, function(v) v[!is.na(v)])                                         # remove missing
         if(length(x[[2]]) < 2)                                                           # if only 1 grab samples point, drop it as we can't plot
            x <- x[1]
         
         
         output$sinaplot <- renderPlot(
            if(input$dist.plot & length(x[[1]]) >= 2 & !input$interval == 'Entire period') {
               par(mai = c(0.75, 0, 0.25, 0))                                             # margins: bottom, left, top, right (inches). Calibrated to dygraph.
               sinaplot(x, xlab = '', pch = 20, cex = 1, seed = 1, ylim = session$userData$y.range,
                        xaxt = 'n', yaxt = 'n', lty = 0, col = c('#3C2692', '#DB5920')[c(input$sensor, input$grab)],
                        main = 'Distribution plot', cex.main = 1)
            }
            else
               NULL
         )
      }
      
      
      
      output$sensor.table <- renderDT({                                                   # --- sensor data table (in 2nd tab)           
         buzz.table(session$userData$dataset[session$userData$dataset$Source == 1, ], '', 'Continuous Monitoring Data')
      })
      
      output$grab.table <- renderDT({                                                     # --- grab sample data table (in 3nd tab)           
         buzz.table(session$userData$dataset[!is.na(session$userData$dataset$Grab_DO), ],
                    'Grab_', 'Grab Sample Monitoring Data')
      })
      
      
      
      if(session$userData$redraw.stats)                                                   # --- summary stats table
         output$stats <- render_gt({
            buzz.stats(session$userData$dataset, input$threshold, input$grab, input$sensor)
         })
   }
   
   
   # --- record actions to log
   if(is.null(session$userData$period))
      session$userData$period <- input$period
   
   session$userData$log <- rbind(session$userData$log, data.frame(time = now(), site_year = input$Site_Year, 
                                                                  period = paste0('(', session$userData$period[1], ' - ', session$userData$period[2], ')'), 
                                                                  threshold = input$threshold, plot_threshold = input$plot.threshold, 
                                                                  plot_dist = input$dist.plot, plot_grab = input$grab, 
                                                                  interval = input$interval, statistic = input$method, moving_window = input$moving.window))
}
