'buzz.plots' <- function(input, output, session) {
   
   # update time series and distribution plots, as well as data tables and summary stats for Buzzard's Bay app
   # Arguments: 
   #  input, output, session - Shiny variables
   # B. Compton, 14 Aug 2024 (pulled from buzzbay.app)
   
   
   
   vars <- session$userData$dataset[, c('Date_Time', paste0(c('', 'Grab_')[1:(input$grab + 1)], unit.vars[as.integer(input$units)]))]
   
   if(dim(vars)[1] > 0) {
      
      show.threshold <- input$plot.threshold & (input$interval == 'None' | (!input$method %in% c('sd', 'pe')))   # plot threshold if on and not aggregating by SD or % exceedance
      plot.data <- vars
      names(plot.data)[2] <- 'Sensor DO'
      if(input$grab)
         names(plot.data)[3] <- 'Grab sample DO'
      
      output$plot <- renderDygraph({                                                      # --- time series plot
         graph <- dygraph(plot.data, main = 'Dissolved oxygen', ylab = unit.names[as.integer(input$units)]) |>
            dyOptions(useDataTimezone = TRUE, connectSeparatedPoints = input$interval != 'None') |>
            dyAxis('x', gridLineColor = '#D0D0D0') |>
            dyAxis('y', gridLineColor = '#D0D0D0', valueRange = session$userData$y.range[[as.numeric(input$units)]]) |>
            dySeries(names(plot.data)[2], color = '#3C2692') |>
            dyRangeSelector(retainDateWindow = session$userData$keep.date.window) |>
            dyCrosshair(direction = "vertical")
         
         if(show.threshold)
            graph <- dyLimit(graph, input$threshold, color = 'green')
         
         if(input$grab)
            graph <- dySeries(graph, names(plot.data)[3], drawPoints = TRUE, pointSize = 3, color = '#DB5920', strokeWidth = 0)  
         
         graph
      })
      
      
      
      if(input$dist.plot) {                                                               # --- distribution plot, if selected and > 2 points
         x <- list(vars[session$userData$dataset$Source == 1, 2], vars[, c(-1, -2)])      # response variables (sensors and maybe grab samples) as list; drop imputed sensor data
         x <- lapply(x, function(v) v[!is.na(v)])                                         # remove missing
         if(length(x[[2]]) < 2)                                                           # if only 1 grab samples point, drop it as we can't plot
            x <- x[1]
         
         output$sinaplot <- renderPlot(
            if(input$dist.plot & length(x[[1]]) >= 2) {
               par(mai = c(0.75, 0, 0.25, 0))                                             # margins: bottom, left, top, right (inches). Calibrated to dygraph.
               sinaplot(x, xlab = '', pch = 20, cex = 1, seed = 1, ylim = session$userData$y.range[[as.numeric(input$units)]],
                        xaxt = 'n', yaxt = 'n', lty = 0, col = c('#3C2692', '#DB5920'), main = 'Distribution plot', cex.main = 1)
            }
            else
               NULL
         )
      }
      
      
      
      output$sensor.table <- renderDT({                                                   # --- sensor data table (in 2nd tab)           
         buzz.table(session$userData$dataset[session$userData$dataset$Source == 1, ], '', 'Continuous Monitoring Data')
      })
      
      output$grab.table <- renderDT({                                                     # --- grab sample data table (in 3nd tab)           
         buzz.table(session$userData$dataset[!is.na(session$userData$dataset$Grab_DO) | !is.na(session$userData$dataset$Grab_DO_Pct_Sat), ],
                  'Grab_', 'Grab Sample Monitoring Data')
      })
      
      
      
      if(session$userData$redraw.stats)                                                   # --- summary stats table
         output$stats <- render_gt({
            buzz.stats(session$userData$dataset, input$threshold, input$units, input$grab)
         })
   }
}
