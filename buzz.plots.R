'buzz.plots' <- function(input, output, session) {
   
   # update time series and distribution plots, as well as tables for Buzzard's Bay app
   # Arguments: 
   #  input, output, session - Shiny variables
   # B. Compton, 14 Aug 2024 (pulled from buzzbay.app)
   
   
   
   vars <- session$userData$dataset[, c('Date_Time', paste0(c('', 'Grab_')[1:(input$grab.bag + 1)], unit.vars[as.integer(input$units)]))]
   
   if(dim(vars)[1] > 0) {
      
      show.threshold <- input$plot.threshold & (input$interval == 0 | (!input$method %in% c('sd', 'pe')))   # plot threshold if on and not aggregating by SD or % exceedance
      
      output$plot <- renderDygraph({                                                                        # --- time series plot
         graph <- dygraph(vars, main = 'Dissolved oxygen', ylab = unit.names[as.integer(input$units)]) |>
            dyOptions(useDataTimezone = TRUE) |>
            dyAxis('x', gridLineColor = '#D0D0D0') |>
            dyAxis('y', gridLineColor = '#D0D0D0',  
                   #               valueRange = ifelse(\input$interval != 0 & input$method == 'sd', c(NA, NA), session$userData$y.range[[as.numeric(input$units)]])) |>  # free y-axis for sd
                   valueRange = session$userData$y.range[[as.numeric(input$units)]]) |>
            dySeries(names(vars)[2], color = '#3C2692') |>
            dyRangeSelector(retainDateWindow = session$userData$keep.date.window) |>
        #    dyUnzoom() |>
            dyCrosshair(direction = "vertical")
         
         
         if(show.threshold)
            graph <- dyLimit(graph, input$threshold, color = 'gray')
         
         
         if(input$grab.bag) {
            if(input$interval != 0 & input$moving.window)                                                   # if aggregation is on and smoothing,
               graph <- dySeries(graph, names(vars)[3], color = '#DB5920', strokeWidth = 2)                 #    grab-bag as lines
            else                                                                                            #    else, grab-bag as points
               graph <- dySeries(graph, names(vars)[3], drawPoints = TRUE, pointShape = 'circle', pointSize = 5, color = '#DB5920', strokeWidth = 0)     
         }
         
         graph
      })
      
      
      if(input$dist.plot) {                                                                                 # --- distribution plot, if selected and > 2 points
         x <- list(vars[session$userData$dataset$Source == 1, 2], vars[, c(-1, -2)])      # response variables (sensors and maybe grab-bag) as list; drop imputed sensor data
         x <- lapply(x, function(v) v[!is.na(v)])                                         # remove missing
         if(length(x[[2]]) < 2)                                                           # if only 1 grab-bag point, drop it as we can't plot
            x <- x[1]
         
         output$sinaplot <- renderPlot(
            if(input$dist.plot & length(x[[1]]) >= 2) {
               par(mai = c(0.75, 0, 0.25, 0))                        # margins: bottom, left, top, right (inches). Calibrated to dygraph.
               sinaplot(x, xlab = '', pch = c('.', 'o'), cex = c(1, 1), seed = 1, ylim = session$userData$y.range[[as.numeric(input$units)]],
                        xaxt = 'n', yaxt = 'n', lty = 0, col = c('#3C2692', '#DB5920'), main = 'Distribution plot', cex.main = 1)
            }
            else
               NULL
         )
      }
      
      
      if(!session$userData$keep.date.window)
         output$table <- renderDT({                                                                         # --- data table (in 2nd tab)           
            table <- datatable(session$userData$dataset[session$userData$dataset$Source == 1, c('Date_Time', 'DO', 'DO_Pct_Sat', 'Temp_CondLog')], 
                               colnames = c('Date and time', 'DO (mg/L)', 'DO (% sat)', 'Temperature (C)'), 
                               caption = htmltools::tags$caption(
                                  style = 'caption-side: top', HTML('<h5><b>Table of Continuous Monitoring Data</b></h5>')),
                               options = list(dom = 'ltipr')) |>
               formatDate('Date_Time', 'toLocaleString') |>
               formatRound('DO', 2) |>
               formatRound('DO_Pct_Sat', 2)
         })
   }
}
