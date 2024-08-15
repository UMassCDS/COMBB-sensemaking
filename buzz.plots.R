'buzz.plots' <- function(input, output, session) {
   
   # update time series and distribution plots, as well as tables for Buzzard's Bay app
   # Arguments: 
   #  input, output, session - Shiny variables
   # B. Compton, 14 Aug 2024 (pulled from buzzbay.app)
   
   
   
   #  cat(paste('--- buzz.plots, ', Sys.time(), '\n', sep = ''))
   
   
   vars <- session$userData$sensor[, c('Date_Time', unit.vars[as.integer(input$units)])]
   
   if(dim(vars)[1] > 0) {
      
      show.threshold <- input$plot.threshold & (input$interval == 0 | (!input$method %in% c('sd', 'pe')))               # plot threshold if on and not aggregating by SD or % exceedance
      
      output$plot <- renderDygraph({                                                                                    # --- time series plot
         graph <- dygraph(vars, main = 'Dissolved oxygen', ylab = unit.names[as.integer(input$units)]) |>
            dyOptions(useDataTimezone = TRUE) |>
            dyAxis('x', gridLineColor = '#D0D0D0') |>
            dyAxis('y', gridLineColor = '#D0D0D0',  valueRange = session$userData$y.range[[as.numeric(input$units)]]) |>
            
            dySeries(ifelse(input$units == 1, 'DO', 'DO_Pct_Sat'), color = '#3C2692') |>
            dyRangeSelector(retainDateWindow = session$userData$keep.date.window) |>
            dyUnzoom() |>
            dyCrosshair(direction = "vertical")
         
         if(show.threshold)
            graph <- dyLimit(graph, input$threshold, color = 'gray')
         
         #    dySeries('grab.bag', drawPoints = input$grab.bag, strokeWidth = 0)     # this is how we'll do grab-bag points
         
         graph
      })
      
      
      if(input$dist.plot) {                                                                                             # --- distribution plot, if selected and > 2 points
         x <- vars[!is.na(vars[, 2]), 2][[1]]
         output$sinaplot <- renderPlot(
            if(input$dist.plot & length(x) > 2) {
               par(mai = c(0.75, 0, 0.25, 0))                        # margins: bottom, left, top, right (inches). Calibrated to dygraph.
               par(col = '#3C2692')
               sinaplot(x, xlab = '', pch = '.', cex = 1, seed = 1, ylim = session$userData$y.range[[as.numeric(input$units)]],
                        xaxt = 'n', yaxt = 'n', lty = 0)
            }
            else
               NULL
         )
      }
      
      
      if(!session$userData$keep.date.window)
         output$table <- renderDT({                                                                                     # --- data table (in 2nd tab)           
            table <- datatable(session$userData$sensor[, c('Date_Time', 'DO', 'DO_Pct_Sat')], 
                               colnames = c('Date and time', 'DO (mg/L)', 'DO (% sat)'),
                               options = list(dom = 'ltipr')) |>
               formatDate('Date_Time', 'toLocaleString') |>
               formatRound('DO', 2) |>
               formatRound('DO_Pct_Sat', 2)
            
         })
   }
}
