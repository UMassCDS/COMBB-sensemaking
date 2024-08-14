'buzz.plots' <- function(input, output, session) {
   
   # update time series and distribution plots, as well as tables for Buzzard's Bay app
   # Arguments: 
   # 
   # Result:
   # 
   # B. Compton, 14 Aug 2024 (pulled from buzzbay.app)
   
   
   
 #  cat(paste('--- buzz.plots, ', Sys.time(), '\n', sep = ''))
   
  
   vars <- session$userData$sensor[, c('Date_Time', unit.vars[as.integer(input$units)])]
   
   if(dim(vars)[1] > 0) {
      
      show.threshold <- input$plot.threshold & (input$interval == 0 | (!input$method %in% c('sd', 'pe')))   # plot threshold if on and not aggregating by SD or % exceedance
      
      output$plot <- renderDygraph({
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
   }
}