# buzzbay - Buzzards Bay data display tool
# B. Compton, 31 Jul 2024



library(shiny)
library(bslib)
library(shinybusy)
library(shinyWidgets)
library(dygraphs)
library(lubridate)
library(shinyjs)


source('dygraphs_plugins.R')


unit.vars <- c('DO', 'DO_Pct_Sat')
unit.names <- c('mg/L', '% saturation')
sites <- read.csv('inst/sitenames.csv')
sites <- sites[sites$include == TRUE,]

siteYear <- readRDS('inst/siteYear.RDS')
all.sensors <- readRDS('inst/sensors.RDS')

y.range <- list(c(min(all.sensors$DO, na.rm = TRUE), max(all.sensors$DO, na.rm = TRUE)), 
                c(min(all.sensors$DO_Pct_Sat, na.rm = TRUE), max(all.sensors$DO_Pct_Sat, na.rm = TRUE)))

print(y.range)

aggreg.choices = list('None' = 0, 'Hourly' = 1, '4 hours' = 4, '8 hours' = 8, '12 hours' = 12, 'Daily' = 24, 
                      'Weekly' = 7 * 24, 'Bi-weekly' = 14 * 24, '30 days' = 30 & 24, 'Entire period' = 1e6)
method.choices = c('Mean' = 'mean', 'Minimum' = 'min', '5th percentile' = 'p5', '10th percentile' = 'p10', 'Median' = 'median', 
                   'Maximum' = 'max', 'Standard deviation' = 'sd', 'Percent exceedence' = 'pe')



# User interface ---------------------
ui <- page_sidebar(
   theme = bs_theme(bootswatch = 'cosmo', version = 5),     # bslib version defense. Use version_default() to update
   useShinyjs(),
   
   title = 'buzzbay (dev version)',
   
   sidebar = 
      sidebar(
         add_busy_spinner(spin = 'fading-circle', position = 'top-left', onstart = FALSE, timeout = 500),
         
         card(
            selectInput('siteYear', label = 'Site and year', choices = siteYear$siteYear),
            
            sliderInput('period', label = 'Time period', min = 0, max = 0, value = c(0, 0)),
            
            radioButtons('units', label = 'Units', choiceNames = as.list(unit.names), choiceValues = 1:2),
            
            numericInput('threshold', label = 'Comparison threshold', value = '',
                         min = 0, step = 1),   
            materialSwitch(inputId = 'plot.threshold', label = 'Plot comparison threshold', 
                           value = FALSE),
            numericInput('exceedance', label = 'Exceedance threshold (%)', value = '',
                         min = 0, max = 100, step = 1),
            materialSwitch(inputId = 'grab.bag', label = 'Display grab-bag samples', 
                           value = FALSE),
            
            selectInput('interval', label = 'Aggregation interval', choices = aggreg.choices),
            
            selectInput('method', label = 'Aggregation method', choices = method.choices, 
                        selected = 'median'),
            
            materialSwitch(inputId = 'moving.window', label = 'Moving window'),
            
            hr(),
            
            actionLink('aboutSite', label = 'About this site'),
            br(),
            tags$img(height = 77, width = 133, src = 'umass_logo.png')
         ),
         width = 340
      ),
   
   card(
      dygraphOutput('plot'),
      max_height = 500
   )
)



# Server -----------------------------
server <- function(input, output, session) {
   
   # bs_themer()                                                # uncomment to select a new theme
   # print(getDefaultReactiveDomain())
   
   disable('period')                                           # this is dim while it shows 0,0
   
   
   observe({                                                   # --- New site/year selected. Update time period slider
      session$userData$sensor <- all.sensors[all.sensors$siteYear == input$siteYear, ]
      minmax <- c(min(session$userData$sensor$Date_Time), max(session$userData$sensor$Date_Time))
      freezeReactiveValue(input, 'period')
      updateSliderInput('period', min = minmax[1], max = minmax[2], value = minmax,
                        timeFormat = '%b %e', session = getDefaultReactiveDomain())
      enable('period')
      session$userData$keep.date.window <- FALSE
   })
   
   
   # observeEvent(input$units, {                                 # --- Units changed, so keep date window
   #    updateNumericInput('threshold', label = paste0('Comparison threshold ',  ifelse(input$units == 1, '(mg/L)', '(% saturation)')), 
   #                       value = ifelse(input$units == 1, 5, 75), session = getDefaultReactiveDomain())
   #    session$userData$keep.date.window <- TRUE
   # })
   
   
   observeEvent(input$grab.bag, {                              # --- Grab bag. Keep date window        
      # nothing here yet
      session$userData$keep.date.window <- TRUE
   })
   
   
   observeEvent(input$interval, {                              # --- Aggregation interval
      if(input$interval == 0) {
         disable('method')
         disable('moving.window')
      }
      else 
      {
         enable('method')
         enable('moving.window')
      }
      session$userData$keep.date.window <- TRUE
   })
   
   
   
   observeEvent(input$method, {                                # --- Aggregation method
      
      session$userData$keep.date.window <- TRUE
   })
   
   
   observeEvent(list(input$siteYear, input$period, input$units, input$grab.bag,                   # --- Draw the plot
                     input$threshold, input$plot.threshold, input$grab.bag, input$interval,
                     input$method, input$moving.window), {
                        
                        period <- as.POSIXct(floor_date(as.POSIXct(input$period) - 4 * 60 * 60, 'days')) + 4 * 60 * 60  # round to midnight (adjusted for EDT)
                        vars <- session$userData$sensor[session$userData$sensor$Date_Time >= period[1] &
                                                           session$userData$sensor$Date_Time <= period[2],
                                                        c('Date_Time', unit.vars[as.integer(input$units)])]
                        
                        
                        #   print(Sys.time())
                        
                        if(!identical(input$units, session$userData$units)) {    # if units change,
                           freezeReactiveValue(input,'threshold')
                           updateNumericInput('threshold', label = paste0('Comparison threshold ',  ifelse(input$units == 1, '(mg/L)', '(% saturation)')), 
                                              value = ifelse(input$units == 1, 5, 75), session = getDefaultReactiveDomain())
                           session$userData$keep.date.window <- TRUE
                        }
                        session$userData$units <- input$units
                        
                        
                        if(!identical(input$period, session$userData$period))    # if time period changed, reset date window
                           session$userData$keep.date.window <- FALSE
                        session$userData$period <- input$period
                        
                        
                        if(!identical(input$siteYear, session$userData$siteYear))    # if site or year changed, reset date window
                           session$userData$keep.date.window <- FALSE
                        session$userData$siteYear <- input$siteYear
                        
                        
                        if(dim(vars)[1] > 0) {
                           
                           show.threshold <- input$plot.threshold & (input$interval == 0 | (!input$method %in% c('sd', 'pe')))   # plot threshold if switch is on and we're not
                           # if(show.threshold) {                                                                                     #    aggregating by SD or % exceedance
                           #     #  vars <- cbind(vars, threshold = input$threshold)
                           #       session$userData$keep.date.window <- TRUE
                           #    }
                           
                           output$plot <- renderDygraph({
                              graph <- dygraph(vars, main = 'Dissolved oxygen', ylab = unit.names[as.integer(input$units)]) |>
                                 dyOptions(useDataTimezone = TRUE) |>
                                 dyAxis('x', gridLineColor = '#D0D0D0') |>
                                 dyAxis('y', gridLineColor = '#D0D0D0',  valueRange = y.range[[as.numeric(input$units)]]) |>
                                 
                                 dySeries(ifelse(input$units == 1, 'DO', 'DO_Pct_Sat'), color = '#3C2692') |>
                                 dyRangeSelector(retainDateWindow = session$userData$keep.date.window) |>
                                 dyUnzoom() |>
                                 dyCrosshair(direction = "vertical")
                              
                              
                              #  print(show.threshold)
                              if(show.threshold)
                                 graph <- dyLimit(graph, input$threshold, color = 'gray')
                              # graph <- dySeries(graph, 'threshold', color = 'gray')
                              
                              #    dySeries('grab.bag', drawPoints = input$grab.bag, strokeWidth = 0)     # this is how we'll do grab-bag points
                              
                              graph
                           })
                        }
                     })
}   


shinyApp(ui, server)
