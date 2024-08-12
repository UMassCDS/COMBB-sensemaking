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

aggreg.choices = list('None' = 0, 'Hourly' = 1, '4 hours' = 4, '8 hours' = 8, '12 hours' = 12, 'Daily' = 24, 
                      'Weekly' = 7 * 24, 'Bi-weekly' = 14 * 24, '30 days' = 30 & 24, 'Entire period' = 1e6)
method.choices = c('Mean', 'Minimum', '5th percentile', '10th percentile', 'Median', 
                  'Maximum', 'Standard deviation', 'Percent exceedence')



# User interface ---------------------
ui <- page_sidebar(
   theme = bs_theme(bootswatch = 'cerulean', version = 5),     # bslib version defense. Use version_default() to update
   useShinyjs(),
   
   title = 'buzzbay (dev version)',
   
   sidebar = 
      sidebar(
         add_busy_spinner(spin = 'fading-circle', position = 'top-left', onstart = FALSE, timeout = 500),
         
         card(
            selectInput('siteYear', label = 'Site and year', choices = siteYear$siteYear),
            
            sliderInput('period', label = 'Time period', min = 0, max = 0, value = c(0, 0)),
            
            radioButtons('units', label = 'Units', choiceNames = as.list(unit.names), choiceValues = 1:2),
            
            hr(),
            
            numericInput('threshold', label = 'Comparison threshold', value = '',
                         step = 0.1, width = '40%'),   
            materialSwitch(inputId = 'plot.threshold', label = 'Plot comparison threshold', 
                           value = FALSE),
            numericInput('exceedance', label = 'Exceedance threshold (%)', value = '',
                         step = 1, width = '40%'),
            materialSwitch(inputId = 'grab.bag', label = 'Display grab-bag samples', 
                           value = FALSE),
            
            selectInput('interval', label = 'Aggregation interval', choices = aggreg.choices),
            
            selectInput('method', label = 'Aggregation method', choices = method.choices, 
                        selected = 'median'),
            
            materialSwitch(inputId = 'moving.window', label = 'Moving window'),
            
            hr(),
            
            actionLink('aboutSite', label = 'About this site'),
            br(),
            tags$img(height = 103, width = 178, src = 'umass_logo.png')
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
   
   #bs_themer()                                                # uncomment to select a new theme
   #  print(getDefaultReactiveDomain())
   
   disable('period')                                           # this is dim while it shows 0,0
   
   
   observe({                                                   # --- New site/year selected. Update time period slider
      session$userData$sensor <- all.sensors[all.sensors$siteYear == input$siteYear, ]
      minmax <- c(min(session$userData$sensor$Date_Time), max(session$userData$sensor$Date_Time))
      updateSliderInput('period', min = minmax[1], max = minmax[2], value = minmax,
                        timeFormat = '%b %e', session = getDefaultReactiveDomain())
      enable('period')
      session$userData$keep.date.window <- FALSE
   })
   
   
   observeEvent(input$units, {                                 # --- Units changed, so keep date window
      session$userData$keep.date.window <- TRUE
   })
   
   
   observeEvent(input$grab.bag, {                              # --- Grab bag. Keep date window        
      # nothing here yet
      session$userData$keep.date.window <- TRUE
   })
   
   
   observeEvent(list(input$siteYear, input$period, input$units, input$grab.bag), {     # --- Draw the plot
      period <- as.POSIXct(floor_date(as.POSIXct(input$period) - 4 * 60 * 60, 'days')) + 4 * 60 * 60  # round to midnight (adjusted for EDT)
      vars <- session$userData$sensor[session$userData$sensor$Date_Time >= period[1] &
                                         session$userData$sensor$Date_Time <= period[2],
                                      c('Date_Time', unit.vars[as.integer(input$units)])]
      
      if(!identical(input$period, session$userData$period))    # if time period changed, reset date window
         session$userData$keep.date.window <- FALSE
      session$userData$period <- input$period
      
      if(dim(vars)[1] > 0) {
         output$plot <- renderDygraph({
            dygraph(vars, main = 'Dissolved oxygen', ylab = unit.names[as.integer(input$units)]) |>
               dyOptions(useDataTimezone = TRUE) |>
               dyAxis('x', gridLineColor = '#D0D0D0') |>
               dyAxis('y', gridLineColor = '#D0D0D0') |>
               dySeries(color = '#3C2692') |>
               #    dySeries('grab.bag', drawPoints = input$grab.bag, strokeWidth = 0) |>     # this is how we'll do grab-bag points
               dyRangeSelector(retainDateWindow = session$userData$keep.date.window) |>
               dyUnzoom() |>
               dyCrosshair(direction = "vertical")
         })
      }
   })
}   


shinyApp(ui, server)
