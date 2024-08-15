# buzzbay - Buzzards Bay data display tool
# B. Compton, 31 Jul 2024



library(shiny)
library(bslib)
library(dygraphs)
library(sinaplot)
library(DT)
library(lubridate)
library(shinybusy)
library(shinyWidgets)
library(shinyjs)


source('buzz.plots.R')
source('dygraphs_plugins.R')


unit.vars <<- c('DO', 'DO_Pct_Sat')          # these two are global, shared with subroutine and other sessions
unit.names <<- c('mg/L', '% saturation')

aggreg.choices = list('None' = 0, 'Hourly' = 1, '4 hours' = 4, '8 hours' = 8, '12 hours' = 12, 'Daily' = 24, 
                      'Weekly' = 7 * 24, 'Bi-weekly' = 14 * 24, '30 days' = 30 & 24, 'Entire period' = 1e6)
method.choices = c('Mean' = 'mean', 'Minimum' = 'min', '5th percentile' = 'p5', '10th percentile' = 'p10', 'Median' = 'median', 
                   'Maximum' = 'max', 'Standard deviation' = 'sd', 'Percent exceedence' = 'pe')


# Read site, sensor, and grab bag data
sites <- read.csv('inst/sitenames.csv')
sites <- sites[sites$include == TRUE,]

Site_Year <- readRDS('inst/Site_Year.RDS')
all.sensors <- readRDS('inst/sensors.RDS')



# User interface ---------------------
ui <- page_sidebar(
   theme = bs_theme(bootswatch = 'cosmo', version = 5),     # bslib version defense. Use version_default() to update
   useShinyjs(),
   
   title = 'buzzbay (dev version)',
   
   sidebar = 
      sidebar(
         add_busy_spinner(spin = 'fading-circle', position = 'top-left', onstart = FALSE, timeout = 500),
         
         card(
            selectInput('Site_Year', label = 'Site and year', choices = Site_Year$Site_Year),
            
            sliderInput('period', label = 'Time period', min = 0, max = 0, value = c(0, 0)),
            
            radioButtons('units', label = 'Units', choiceNames = as.list(unit.names), choiceValues = 1:2),
            
            numericInput('threshold', label = 'Comparison threshold', value = '',
                         min = 0, step = 1),   
            
            materialSwitch('plot.threshold', label = 'Plot comparison threshold', 
                           value = FALSE),
            
            materialSwitch('dist.plot', label = 'Show distribution plot', value = FALSE),
            
            numericInput('exceedance', label = 'Exceedance threshold (%)', value = '',
                         min = 0, max = 100, step = 1),
            
            materialSwitch('grab.bag', label = 'Display grab-bag samples', 
                           value = FALSE),
            
            selectInput('interval', label = 'Aggregation interval', choices = aggreg.choices),
            
            selectInput('method', label = 'Aggregation method', choices = method.choices, 
                        selected = 'median'),
            
            materialSwitch('moving.window', label = 'Smooth data'),
            
            br(),
            hr(),
            
            tags$img(height = 77, width = 133, src = 'umass_logo.png')
         ),
         width = 340
      ),
   
   card(
      navset_pill(
         nav_panel('Plot',
                   fluidRow(
                      column(width = 11,
                             dygraphOutput('plot')
                      ),
                      column(width = 1, 
                             plotOutput('sinaplot'))
                   )
         ),
         nav_panel('Table',
                   DTOutput('table'))
      )
   )
)



# Server -----------------------------
server <- function(input, output, session) {
   
   # bs_themer()                                                     # uncomment to select a new theme
   # print(getDefaultReactiveDomain())
   
   disable('period')                                                 # this is dim while it shows 0,0
   
   
   session$userData$y.range <- list(c(min(all.sensors$DO, na.rm = TRUE), max(all.sensors$DO, na.rm = TRUE)),         # full range of DO data
                                    c(min(all.sensors$DO_Pct_Sat, na.rm = TRUE), max(all.sensors$DO_Pct_Sat, na.rm = TRUE)))
   
   
   observeEvent(input$Site_Year, {                                    # --- New site/year selected. Select site and year (entire period) and update time period slider
      session$userData$sensor <- all.sensors[all.sensors$Site_Year == input$Site_Year, ] 
      
      minmax <- c(min(session$userData$sensor$Date_Time), max(session$userData$sensor$Date_Time))
      freezeReactiveValue(input, 'period')
      updateSliderInput('period', min = minmax[1], max = minmax[2], value = minmax,
                        timeFormat = '%b %e', session = getDefaultReactiveDomain())
      enable('period')
      session$userData$keep.date.window <- FALSE
      buzz.plots(input, output, session = getDefaultReactiveDomain())
   })
   
   
   observeEvent(input$period, {                                      # --- Period selected. Select site, year, and period
      period <- as.POSIXct(floor_date(as.POSIXct(input$period) - 4 * 60 * 60, 'days')) + 4 * 60 * 60  # round to midnight (adjusted for EDT)
      session$userData$sensor <- all.sensors[all.sensors$Site_Year == input$Site_Year &
                                                all.sensors$Date_Time >= period[1] & all.sensors$Date_Time <= period[2], ]
      session$userData$keep.date.window <- FALSE
      buzz.plots(input, output, session = getDefaultReactiveDomain())
   })
   
   
   observeEvent(input$units, {                                       # --- Units changed, so keep date window
      freezeReactiveValue(input,'threshold')
      updateNumericInput('threshold', label = paste0('Comparison threshold ',  ifelse(input$units == 1, '(mg/L)', '(% saturation)')), 
                         value = ifelse(input$units == 1, 5, 75), session = getDefaultReactiveDomain())
      session$userData$keep.date.window <- TRUE
      buzz.plots(input, output, session = getDefaultReactiveDomain())
   })
   
   
   observeEvent(input$interval, {                                    # --- Aggregation interval
      if(input$interval == 0) {
         disable('method')
         disable('moving.window')
      }
      else                                                           # if interval is not none, enable method and moving window
      {
         enable('method')
         enable('moving.window')
      }
      session$userData$keep.date.window <- TRUE
      buzz.plots(input, output, session = getDefaultReactiveDomain())
   })
   
   
   observeEvent(input$method, {                                      # --- Aggregation method
      session$userData$keep.date.window <- TRUE
      buzz.plots(input, output, session = getDefaultReactiveDomain())
   })
   
   
   observeEvent(list(input$dist.plot, input$grab.bag, input$threshold, input$plot.threshold, input$exceedance), {      # --- Remaining controls that don't need to do anything but keep date window        
      session$userData$keep.date.window <- TRUE
      buzz.plots(input, output, session = getDefaultReactiveDomain())
   })
}   


shinyApp(ui, server)
