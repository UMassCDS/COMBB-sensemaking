# buzzbay - Buzzards Bay data display tool
# B. Compton, 31 Jul 2024



library(shiny)
library(bslib)
library(dygraphs)
library(sinaplot)
library(DT)
library(gt)
library(slider)
library(dplyr)
library(lubridate)
library(shinybusy)
library(shinyWidgets)
library(shinyjs)
library(bslib)
library(bsicons)


source('buzz.aggregate.R')
source('buzz.plots.R')
source('buzz.table.R')
source('buzz.stats.R')
source('dygraphs_plugins.R')


unit.vars <<- c('DO', 'DO_Pct_Sat')                         # these two are global, shared with subroutines and other sessions
unit.names <<- c('mg/L')


aggreg.choices = list('None', 'Hourly', '4 hours', '8 hours', '12 hours', 'Daily', 'Weekly', 'Bi-weekly', 'Monthly', 
                      'Entire period')

method.choices = c('Mean' = 'mean', 'Median' = 'median', 'Minimum' = 'min', 'Maximum' = 'max', 'Standard deviation' = 'sd')       


# Read site, dataset, and grab sample data
sites <- read.csv('inst/sitenames.csv')
sites <- sites[sites$include == TRUE,]

Site_Year <- readRDS('inst/Site_Year.RDS')
Site_Year <- rbind('', Site_Year)                           # add blank for first Site_Year so we can start up without any data showing

data <- readRDS('inst/data.RDS')

about_site <- includeMarkdown('inst/about_site.md')




# User interface ---------------------
ui <- page_sidebar(
   theme = bs_theme(bootswatch = 'cosmo', version = 5),     # bslib version defense. Use version_default() to update
   useShinyjs(),
   
   title = 'COMBB: Data Sensemaking Tool',
   
   sidebar = 
      sidebar(
         add_busy_spinner(spin = 'fading-circle', position = 'bottom-left', onstart = FALSE, timeout = 500),
         
         #         card(textInput('intervieweeID', 'Interviewee ID', value = '', width = NULL, placeholder = 'Enter Interviewee name or ID')),  # I like this at the bottom better ....
         
         card(
            card_header (h4("Data Viewing Controls")),
            selectInput('Site_Year', label = 'Site and year', choices = Site_Year$Site_Year, selected = NULL),
            
            sliderInput('period', label = 'Time period', min = 0, max = 0, value = c(0, 0)),
            
            
            materialSwitch('grab', label = 'Plot grab sample data', 
                           value = FALSE),
            
            materialSwitch('sensor', label = 'Plot sensor data', 
                           value = FALSE),
            
            materialSwitch('plot.threshold', label = 'Plot comparison threshold', 
                           value = FALSE),
            
            numericInput('threshold', label = 'Comparison threshold', value = 5,
                         min = 0, step = 1),  
            
            materialSwitch('dist.plot', label = 'Show distribution plot', value = FALSE),
            
            
            br(),
            
            span(HTML('<h5 style="display: inline-block;">Aggregation</h5>')),
            
            selectInput('interval', label = span('Interval', 
                                                 tooltip(bs_icon('info-circle'), 'This is a hover tooltip')), 
                        choices = aggreg.choices),
            
            selectInput('method', label = span('Statistic', 
                                               tooltip(bs_icon('info-circle'), 'This is a hover tooltip with 300 ms delay', options = list(delay = 300))), 
                        choices = method.choices, selected = 'mean'),
            
            materialSwitch('moving.window', label = span('Moving window', 
                                                         tooltip(bs_icon('info-circle'), 'This tooltip requres clicks', options = list(trigger = 'click'))
            )),
            #           materialSwitch('moving.window', label = 'Moving window'),
            
            actionButton('reset', 'Reset', width = '25%'),
            
            
            br(),
            hr(),
            
            textInput('intervieweeID', 'Interviewee', value = '', width = NULL, placeholder = 'Enter Interviewee name or ID'),
            
            tags$style(".btn {width: 50%;}"),
            downloadButton('get_log', label = 'End interview'),
            
            tags$img(height = 77, width = 133, src = 'umass_logo.png'),
            
            br(),
            
            actionLink('about_site', label = 'About this site')
            
         ),
         width = 340
      ),
   
   card(
      card_header (h3("Dissolved Oxygen Data Viewer")),
      navset_pill(
         nav_panel('Plot',
                   br(),
                   fluidRow(
                      column(width = 10,
                             dygraphOutput('plot')
                      ),
                      column(width = 2, 
                             plotOutput('sinaplot'))
                   ),
         ),
         nav_panel('Sensor table',
                   DTOutput('sensor.table')),
         nav_panel('Grab sample table',
                   DTOutput('grab.table')),
         nav_panel('Summary Stats',
                   gt_output('stats'))
      )
   )
)



# Server -----------------------------
server <- function(input, output, session) {
   
   # bs_themer()                                                     # uncomment to select a new theme
   # print(getDefaultReactiveDomain())
   
   disable('period')                                                 # this is dim while it shows 0,0
   disable('method')
   disable('moving.window')
   
   
   observeEvent(input$about_site, {
      showModal(modalDialog(
         about_site, title = 'About this site', easyClose = TRUE, fade = TRUE, footer = modalButton('OK'), size = 'l'))
   })
   
   
   
   session$userData$y.range <- c(min(c(data$DO, data$Grab_DO), na.rm = TRUE), max(c(data$DO, data$Grab_DO), na.rm = TRUE))        # full range of DO data
                                  
   
   
   observeEvent(input$Site_Year, {                                    # --- New site/year selected. Select site and year (entire period) and update time period slider
      if(input$Site_Year != '') {
         session$userData$dataset <- data[data$Site_Year == input$Site_Year, ] 
         
         minmax <- c(min(session$userData$dataset$Date_Time), max(session$userData$dataset$Date_Time))
         freezeReactiveValue(input, 'period')
         updateSliderInput('period', min = minmax[1], max = minmax[2], value = minmax,
                           timeFormat = '%b %e', session = getDefaultReactiveDomain())
         enable('period')
         session$userData$keep.date.window <- FALSE
         session$userData$redraw.stats <- TRUE
         if(input$interval != 'None')                                   #     if aggregation is on, need to reaggregate (means we're pulling data twice; I don't think we care)
            session$userData$dataset <- buzz.aggregate(data, input$Site_Year, session$userData$period, input$interval, input$method, input$moving.window, input$threshold)
         buzz.plots(input, output, session = getDefaultReactiveDomain())
      }
   })
   
   
   observeEvent(input$period, {                                      # --- Period selected. Select site, year, and period
      session$userData$period <- as.POSIXct(floor_date(as.POSIXct(input$period) - hours(4), 'days')) + hours(4)   # round to midnight (adjusted for EDT)
      session$userData$dataset <- data[data$Site_Year == input$Site_Year &
                                          data$Date_Time >= session$userData$period[1] & data$Date_Time <= session$userData$period[2], ]
      session$userData$keep.date.window <- FALSE
      session$userData$redraw.stats <- TRUE
      if(input$interval != 'None')                                   #     if aggregation is on, need to reaggregate 
         session$userData$dataset <- buzz.aggregate(data, input$Site_Year, session$userData$period, input$interval, input$method, input$moving.window, input$threshold)
      buzz.plots(input, output, session = getDefaultReactiveDomain())
   })
   
   
   observeEvent(input$threshold, {                                   # --- Comparison threshold
      session$userData$keep.date.window <- TRUE
      session$userData$redraw.stats <- TRUE
      buzz.plots(input, output, session = getDefaultReactiveDomain())
   }, ignoreInit = TRUE)
   
   
   observeEvent(input$grab, {                                        # --- Plot grab samples    
      session$userData$keep.date.window <- TRUE
      session$userData$redraw.stats <- TRUE
      buzz.plots(input, output, session = getDefaultReactiveDomain())
   })
   
   
   observeEvent(input$interval, {                                    # --- Aggregation interval
      if(input$interval == 'None') {
         disable('method')
         disable('moving.window')
      }
      else                                                           #     if interval is not none, enable method and moving window
      {
         enable('method')
         enable('moving.window')
      }
      session$userData$keep.date.window <- TRUE
      session$userData$redraw.stats <- TRUE
      session$userData$dataset <- buzz.aggregate(data, input$Site_Year, session$userData$period, input$interval, input$method, input$moving.window, input$threshold)
      buzz.plots(input, output, session = getDefaultReactiveDomain())
   }, ignoreInit = TRUE)
   
   
   observeEvent(list(input$method, input$moving.window), {           # --- Aggregation method or moving window
      session$userData$keep.date.window <- TRUE
      session$userData$redraw.stats <- TRUE
      session$userData$dataset <- buzz.aggregate(data, input$Site_Year, session$userData$period, input$interval, input$method, input$moving.window, input$threshold)
      buzz.plots(input, output, session = getDefaultReactiveDomain())
   }, ignoreInit = TRUE)
   
   
   observeEvent(list(input$dist.plot, input$plot.threshold), {
      # --- Remaining controls that don't need to do anything but keep date window and update plot    
      session$userData$keep.date.window <- TRUE
      session$userData$redraw.stats <- FALSE
      buzz.plots(input, output, session = getDefaultReactiveDomain())
   })
   
   
   observeEvent(input$reset, {                                       # --- Reset inputs
      
      session$userData$dataset <- data[data$Site_Year == input$Site_Year, ]                           # Updating time period slider is crazy
      minmax <- c(min(session$userData$dataset$Date_Time), max(session$userData$dataset$Date_Time))
      freezeReactiveValue(input, 'period')
      updateSliderInput('period', min = minmax[1], max = minmax[2], value = minmax,
                        timeFormat = '%b %e', session = getDefaultReactiveDomain())
      enable('period')
      
      updateNumericInput(session, inputId = 'threshold', value = 5)
      updateMaterialSwitch(session, inputId = 'plot.threshold', value = FALSE)
      
      updateMaterialSwitch(session, inputId = 'dist.plot', value = FALSE)
      updateMaterialSwitch(session, inputId = 'sensor', value = FALSE)
      updateMaterialSwitch(session, inputId = 'grab', value = FALSE)
      
      updateSelectInput(session, 'interval', selected = 'None')
      updateSelectInput(session, 'method', selected = 'mean')
      
      updateMaterialSwitch(session, inputId = 'moving.window', value = FALSE)
   })
   
   
   output$get_log <- downloadHandler(
      filename = function() {
         x <- paste0('sensemaking', ifelse(input$intervieweeID != '', '_', ''), input$intervieweeID)
         gsub('[ <>:"/\\|?*]', '_', x) |>
            gsub('[_]+', '_', x = _) |>
            paste0('.txt')
      },
      content = function(file) {
         write.table(session$userData$log, file, sep = '\t', row.names = FALSE, quote = FALSE)
      }
   )
}   


shinyApp(ui, server)
