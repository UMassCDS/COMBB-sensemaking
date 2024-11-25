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
data <- readRDS('inst/data.RDS')


about_site <- includeMarkdown('inst/about_site.md')

tooltip_site <- includeMarkdown('inst/tooltip_site.md')
tooltip_period <- includeMarkdown('inst/tooltip_period.md')
tooltip_grab <- includeMarkdown('inst/tooltip_grab.md')
tooltip_sensor <- includeMarkdown('inst/tooltip_sensor.md')
tooltip_threshold <- includeMarkdown('inst/tooltip_threshold.md')
tooltip_distribution <- includeMarkdown('inst/tooltip_distribution.md')
tooltip_aggregation <- includeMarkdown('inst/tooltip_aggregation.md')
tooltip_interval <- includeMarkdown('inst/tooltip_interval.md')
tooltip_statistic <- includeMarkdown('inst/tooltip_statistic.md')
tooltip_movingwindow <- includeMarkdown('inst/tooltip_movingwindow.md')
tooltip_reset <- includeMarkdown('inst/tooltip_reset.md')
tooltip_interviewee <- includeMarkdown('inst/tooltip_interviewee.md')
tooltip_end <- includeMarkdown('inst/tooltip_end.md')


tipped <- function(text, tooltip, delay = 300)                                            # display text with a tooltip
   span(text, tooltip(bs_icon('info-circle'), tooltip, options = list(delay = delay)))


# version <- 1                               #  version <- 1 forces stats to front tab and opens with sensor data on (umassdsl version)
version <- 2                               #  version <- 2 moves stats to fourth tab and opens with sensor data off (combb version)




# User interface ---------------------
ui <- page_sidebar(
   theme = bs_theme(bootswatch = 'cosmo', version = 5),     # bslib version defense. Use version_default() to update
   useShinyjs(),
   
   tags$style(HTML('.modal-footer .btn {width: 10%;}')),    # set button width for About this site
   
   title = 'COMBB: Data Sensemaking Tool',
   
   sidebar = 
      sidebar(
         add_busy_spinner(spin = 'fading-circle', position = 'bottom-left', onstart = FALSE, timeout = 500),
         
         card(
            card_header (h4("Data Viewing Controls")),
            
            actionLink('about_site', label = 'About this site'),
            
            br(),
            
            selectInput('Site_Year', label = tipped('Site and year', tooltip_site), choices = Site_Year$Site_Year),
            
            sliderInput('period', label = tipped('Time period', tooltip_period), min = 0, max = 0, value = c(0, 0)),
            
            
            materialSwitch('grab', label = tipped('Plot grab sample data', tooltip_grab),
                           value = FALSE),
            
            materialSwitch('sensor', label = tipped('Plot sensor data', tooltip_sensor),                 # if version 1, start with sensor ON
                           value = version == 1),
            
            materialSwitch('plot.threshold', label = tipped('Plot comparison threshold', tooltip_threshold),
                           value = FALSE),
            
            numericInput('threshold', label = tipped('Comparison threshold', tooltip_threshold), value = 5,
                         min = 0, step = 1),  
            
            materialSwitch('dist.plot', label = tipped('Show distribution plot', tooltip_distribution), value = FALSE),
            
            br(),
            
            tipped(HTML('<h5 style="display: inline-block;">Aggregation</h5>'), tooltip_aggregation),
            
            selectInput('interval', label = tipped('Interval', tooltip_interval), 
                        choices = aggreg.choices),
            
            selectInput('method', label = tipped('Statistic', tooltip_statistic), 
                        choices = method.choices, selected = 'mean'),
            
            materialSwitch('moving.window', label = tipped('Moving window', tooltip_movingwindow)),
            
            tipped(actionButton('reset', 'Reset', width = '25%'), tooltip_reset), 
            
            hr(),
            
            textInput('intervieweeID', tipped('Interviewee', tooltip_interviewee), value = '', width = NULL, placeholder = 'Enter Interviewee name or ID'),
            
            tags$style('.btn {width: 50%;}'),
            tipped(downloadButton('get_log', label = 'End interview'), tooltip_end),
            
            tags$img(height = 77, width = 133, src = 'umass_logo.png')
         ),
         width = 340
      ),
   
   card(
      card_header (h3("Dissolved Oxygen Data Viewer")),
      navset_pill(
         nav_panel('Plot',
                   br(),
                   br(),
                   fluidRow(
                      column(width = 10,
                             dygraphOutput('plot')
                      ),
                      column(width = 2, 
                             plotOutput('sinaplot'))
                   ),
                   
                   br(),
                   if(version == 1)                                  # if version 1, display stats on first tab
                      gt_output('stats')
         ),
         
         nav_panel('Sensor table',
                   DTOutput('sensor.table')),
         nav_panel('Grab sample table',
                   DTOutput('grab.table')),
         if(version == 2)                                            # if version 2, display stats on fourth tab
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
   
   
   session$userData$x.range <- c(min(data$Date_Time, na.rm = TRUE), max(data$Date_Time, na.rm = TRUE))                           # fix full date range
   session$userData$y.range <- c(min(c(data$DO, data$Grab_DO), na.rm = TRUE), max(c(data$DO, data$Grab_DO) * 1.15, na.rm = TRUE)) # fix full range of DO data (pad 15% at top)
   
   
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
   
   
   observeEvent(list(input$grab, input$sensor), {                    # --- Plot grab samples or sensor data
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
      
      session$userData$redraw.stats <- TRUE
      session$userData$keep.date.window <- FALSE
      buzz.plots(input, output, session = getDefaultReactiveDomain())                                 # redraw plot to reset date slider
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
