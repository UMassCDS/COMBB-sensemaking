# buzzbay - Buzzards Bay data display tool
# B. Compton, 31 Jul 2024



library(shiny)
library(bslib)
library(shinybusy)
library(shinyWidgets)
library(dygraphs)
# library(xts)

source('dygraphs_plugins.R')


unit.vars <- c('DO', 'DO_Pct_Sat')
unit.names <- c('mg/L', '% saturation')
sites <- read.csv('inst/sitenames.csv')
sites <- sites[sites$include == TRUE,]

siteYear <- readRDS('inst/siteYear.RDS')
sensors <- readRDS('inst/sensors.RDS')



# User interface ---------------------
ui <- page_sidebar(
   theme = bs_theme(bootswatch = 'cerulean', version = 5),   # bslib version defense. Use version_default() to update
   
   title = 'buzzbay (dev version)',
   
   sidebar = 
      sidebar(
         add_busy_spinner(spin = 'fading-circle', position = 'top-left', onstart = FALSE, timeout = 500),
         
         card(
            selectInput('siteYear', label = 'Site', choices = siteYear$siteYear),
            sliderInput('period', label = 'Time period', min = 0, max = 1e12, value = c(0, 1e12)), 
            radioButtons('units', label = 'Units', choiceNames = as.list(unit.names), choiceValues = 1:2),
            
            textInput('threshold', label = 'Comparison threshold', value = '', placeholder = 'n.nn',
                      width = '40%'),   
            materialSwitch(inputId = 'plot.threshold', label = 'Plot comparison threshold', 
                           value = FALSE),
            textInput('exceedance', label = 'Exceedance threshold', value = '', placeholder = 'nn',
                      width = '40%'),
            materialSwitch(inputId = 'grab.bag', label = 'Display grab-bag samples', 
                           value = FALSE)
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
   
   #bs_themer()                                 # uncomment to select a new theme
   #  print(getDefaultReactiveDomain())
   
   
   observe({      # observeEvent(input$siteYear, {
      print('siteyear triggered')
      session$userData$window <- session$userData$sensor <- sensors[sensors$siteYear == input$siteYear, ]
      cat('sensors = ', dim(session$userData$sensor)[1], '\n', sep = '')
      cat('window = ', dim(session$userData$window)[1], '\n', sep = '')
      cat('---\n')
      zzz <<- session$userData$sensor
      minmax <- c(min(session$userData$sensor$Date_Time), max(session$userData$sensor$Date_Time))
      updateSliderInput('period', min = minmax[1], max = minmax[2], value = minmax,
                        timeFormat = '%b %e',
                        session = getDefaultReactiveDomain())
      session$userData$keep.date.window <- FALSE
      
   })
   
   # observeEvent(list(input$siteYear, input$period), {
   #    print('siteyear and period triggered')
   #    session$userData$window <- session$userData$sensor[session$userData$sensor$Date_Time >= input$period[1] & 
   #                                                          session$userData$sensor$Date_Time <= input$period[2], ] 
   #    cat('period = ', input$period[1], ' - ', input$period[2], '\n', sep =)
   #    cat('window = ', dim(session$userData$window)[1], '\n', sep = '')
   #    cat('---\n')
   #    session$userData$keep.date.window <- FALSE
   # })
   
   observeEvent(input$units, {
      session$userData$keep.date.window <- TRUE
   })
   
   observeEvent(input$grab.bag, {                        
      # nothing here yet
      session$userData$keep.date.window <- TRUE
   })
   
   observeEvent(list(input$siteYear, input$period, input$units, input$grab.bag), {
      session$userData$window <- session$userData$sensor[session$userData$sensor$Date_Time >= input$period[1] & 
                                                            session$userData$sensor$Date_Time <= input$period[2], ] 
      cat('--- plot triggered ', dim(session$userData$window)[1], '\n', sep = '')
      if(dim(session$userData$window)[1] > 0) {
         vars <- session$userData$window[, c('Date_Time', unit.vars[as.integer(input$units)])]
         
         output$plot <- renderDygraph({
            dygraph(vars, main = 'Dissolved oxygen', ylab = unit.names[as.integer(input$units)]) |>
               dyAxis('x', gridLineColor = '#D0D0D0') |>
               dyAxis('y', gridLineColor = '#D0D0D0') |>
               dySeries(color = '#3C2692') |>
               #    dySeries('grab.bag', drawPoints = TRUE, strokeWidth = 0) |>     # this is how we'll do grab-bag points
               dyRangeSelector(retainDateWindow = session$userData$keep.date.window) |>
               dyUnzoom() |>
               dyCrosshair(direction = "vertical")
         })
      }
   }    ) #       , ignoreInit = TRUE)
}   


shinyApp(ui, server)
