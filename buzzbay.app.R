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
            sliderInput('period', label = 'Time period', min = 6, max = 9, value = c(6, 9)), 
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
   
   
   observeEvent(input$siteYear, {
    print('siteyear triggered')
        session$userData$sensor <- sensors[sensors$siteYear == input$siteYear, ]
      zzz <<- session$userData$sensor
      minmax <- c(min(session$userData$sensor$Date_Time), max(session$userData$sensor$Date_Time))
      updateSliderInput('period', min = minmax[1], max = minmax[2], value = minmax,
                        timeFormat = '%b %e',
                        session = getDefaultReactiveDomain())
      session$userData$keep.date.window <- FALSE
      
   })
   
   observeEvent(list(input$siteYear, input$period), {
      print('siteyear and period triggered')
      session$userData$window <- session$userData$sensor[session$userData$sensor$Date_Time >= input$period[1] & 
                                                            session$userData$sensor$Date_Time <= input$period[2], ]   
      cat('=== window = ', paste(dim(session$userData$window), collapse = ', '), ' ===\n', sep = '')
      session$userData$keep.date.window <- FALSE
   })
   
   observeEvent(input$units, {
      session$userData$keep.date.window <- TRUE
   })
   
   observeEvent(input$grab.bag, {                        # nothing here yet
      session$userData$keep.date.window <- TRUE
   })
   
   observeEvent(list(input$siteYear, input$period, input$units, input$grab.bag), {
      cat('--- plot triggered\n')
      if(dim(session$userData$window)[1] > 0) {
         vars <- session$userData$window[, c('Date_Time', unit.vars[as.integer(input$units)])]
         #  vars <- xts(vars[,-1], vars[,1])
         cat('--- siteYear:\n')
         print(dim(input$siteYear))
         cat('--- period:\n')
         print(input$period)
         cat('--- units:\n')
         print(input$units)
         
         
         
         #vars <- data.frame(1:1000, runif(1000))
         #vars <- head(vars)
         
         #vars <- head(vars)
         
         
         #      cat('converting to POSIXct\n')
         # #     vars$Date_Time <- as.POSIXct(vars$Date_Time)
         #    #  vars <- head(vars)
         #      cat('here it is\n')
         #      print(head(vars))
         #      cat('converting to xts\n')
         #    #  vars <- xts(vars$DO, vars$Date_Time)          # not needed it seems
         #      cat('here it is as xts\n')
         #      print(head(vars))
         #      cat('lets go!\n')
         #      
         #      vars <<- vars
         #      
         
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
      }})
}   


shinyApp(ui, server)
