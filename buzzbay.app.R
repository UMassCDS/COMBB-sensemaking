# buzzbay - Buzzards Bay data display tool
# B. Compton, 31 Jul 2024


library(shiny)
library(bslib)
library(shinybusy)
library(shinyWidgets)
library(ggvis)


sites <- read.csv('inst/sitenames.csv')
sites <- sites[sites$include == TRUE,]

siteYear <- readRDS('inst/siteYear.RDS')
sensors <- readRDS('inst/sensors.RDS')

print(sites)
siteYear <<- siteYear


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
            radioButtons('units', label = 'Units', choiceNames = list('mg/L', '% saturation'), choiceValues = 1:2),
            
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
   mainPanel(
      ggvisOutput(plot_id = 'dissolved_oxygen')
   )
   
   
   
)



# Server -----------------------------
server <- function(input, output, session) {
   
   #bs_themer()                                 # uncomment to select a new theme
   #  print(getDefaultReactiveDomain())
   
   # data |>
   #    ggvis(x = ~rows, y = ~random) |>
   #    bind_shiny('dissolved_oxygen')
   
   
   
   
   observeEvent(input$siteYear, {
      session$userData$sensor <- sensors[sensors$siteYear == input$siteYear, ]
      zzz <<- session$userData$sensor
      minmax <- c(min(session$userData$sensor$Date_Time), max(session$userData$sensor$Date_Time))
      updateSliderInput('period', min = minmax[1], max = minmax[2], value = minmax,
                        timeFormat = '%m-%d',
                        session = getDefaultReactiveDomain())
      
   })
   
   observeEvent(list(input$siteYear, input$period), {
      session$userData$window <- session$userData$sensor[session$userData$sensor$Date_Time >= input$period[1] & 
                                                            session$userData$sensor$Date_Time <= input$period[2], ]      
   })
   
   observeEvent(list(input$siteYear, input$period, input$units), {
      cat('input$units = ', input$units, '\n', sep = '')
      print(is.numeric(input$units))
      cat("c('DO', 'DO_Pct_Sat')[input$units] = ", c('DO', 'DO_Pct_Sat')[as.integer(unlist(input$units))], '\n', sep = '')
      print(names(session$userData$window))
      session$userData$window$units <- session$userData$window$DO  # [, 'DO']    #[, c('DO', 'DO_Pct_Sat')[as.integer(unlist(input$units))]]
      session$userData$window |>
         ggvis(x = ~Date_Time, y = ~units) |>
         bind_shiny('dissolved_oxygen')
   })
}   


shinyApp(ui, server)
