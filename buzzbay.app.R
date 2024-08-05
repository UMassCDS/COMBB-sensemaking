# buzzbay - Buzzards Bay data display tool
# B. Compton, 31 Jul 2024


library(shiny)
library(bslib)
library(shinybusy)
library(shinyWidgets)


sites <- read.csv('inst/sitenames.csv')
sites <- sites[sites$include == TRUE,]

print(sites)



# User interface ---------------------
ui <- page_sidebar(
   theme = bs_theme(bootswatch = 'cerulean', version = 5),   # bslib version defense. Use version_default() to update
   
   title = 'buzzbay (dev version)',
   
   sidebar = 
      sidebar(
         add_busy_spinner(spin = 'fading-circle', position = 'top-left', onstart = FALSE, timeout = 500),
         
         card(
            selectInput('site', label = 'Site', choices = split(sites$site, sites$description)),
            radioButtons('units', label = 'Units', choices = list('mg/l', '% saturation')),
            sliderInput('period', label = 'Time period', min = 6, max = 9, value = c(6, 9)),  ######### need to read
            # dates from file and set these accordingly
            
            textInput('threshold', label = 'Comparison threshold', value = '', placeholder = 'n.nn',
                      width = '40%'),   
            materialSwitch(inputId = 'plot.threshold', label = 'Plot comparison threshold', 
                           value = FALSE),
            textInput('exceedance', label = 'Exceedance threshold', value = '', placeholder = 'nn',
                      width = '40%'),
            materialSwitch(inputId = 'grab.bag', label = 'Display grab-bag samples', 
                          value = FALSE)
         ),
         width = 290
      ),
   mainPanel(
      ggvisOutput(plot_id = 'dissolved_oxygen')
   )
   
   
   
)



# Server -----------------------------
server <- function(input, output, session) {
   
   #bs_themer()                                 # uncomment to select a new theme
   #  print(getDefaultReactiveDomain())
   
   data |>
      ggvis(x = ~rows, y = ~random) |>
      bind_shiny('dissolved_oxygen')
   
}

shinyApp(ui, server)


