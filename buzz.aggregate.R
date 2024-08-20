'buzz.aggregate' <- function(data, input, session) {
   
   # update aggregation for Buzzard's Bay app
   # Arguments: 
   #     data                       full dataset
   #     input, session             Shiny variables
   # Result:
   #     session$userData$dataset   fresh dataset, aggregated for all 4 DO variables
   # B. Compton, 20 Aug 2024
   
   
   
   print(session$userData$period)
   
   session$userData$dataset <- data[data$Site_Year == input$Site_Year &                                  # get fresh dataset
                                       data$Date_Time >= session$userData$period[1] & data$Date_Time <= session$userData$period[2], ]
   
   
}