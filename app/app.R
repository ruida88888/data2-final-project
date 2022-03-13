# link to my app: https://ruida.shinyapps.io/David_Final/
require(dplyr)
require(ggplot2)
library(tidyverse)

#construct UI
ui <- fluidPage(selectInput("City", "City:",
                            c("CITY OF LONDON","CAMDEN","CITY OF WESTMINSTER")),
                selectInput("Year", "Year:",
                            c("2005","2010")),
                            plotOutput("plot"))
  
  #build server
server<- function(input, output){
  #read building data
  Data <- read_csv("Data.csv")
  
  plot_data<- reactive({ 
    plot_data <- Data %>% dplyr::filter(City==input$City) %>% dplyr::filter(Year==as.numeric(input$Year))
    return(plot_data)
  })

  output$plot <- renderPlot({ 
    ggplot(plot_data(), aes(x=`Week Number`, y=`Average House Prices`, group=Year, colour = Year)) +
    geom_errorbar(aes(ymin=`Average House Prices`-SD, ymax=`Average House Prices`+SD), width=.1,) +
    geom_line() + geom_point()+
    theme(plot.title = element_text(size=14, face="bold",hjust = 0.5))
  
  })
   
}

#run app
shinyApp(ui, server)

