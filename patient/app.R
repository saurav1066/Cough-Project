#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)

#reading required files
ds = read_excel("ds.xlsx")
pro = read_excel("pro.xlsx")

#Converting to dataframe
subject_list <- data.frame(ds)
input_file <- data.frame(pro)

#UI
ui <- fluidPage(
  mainPanel(
    h3("Plot")
    plotOutput(outputId = "plot")
    
  )
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)