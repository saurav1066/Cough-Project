#
#This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/
#



library(shiny)
library(readxl)
library(dplyr)
library(zoo)

#Reading required excel files
ds <- read_excel("ds.xlsx")
pro <- read_excel("pro.xlsx")



#converting csv to data frame
subject_list <- data.frame(ds)
input_file <- data.frame(pro)


#UI
ui <- fluidPage(
  titlePanel("Patient Visualization"),
  sidebarLayout(
    sidebarPanel(
      fluidRow( h3("Select Options"),
                #Selection box for subject Ids
                selectInput(inputId = "subject_id",
                            label = "Select Subject ID",
                            choices = subject_list$USUBJID)
      )
      
    ),
    
    #Plot in the main panel
    mainPanel(
      h3("Plot"),
      plotOutput(outputId = "plot")
    )
  )
  
  
)

#Server
server <- function(input, output, session) {
  
  #Selecting the data only for selected subject id
  observe({
    selected_subject <- reactive({
      input_file[input_file$USUBJID == input$subject_id,]
    })
    
    output$plot <- renderPlot({
      input_file <- selected_subject()
      
      #creating 3 different dataframes for the three required subsets
      stool <- input_file[input_file$QSTEST == "CDAI01-Daily Number of Stools",]
      abdominal_pain <- input_file[input_file$QSTEST == "CDAI01-Daily Average Abdominal Pain",]
      well_being <- input_file[input_file$QSTEST == "CDAI01-Daily Average General Well-being",]
      
      
      #Running average
      stool <- stool %>%
        arrange(QSDY) %>%
        mutate((avg = ifelse(row_number() >= 7 & !is.na(QSSTRESN),
                             rollapply(QSSTRESN, width = 7,
                                       FUN = function(x) if(sum(!is.na(x)) >= 4)
                                         mean(x, na.rm = TRUE)
                                       else NA, align = "right", fill = NA),
                             NA)))
      
      print(stool)
    })
    
  })
  
  
  
}

shinyApp(ui, server)