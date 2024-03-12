library(shiny)
library(haven)
library(ggplot2)

# Helper Functions --------------------------------------------------------

#Function to select and load required data
filter_data <- function() {
  data <- read_sas("cough-project/Cough-App_V2/fa.sas7bdat")
  
  #Selecting required data
  filtered_df <- subset(data, FACAT == "Cough Assessment" & FATESTCD == "CHHR" & FASPID != "HR99")
  print(filtered_df$FASTRESN)
  
  #checking unique subject ids
  subject_id <- unique(filtered_df$USUBJID)
  
  #checking unique visits
  visit_id <- unique(filtered_df$VISIT)
  
  
  # Return the processed data and unique IDs
  list(cough_data = filtered_df, subject_id = subject_id, visit_id = visit_id )
  
  
}

#Calling the helper function
result <- filter_data()
# UI --------------------------------------------------------

ui <- fluidPage(
  titlePanel("Cough Application V2"),
  
  sidebarLayout(
    sidebarPanel (
      #Selecting required visit and subject for observation as input
      selectInput(inputId = "SubjectID",
                  label = "Select Subject ID",
                  choices = result$subject_id),
      
      selectInput(inputId = "Visit",
                  label = "Select Visit Number",
                  choices = result$visit_id),
      
    ),
    
    mainPanel(
      plotOutput("cough_count_plot")
    )
  )
)

# Server --------------------------------------------------------

server <- function(input, output, session) {
  observe({
    # Selecting required data as per input
    selected_data <- reactive({
      result$cough_data[result$cough_data$VISIT == input$Visit &
                          result$cough_data$USUBJID == input$SubjectID,]
    })
    new_df <- selected_data()
    print(new_df)
  })
}

shinyApp(ui, server)