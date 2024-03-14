library(shiny)
library(haven)
library(ggplot2)
library(gt)
library(tidyverse)

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
      
      gt_output("summary_table"),
      
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
    
    output$cough_count_plot <- renderPlot({
      ggplot(new_df, aes(x = FASPID, y = FASTRESN, group = 1))+
        geom_poit()+
        geom_line()+
        theme_minimal()
      
    })
    
    #generating summary table
    output$summary_table <- render_gt({
      new_df %>%
        summarise(
          count = n(),
          mean = mean(FASTRESN, na.rm = TRUE),
          sd = sd(FASTRESN, na.rm = TRUE),
          q1 = quantile(FASTRESN, 0.25, na.rm = TRUE),
          median = median(FASTRESN, na.rm = TRUE),
          q3 = quantile(FASTRESN, 0.75, na.rm = TRUE)
        ) %>%
        gather(key = "Statistic", value = "Value") %>%
        gt() %>%
        tab_header(
          title = "Summary Statistics"
        )
    })
    
    
  })
}

shinyApp(ui, server)