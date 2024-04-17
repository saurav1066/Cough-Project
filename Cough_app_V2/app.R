library(shiny)
library(haven)
library(ggplot2)
library(gt)
library(tidyverse)
library(dplyr)
library(reshape2)


# Helper Functions --------------------------------------------------------

#Function to select and load required data
filter_data <- function() {
  data <- read_sas("fa.sas7bdat")
  
  #Selecting required data
  filtered_df <- subset(data, FACAT == "Cough Assessment" & FATESTCD == "CHHR" & FASPID != "HR99")
  
  #filtering for required data 
  low_activity <- subset(data, FACAT == "Cough Assessment" & FATEST == "Low Activity Cough Count" & FASPID != "HR99")
  mid_activity <- subset(data, FACAT == "Cough Assessment" & FATEST == "Medium Activity Cough Count" & FASPID != "HR99")
  high_activity <- subset(data, FACAT == "Cough Assessment" & FATEST == "High Activity Cough Count" & FASPID != "HR99")
  
  
  #checking unique subject ids
  subject_id <- unique(filtered_df$USUBJID)
  
  #checking unique visits
  visit_id <- unique(filtered_df$VISIT)
  
  
  # Return the processed data and unique IDs
  list(cough_data = filtered_df, subject_id = subject_id, visit_id = visit_id, low = low_activity, mid = mid_activity, high = high_activity )
  
  
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
      
      h3("Summary of Coughs"),
      
      gt_output("summary_table")
      
    ),
    
    mainPanel(
      plotOutput("cough_count_plot"),
      plotOutput("activity_plot")
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
    
    
    low_activity <- reactive({
      result$low[result$low$VISIT == input$Visit &
                   result$low$USUBJID == input$SubjectID,]
    })
    
    mid_activity <- reactive({
      result$mid[result$mid$VISIT == input$Visit &
                   result$mid$USUBJID == input$SubjectID,]
    })
    
    high_activity <- reactive({
      result$high[result$high$VISIT == input$Visit &
                    result$high$USUBJID == input$SubjectID,]
    })
    
    
    new_df <- selected_data()
    low_activity_dur <- low_activity()
    mid_activity_dur <- mid_activity()
    high_activity_dur <- high_activity()
    
    #Selecting only necessary column from the duration dataframes
    low_activity_dur <- low_activity_dur %>% select(FASPID,LOW = FASTRESN)
    mid_activity_dur <- mid_activity_dur %>% select(FASPID,MID = FASTRESN)
    high_activity_dur <- high_activity_dur %>% select(FASPID,HIGH = FASTRESN)
    
    #merging all three activity values
    activity_df <- low_activity_dur %>%
      inner_join(mid_activity_dur, by = "FASPID") %>%
      inner_join(high_activity_dur, by = "FASPID")
    
    
    
    #Creating output plot
    output$cough_count_plot <- renderPlot({
      #Generating Plot
      ggplot(new_df, aes(x = FASPID, y = FASTRESN, group = 1))+
        geom_point()+
        geom_line()+
        ggtitle("Cough counts per hour")+
        geom_hline(yintercept = mean(new_df$FASTRESN, na.rm = TRUE), linetype="dashed", color = "green")+
        theme_minimal()
      
    })
    
    #Creating the activity plot
    
    # Reshape data
    df_melt <- melt(activity_df, id.vars = 'FASPID', variable.name = 'Variable', value.name = 'Value')
    
    # Create plot
    
    output$activity_plot <- renderPlot({
      ggplot(df_melt, aes(x = FASPID, y = Value, color = Variable, group = Variable)) +
        geom_point() +
        geom_line() +
        ggtitle("Cough counts per hour per activity level")+
        labs(x = "FASPID", y = "Value") +
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