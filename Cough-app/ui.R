# Module UI
coughDataUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    
    titlePanel("Cough Data Application"),
    tabsetPanel(
      tabPanel("Visualization",
               sidebarLayout(
                 sidebarPanel(
                   h3("Select Options"),
                   
                   #Selecting required visit and subject for observation as input
                   selectInput(inputId = "SubjectID", 
                               label = "Select Subject ID", 
                               choices = subject_id),
                   
                   # selectInput("Session", "Select Session ID",choices = session_id),
                   
                   # Condition Statement for popping out another input 
                   selectInput(inputId = "Condition",
                               label = "Select Choice",
                               choices = c("Single Insight", "Comparision")),
                   
                   conditionalPanel(
                     condition = "input.Condition == 'Single Insight'",
                     selectInput(inputId = "Visit",
                                 label = "Select Visit Number", 
                                 choices = visit_id)
                   )
                 ),
                 
                 mainPanel(
                   h3("Generated Plot"),
                   
                   # selectInput(inputId = "Visit", 
                   #             label = "Select Visit Number", 
                   #             choices = visit_id),
                   
                   #generating plot as output
                   plotOutput(outputId = "cough_plot")
                   
                   
                   
                   # #generating a table output
                   # tableOutput("cough_table")
                   
                 )
                 
               )
               
      ),
      tabPanel("Tables",
               mainPanel(
                 h3("Table for the cough counts per hour"),
                 gt_output("gt_table")
               )
      ),
      
      tabPanel("About",
               mainPanel(
                 h3("About the Application"),
                 textOutput('about')
               )
      )
    )
    
  )
}
