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
library(ggplot2)

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
      
      abdominal_pain <- abdominal_pain %>%
        arrange(QSDY) %>%
        mutate((avg = ifelse(row_number() >= 7 & !is.na(QSSTRESN),
                             rollapply(QSSTRESN, width = 7,
                                       FUN = function(x) if(sum(!is.na(x)) >= 4)
                                         mean(x, na.rm = TRUE)
                                       else NA, align = "right", fill = NA),
                             NA)))
      
      well_being <- well_being %>%
        arrange(QSDY) %>%
        mutate((avg = ifelse(row_number() >= 7 & !is.na(QSSTRESN),
                             rollapply(QSSTRESN, width = 7,
                                       FUN = function(x) if(sum(!is.na(x)) >= 4)
                                         mean(x, na.rm = TRUE)
                                       else NA, align = "right", fill = NA),
                             NA)))
      
      #New running average on visit widow
      stool$running_average <- sapply(1:nrow(stool), function(row){
        
        #GET QSDY values for current row
        current_qsd_value <- stool$QSDY[row]
        
        #Get rows for past 7 QSDY 
        past_rows <- which(stool$QSDY %in% (current_qsd_value-1):(current_qsd_value-7))
        
        #condition for rejection
        
        if(length(past_rows) < 4){
          return(0)
        }
        
        #otherwise compute mean
        return(mean(stool[past_rows,"QSORRES"]))
      })
      
      
      
      
      
    #Plotting individual datasets
      
     ggplot(data = well_being,
            aes(x = QSDY, y = well_being[,ncol(well_being)]))+
       scale_x_continuous(limits = c(1,max(well_being["QSDY"])))+
       geom_point()+
       geom_line()+
       labs(x = "VISIT",  y = "seven day average")+
       theme_minimal()
     
     ggplot(data = stool,
            aes(x = QSDY, y = stool[,ncol(stool)]))+
       scale_x_continuous(limits = c(1,max(stool["QSDY"])))+
       geom_point()+
       geom_line()+
       labs(x = "VISIT",  y = "seven day average")+
       theme_minimal()
     
     ggplot(data = well_being,
            aes(x = QSDY, y = well_being[,ncol(well_being)]))+
       scale_x_continuous(limits = c(1,max(well_being["QSDY"])))+
       geom_point()+
       geom_line()+
       labs(x = "VISIT",  y = "seven day average")+
       theme_minimal()
      
    })
    
  })
  
  
  
}

shinyApp(ui, server)