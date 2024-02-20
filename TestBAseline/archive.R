
# UI Checkbox dummy code --------------------------------------------------

#     # Checkbox for Visit and Sex
#     checkboxGroupInput(inputId = "baseline_condition",
#                        label = "Select Baseline Choice",
#                        choices = c("Visit", "Sex")),
#     
#     # Conditionally display checkboxes for Visit
#     conditionalPanel(
#       condition = "input.baseline_condition.includes('Visit')",
#       
#       # Checkboxes for Visit Number
#       checkboxGroupInput(inputId = "baseline_Visit",
#                          label = "Select Visit Number",
#                          choices = result$visit_id)
#     ),
#     
#     # Conditionally display checkboxes for Sex
#     conditionalPanel(
#       condition = "input.baseline_condition.includes('Sex')",
#       
#       # Checkboxes for Sex
#       checkboxGroupInput(inputId = "baseline_Sex",
#                          label = "Select Sex",
#                          choices = c("Male", "Female"))
#     )
#     
#   ),
#   
# ),
# 


# Server checkbox dummy code ----------------------------------------------

# #Logic for the checkboxes
# new_df <- result$cough_data
# 
# baseline_choice <- input$baseline_condition
# 
# 
# #Condition when Visit is Selected
# 
# if ("Visit" %in% baseline_choice) {
#   
#   #Selectiing Visit to plot 
#   visit <- input$baseline_Visit
#   
#   selected_data <- reactive({
#     result$cough_data[result$cough_data$VISIT == input$baseline_Visit,]
#   })
#   
#   
#   new_df <- selected_data()
#   
#   # Generate UI elements dynamically
#   id_list <- lapply(result$subject_id, as.character)
#   
#   output$baseline_plot <- renderUI({
#     lapply(id_list, function(id) {
#       plotOutput(id)
#     })
#   })
#   
#   # Render plots for each ID
#   lapply(id_list, function(subject) {
#     
#     #Selected number of Visits
#     if(length(visit) <= 1){
#       output[[subject]] <- renderPlot({
#         df <- new_df[new_df$USUBJID == as.numeric(subject),]
#         
#         
#         #Getting sleep, wake and start time of recording values
#         sleep_time <- as.integer(df$hour[df$FAOBJ == "Sleep"])
#         wake_time <- as.integer(df$hour[df$FAOBJ == "Wake"])
#         
#         
#         start_time<- 0 # needs initialization because some subjects do not have a record
#         
#         start_time <-  as.integer(df$hour[df$FAOBJ == "Actual Recording Start Time"])
#         
#         
#         # Counting the occurrences of "cough" for each hour of the day
#         hourly_counts <- table(df$hour[df$FAOBJ == "Cough"])
#         hourly_counts <- data.frame(hourly_counts)
#         
#         #Converting the hour data to integer
#         hourly_counts$Var1 <- as.integer(as.character(hourly_counts$Var1))
#         
#         if (nrow(hourly_counts) == 0){
#           ggplot() +
#             ggtitle(paste("Cough Plot for" ,subject) )+
#             labs(x = "Hour", y = "Count", title = paste0("No data found for ", subject , ' in ', input$baseline_Visit ))
#         }
#         else {
#           #Calling helper function
#           hourly_counts <- hourly_counts_table(df, sleep_time , wake_time,start_time)
#           
#           # Plot with sleep indicators for each visit
#           ggplot(hourly_counts,
#                  aes(x = factor(Var1, levels = unique(Var1)),
#                      y = Freq, color = sleep, group=1)) +
#             geom_line() +
#             geom_point() +
#             ggtitle(paste("Cough Plot for" ,subject) )+
#             scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
#             labs(x = "Hours", y = "Frequency of Cough") +
#             scale_y_continuous(limits = c(0, 290))+
#             theme_minimal()
#           
#           
#           
#         }
#         
#       })
#     }
#     else{
#       
#       output[[subject]] <- renderPlot({
#         df <- result$cough_data[result$cough_data$USUBJID == as.numeric(subject),]
#         
#         if(nrow(df) != 0) {
#           #grouping the dataframe based on visits
#           grouped_df <- split(df, df$VISIT,df$USUBJID)
#           
#           #Initilizing for later use to store plots and dataframes
#           cough_counts <- data.frame()
#           
#           
#           #getting hourly counts of each group of visits
#           for (i in 1:length(grouped_df)) {
#             
#             
#             #Getting the hour where the subject slept
#             sleep_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Sleep"])
#             
#             #Getting the hour where the subject woke up
#             wake_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Wake"])
#             
#             #Getting the hour where the recording started
#             start_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Actual Recording Start Time"])
#             
#             #Calling helper function
#             hourly_count <- hourly_counts_table(grouped_df[[i]], sleep_time,wake_time, start_time)
#             
#             #Adding Visit Column in the hourly count  table
#             hourly_count$Visit <- grouped_df[[i]]$VISIT[1]
#             
#             #Storing all cough counts in a separate dataframe
#             cough_counts <- rbind(cough_counts, hourly_count)
#           }
#           
#           
#           #plotting all the visits in same graph
#           ggplot(cough_counts,
#                  aes(x = Var1, y = Freq, group = Visit, color = Visit)) +
#             geom_line() +
#             geom_point()+
#             ggtitle(paste0("Comparision of all visits for subject ", subject)) +
#             labs(x = "Hours", y = "Frequency of Cough")+
#             theme_minimal()
#         }
#         else{
#           ggplot() +
#             ggtitle(paste("Cough Plot for" ,subject) )+
#             labs(x = "Hour", y = "Count", title = paste0("No data found for ", subject ))
#         }
#         
#         
#       })
#       
#     }
#     
#     
#   })
#   
#   
#   
#   
#   
# }
# 
# if("Sex" %in% baseline_choice){
#   
#   sex <- input$baseline_Sex
#   
#   if(length(sex) <= 1){
#     
#     #Selecting Filtered sex data
#     selected_extension <- reactive({
#       result$extension_data[result$extension_data$SEX == input$baseline_Sex,]
#       
#     })
#     
#     extension <- selected_extension()
#     
#     # Generate UI elements dynamically
#     id_list <- lapply(extension$SUBJID, as.character)
#     
#     output$baseline_plot <- renderUI({
#       lapply(id_list, function(id) {
#         plotOutput(id)
#       })
#     })
#     
#     # Render plots for each ID
#     lapply(id_list, function(subject) {
#       
#       
#       output[[subject]] <- renderPlot({
#         df <- new_df[new_df$USUBJID == as.numeric(subject),]
#         
#         if(nrow(df) != 0) {
#           #grouping the dataframe based on visits
#           grouped_df <- split(df, df$VISIT,df$USUBJID)
#           
#           #Initilizing for later use to store plots and dataframes
#           cough_counts <- data.frame()
#           
#           
#           #getting hourly counts of each group of visits
#           for (i in 1:length(grouped_df)) {
#             
#             
#             #Getting the hour where the subject slept
#             sleep_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Sleep"])
#             
#             #Getting the hour where the subject woke up
#             wake_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Wake"])
#             
#             #Getting the hour where the recording started
#             start_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Actual Recording Start Time"])
#             
#             #Calling helper function
#             hourly_count <- hourly_counts_table(grouped_df[[i]], sleep_time,wake_time, start_time)
#             
#             #Adding Visit Column in the hourly count  table
#             hourly_count$Visit <- grouped_df[[i]]$VISIT[1]
#             
#             #Storing all cough counts in a separate dataframe
#             cough_counts <- rbind(cough_counts, hourly_count)
#           }
#           
#           
#           #plotting all the visits in same graph
#           ggplot(cough_counts,
#                  aes(x = Var1, y = Freq, group = Visit, color = Visit)) +
#             geom_line() +
#             geom_point()+
#             ggtitle(paste0("Comparision of all visits for subject ", subject)) +
#             labs(x = "Hours", y = "Frequency of Cough")+
#             theme_minimal()
#         }
#         else{
#           ggplot() +
#             ggtitle(paste("Cough Plot for" ,subject) )+
#             labs(x = "Hour", y = "Count", title = paste0("No data found for ", subject ))
#         }
#         
#         
#       })
#     })
#     
#     
#   }
#   
#   
#   
#   else{
#     
#     
#     
#   }
#   

