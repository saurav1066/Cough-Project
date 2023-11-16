# Test
library(readxl)
library(plotly)



#reading the given excel file
cough_data <- read_excel("1465-0008-coughdata.xlsx")

#converting excel file to data frame
cough_data <- data.frame(cough_data)

#checking unique subject ids
subject_id <- unique(cough_data$USUBJID)
subject_id

#checking unique sessions
session_id <- unique(cough_data$SessionID)
session_id

#test dataset for some session for some subject 
# Select the columns with sessionid = 22 and usubjid = 1002001 into a new data frame
# new_df <- cough_data[cough_data$SessionID == 22 & cough_data$USUBJID == 1002001,]

#Selecting just the columns with subject id
new_df<- cough_data[cough_data$USUBJID == 6004001,]

# Keeping the same headers
colnames(new_df) <- colnames(cough_data)


# Converting the 'fadtc' column to POSIXct format
new_df$FADTC <- as.POSIXct(new_df$FADTC, format = "%Y-%m-%dT%H:%M:%S")

# Creating a new column 'hour' to extract the hour from the timestamp
new_df$hour <- format(new_df$FADTC, "%H")
new_df$hour

#Getting the hour where the subject slept
sleep_time <- as.integer(new_df$hour[new_df$FAOBJ == "Sleep"])
sleep_time

#Getting the hour where the subject woke up
wake_time <- as.integer(new_df$hour[new_df$FAOBJ == "Wake"])
wake_time

#Getting the time when the recording was started
start_time <-  as.integer(new_df$hour[new_df$FAOBJ == "Actual Recording Start Time"])
start_time

#grouping the dataframe based on visits
grouped_df <- split(new_df,new_df$VISIT)
length(grouped_df)
grouped_df[[2]]$hour

# # Counting  the occurrences of "cough" for each hour of the day
# hourly_counts <- table(new_df$hour[new_df$FAOBJ == "Cough"])
# hourly_counts <- data.frame(hourly_counts)
# hourly_counts

#getting hourly counts of each group of visits
cough_counts <- data.frame()

for (i in 1:length(grouped_df)) {
  
  # Counting  the occurrences of "cough" for each hour of the day
  hourly_count <- table(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ =="Cough"])
  hourly_count<- data.frame(hourly_count)
  
  #Converting the hour data to integer
  hourly_count$Var1 <- as.integer(as.character(hourly_count$Var1))
  
  #Filling Missing Values
  hourly_count <- merge(hourly_count,data.frame("Var1" = 0:23), by = 'Var1', all.y = TRUE)
  hourly_count$Freq[is.na(hourly_count$Freq)] <- 0
  hourly_count$Visit <- grouped_df[[i]]$VISIT[1]
  
  cough_counts <- rbind(cough_counts, hourly_count)
}

cough_counts

# #Converting the hour data to integer
# hourly_counts$Var1 <- as.integer(as.character(hourly_counts$Var1))

# #Filling Missing Values
# hourly_counts <- merge(hourly_counts,data.frame("Var1" = 0:23), by = 'Var1', all.y = TRUE)
# 
# hourly_counts$Freq[is.na(hourly_counts$Freq)] <- 0

# 
# #Adding a new column to track sleep and keeping a column to indicate sleep
# 
# hourly_counts$sleep <- ifelse((sleep_time > wake_time & (hourly_counts$Var1 >= sleep_time | hourly_counts$Var1 <= wake_time)) | 
#                                 (sleep_time <= wake_time & (hourly_counts$Var1 >= sleep_time & hourly_counts$Var1 <= wake_time)), TRUE, FALSE)
# 
# 
# #Simple rearranging according to the start of recording time
# hourly_counts <- hourly_counts[order((hourly_counts$Var1 - start_time) %% nrow(hourly_counts)),]
# row.names(hourly_counts) <- 1:nrow(hourly_counts)


# # Load the required library
library(ggplot2)
# 
# # # Create a line plot using ggplot2 with different sleep and wake colors
# # 
# ggplot(hourly_counts, aes(x = factor(Var1, levels = unique(Var1)), y = Freq, color = sleep, group=1)) +
#   geom_line() +
#   geom_point() +
#   scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
#   labs(x = "Hours", y = "Frequency of Cough") +
#   theme_minimal()


#Horizontal line for time of the day (removed for now as we decided to use vertical line instead)
# +
# geom_segment(aes(x = 0, y = 40, xend = 7, yend = 40), linetype = "dashed", color = "red") +
# geom_segment(aes(x = 8, y = 40, xend = 17, yend = 40), linetype = "dashed", color = "green") +
# geom_segment(aes(x = 18, y = 40, xend = 20, yend = 40), linetype = "dashed", color = "purple") +
# geom_segment(aes(x = 21, y = 40, xend = 24, yend = 40), linetype = "dashed", color = "red") 
# 



# 
# ggplot(data = NULL, aes(x = hourly_counts$Var1, y = hourly_counts$Freq)) +
#   geom_point(color = "red") + geom_line(color = "blue") +
#   labs(x = "Hour", y = "Count", title = "Count of 'cough' by Hour") +
#   ylim(0, 40)



#plotting all the visits in same graph
p <- ggplot(cough_counts, aes(x = Var1, y = Freq, group = Visit, color = Visit)) +
  geom_line() +
  geom_point()+
  theme_minimal()

hoverable <- ggplotly(p)
hoverable



