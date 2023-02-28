# Name: Nghia Nguyen

rm(list = ls())

install.packages(c('ggplot2', 'dplyr', 'corrplot', 'infotheo', 'scales', 'shinythemes', 'shiny'))

library(ggplot2)
library(dplyr)
library(corrplot)
library(infotheo)
library(scales)
library(shinythemes)
library(shiny)


# ------------------------------------------------------------------------------
# The objective of this assignment is to gain a comprehensive understanding of the provided dataset through data analysis and visualization. To accomplish this, the following steps will be taken:
#   
#   1.Load the dataset into Tableau and perform initial analysis to gain a general understanding of the data. This step will involve exploring the attributes and relationships between columns to gain insights into the data.
# 
#   2.Load the dataset into R and utilize visualization techniques to further explore and gain insights into the data. This step will involve creating plots and charts to represent the data and highlight any patterns or trends.
# 
# The combination of these two steps will provide a thorough and comprehensive understanding of the dataset, which will serve as a foundation for future analysis and decision making.

# ------------------------------------------------------------------------------

  

## Step 1: Load and view dataset
# Load dataset
data = read.csv("dataset_for_analyst_assignment_20201120.csv", head = TRUE)

# Check the structure of data
str(data)  

## Step 2: Summarize the dataset
# Summary the data
summary(data)

## Step 3: Visualize the Data


# Check which countries using Wolt services and preferred device at the same time
# Results: DNK, FIN and GRC are top 3 countries using Wolt services
plt1 = ggplot(data, aes(x = REGISTRATION_COUNTRY, fill = factor(PREFERRED_DEVICE))) +
  geom_bar(position = "stack")

# Filter the data to include only FIN, DNK, and GRC
data_filtered = data[data$REGISTRATION_COUNTRY %in% c("FIN", "DNK", "GRC"),]
data_filtered_sub <- subset(data_filtered, PREFERRED_DEVICE %in% c("android", "ios", "web"))


# Create the plot with filtered data and custom x-axis labels
plt2 = ggplot(data_filtered_sub, aes(x = REGISTRATION_COUNTRY, fill = factor(PREFERRED_DEVICE))) +
  geom_bar(position = "stack") +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5)) +
  scale_x_discrete(labels = c("FIN", "DNK", "GRC"))


## In this case, I will focus on analyzing Finland only
data_filtered_1 = data[data$REGISTRATION_COUNTRY %in% c("FIN"),]
data_filtered_sub_1 <- subset(data_filtered_1, PREFERRED_DEVICE %in% c("android", "ios", "web"))


# Daily Hour Analysis for Deliveries
# Results: The figure below presents the number of deliveries made during different hours of the day. 
# The graph indicates that the peak delivery hours are between 0:00 - 2:45 AM, 6:30 - 8:30 AM, 9:30 - 10:30 AM, 5:30 - 6:45 PM, and 10:00 - 11:00 PM."

# Create the plot with lines and faceting

plt3 = ggplot(data_filtered_sub_1, aes(x = MOST_COMMON_HOUR_OF_THE_DAY_TO_PURCHASE, y = PURCHASE_COUNT_DELIVERY, color = REGISTRATION_COUNTRY, linetype = PREFERRED_DEVICE)) +
  stat_summary(aes(group = interaction(REGISTRATION_COUNTRY, PREFERRED_DEVICE)), fun.y = "sum", geom = "line") +
  stat_summary(aes(group = interaction(REGISTRATION_COUNTRY, PREFERRED_DEVICE)), fun.y = "sum", geom = "point") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(title = "Daily Hour Analysis for Deliveries",
       x = "Day of the Week",
       y = "Number of Deliveries Orders",
       color = "Registration Country",
       linetype = "Preferred Device") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "bottom") 


# Daily Hour Analysis for Takeaway
# Results: The figure below presents the number of deliveries made during different hours of the day. 
# The graph indicates that the peak delivery hours are between 3:30 - 4:30 AM, 11:00 - 13:30, 16:30 - 17:30, 18:30 - 23. There will be a quiet time at 20:00


plt4 = ggplot(data_filtered_sub_1, aes(x = MOST_COMMON_HOUR_OF_THE_DAY_TO_PURCHASE, y = PURCHASE_COUNT_TAKEAWAY, color = REGISTRATION_COUNTRY)) +
  stat_summary(aes(group = REGISTRATION_COUNTRY), fun.y = "sum", geom = "line") +
  stat_summary(aes(group = REGISTRATION_COUNTRY), fun.y = "sum", geom = "point") +
  labs(title = "Daily Hour Analysis for Takeaway",
       x = "Hour of the Day",
       y = "Number of Deliveries Orders",
       color = "Registration Country") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "bottom") 

plt5 = ggplot(data_filtered_sub_1, aes(x = MOST_COMMON_HOUR_OF_THE_DAY_TO_PURCHASE, y = PURCHASE_COUNT_TAKEAWAY, color = REGISTRATION_COUNTRY)) +
  stat_summary(aes(group = REGISTRATION_COUNTRY), fun.y = "sum", geom = "line") +
  stat_summary(aes(group = REGISTRATION_COUNTRY), fun.y = "sum", geom = "point") +
  facet_wrap(~PREFERRED_DEVICE) +
  labs(title = "Daily Hour Analysis for Takeaway",
       x = "Hour of the Day",
       y = "Number of Deliveries",
       color = "Registration Country") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "bottom") 


# Activity churn
# Results: Based on the graph, it appears that during every day in September, the number of customers who are categorized as "lost" is higher than the number of customers who are categorized as "regular" or "loyal".

data_filtered_sub_1$ï..REGISTRATION_DATE <- format(as.Date(data_filtered_sub_1$ï..REGISTRATION_DATE), "%d")

# Create a new categorical variable based on "AVG_DAYS_BETWEEN_PURCHASES"
data_filtered_sub_1$customer_type <- cut(data_filtered_sub_1$AVG_DAYS_BETWEEN_PURCHASES, 
                                         breaks = c(-Inf, 3, 7, Inf), 
                                         labels = c("Loyal", "Regular", "Lost_Customer"))

plt6 = ggplot(data_filtered_sub_1, aes(x = ï..REGISTRATION_DATE, y = TOTAL_PURCHASES_EUR, fill = customer_type)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Activity Churn", x = "Registration Date", y = "Total Purchases EUR by Customer Type")



