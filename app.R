# Assigning the right directory
setwd("/Users/zjpg/Documents/Sem-3/DataVisualization/Assignemnet 3")

# Load libraries
library(ggplot2)
library(dplyr)
library(shiny)
library(shinydashboard)
library(rsconnect)

# Load data into dataframe
data = read.csv("dataset_Facebook.csv", sep = ";")
dim(data)

# Get the column names
column_names <- colnames(data)

# Print the column names
print(column_names)

# Find missing values
missing_values <- data %>% summarise_all(~sum(is.na(.)))
missing_values

# Remove rows containing missing values
data_clean <- na.omit(data)

# Identifying key Metrics

#1 Visualize month on month increase in the total likes of the page
# Use group_by function
data_total_likes <- data_clean %>%
  group_by(Post.Month) %>%
  summarize(TotalValue = max(Page.total.likes))

data_total_likes <- data_total_likes %>%
  mutate(Post.Month = case_when(
    Post.Month == 1 ~ "Jan",
    Post.Month == 2 ~ "Feb",
    Post.Month == 3 ~ "Mar",
    Post.Month == 4 ~ "Apr",
    Post.Month == 5 ~ "May",
    Post.Month == 6 ~ "Jun",
    Post.Month == 7 ~ "Jul",
    Post.Month == 8 ~ "Aug",
    Post.Month == 9 ~ "Sep",
    Post.Month == 10 ~ "Oct",
    Post.Month == 11 ~ "Nov",
    Post.Month == 12 ~ "Dec",
  ))
# Making the total value in 10K
data_total_likes$TotalValue <- data_total_likes$TotalValue/10000

# Set the Month column as an ordered factor
data_total_likes$Post.Month <- factor(data_total_likes$Post.Month, levels = data_total_likes$Post.Month)

# Plotting bar graph
page_likes_barplot <- ggplot(data = data_total_likes, aes(x = Post.Month, y = TotalValue, fill = Post.Month)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", TotalValue)), vjust = -0.5, family="Arial",
            size = 4, fontface = "bold") +
  labs(x = "Months", y = "Page Total Likes (in 10K)", title = "Growth in Followers") +
  scale_y_continuous(breaks = seq(0, 14, 2)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none") +
  scale_fill_manual(values = c("Jan"="#ADD8E6","Feb"="#87CEEB", "Mar"="#1E90FF", "Apr"="#4169E1",
                               "May"="#0000CD", "Jun"="#0000CD", "Jul"="#00008B", "Aug"="#00008B",
                               "Sep"="#00008B", "Oct"="#00008B", "Nov"="#00008B", "Dec"="#00008B"))
#print(page_likes_barplot)

#2.1 Visualize which kind of post receives highest engagement
# Use group_by function
data_post_engagement <- data_clean %>%
  group_by(Type) %>%
  summarize(TotalValue = sum(Total.Interactions))

# Making the total value in 1K
data_post_engagement$TotalValue <- data_post_engagement$TotalValue/1000

# Plotting bar graph
post_Eng_Plot <- ggplot(data = data_post_engagement, aes(x = Type, y = TotalValue, fill = Type)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", TotalValue)), vjust = -0.5, family="Arial",
            size = 4, fontface = "bold") +
  labs(x = "Type of Post", y = "Total Interactions (in 1000s)", title = "Type Of Post v/s Interactions") +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none") +
  scale_fill_manual(values = c("Link"="#ADD8E6", "Video"="#1E90FF", "Status"="#4169E1",
                               "Photo"="#0000CD"))

#2.2 Visualize which category of post receives highest engagement
# Use group_by function
data_category_engagement <- data_clean %>%
  group_by(Category) %>%
  summarize(TotalInteractions = sum(Total.Interactions))

# Making the total value in 10K
data_category_engagement$TotalInteractions <- data_category_engagement$TotalInteractions/10000

data_category_engagement <- data_category_engagement %>%
  mutate(Category = case_when(
    Category == 1 ~ "Offers",
    Category == 2 ~ "Brand",
    Category == 3 ~ "Non-Brand",
  ))

# Plotting bar graph
Category_Interactions <- ggplot(data = data_category_engagement, aes(x = Category, y = TotalInteractions, fill = Category)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", TotalInteractions)), vjust = -0.5, family="Arial",
            size = 4, fontface = "bold") +
  labs(x = "Category of the Post", y = "Total Interactions (in 10K)", title = "Content of the Post v/s Interactions") +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none") +
  scale_fill_manual(values = c("Brand"="#1E90FF", "Offers"="#ADD8E6",
                               "Non-Brand"="#0000CD"))

#3 Total reach & impressions with respect to each month
# Use group_by function
data_total_reach_impressions <- data_clean %>%
  group_by(Post.Month) %>%
  summarize(TotalReach = sum(Lifetime.Post.Total.Reach),
            TotalImpressions = sum(Lifetime.Post.Total.Impressions))
# Change the name of each month (ex; 1 = Jan)
data_total_reach_impressions <- data_total_reach_impressions %>%
  mutate(Post.Month = case_when(
    Post.Month == 1 ~ "Jan",
    Post.Month == 2 ~ "Feb",
    Post.Month == 3 ~ "Mar",
    Post.Month == 4 ~ "Apr",
    Post.Month == 5 ~ "May",
    Post.Month == 6 ~ "Jun",
    Post.Month == 7 ~ "Jul",
    Post.Month == 8 ~ "Aug",
    Post.Month == 9 ~ "Sep",
    Post.Month == 10 ~ "Oct",
    Post.Month == 11 ~ "Nov",
    Post.Month == 12 ~ "Dec",
  ))
# Making the total reach and impressions in 100K
data_total_reach_impressions$TotalReach <- data_total_reach_impressions$TotalReach/100000
data_total_reach_impressions$TotalImpressions <- data_total_reach_impressions$TotalImpressions/100000

# Set the Month column as an ordered factor
data_total_reach_impressions$Post.Month <- factor(data_total_reach_impressions$Post.Month, levels = data_total_reach_impressions$Post.Month)

# 3.1 total reach vs month
TotalReach_Month <- ggplot(data = data_total_reach_impressions, aes(x = Post.Month, y = TotalReach, fill = Post.Month)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", TotalReach)), vjust = -0.5, family="Arial",
            size = 4, fontface = "bold") +
  labs(x = "Months", y = "Unique Users (in 100K)", title = "Variations In Post Reaching New Users") +
  scale_y_continuous(breaks = seq(0, 14, 2)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none") +
  scale_fill_manual(values = c("Jan"="#87CEEB","Feb"="#4169E1", "Mar"="#1E90FF", "Apr"="#00008B",
                               "May"="#1E90FF", "Jun"="#00008B", "Jul"="#00008B", "Aug"="#1E90FF",
                               "Sep"="#ADD8E6", "Oct"="#4169E1", "Nov"="#4169E1", "Dec"="#1E90FF"))

# 3.2 total impressions vs month
TotalImpressions_Month <- ggplot(data = data_total_reach_impressions, aes(x = Post.Month, y = TotalImpressions, fill = Post.Month)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", TotalImpressions)), vjust = -0.5, family="Arial",
            size = 4, fontface = "bold") +
  labs(x = "Months", y = "Total Impressions (in 100K)", title = "Variations In Total Impressions") +
  scale_y_continuous(breaks = seq(0, 14, 2)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none") +
  scale_fill_manual(values = c("Jan"="#ADD8E6","Feb"="#00008B", "Mar"="#00008B", "Apr"="#4169E1",
                               "May"="#87CEEB", "Jun"="#4169E1", "Jul"="#4169E1", "Aug"="#1E90FF",
                               "Sep"="#87CEEB", "Oct"="#4169E1", "Nov"="#4169E1", "Dec"="#1E90FF"))


## app.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Main Tab", icon = icon("th"), tabName = "Main Tab",
             badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  fluidRow(
    column(width = 4,
           box(
             title = "Popularity of Page",width = NULL, status = "warning", solidHeader = TRUE,
             plotOutput("plot1")
           ),
           box(
             title = "Data Overview",width = NULL, background = "light-blue",
             "The information relates to posts made in 2014 on a well-known cosmetics company's Facebook page. This dashboard analyses various Facebook page-related metrics to provide a quick insight into how well it is performing."
           ),
           box(
             title = "Data Reference",width = NULL, background = "light-blue",
             "Moro, S., Rita, P., & Vala, B. (2016). Predicting social media performance metrics and evaluation of the impact on brand building: A data mining approach. ScienceDirect, 69(9), 3341–3351. Retrieved from https://doi.org/10.1016/j.jbusres.2016.02.010."
           )
    ),
    column(width = 4,
           box(
             title = "Impact of Posts Developing Interactions",status = "warning", width = NULL, solidHeader = TRUE,
             plotOutput("plot2")
           ),
           box(
             width = NULL, solidHeader = TRUE,
             plotOutput("plot3")
           )
    ),
    column(width = 4,
           box(
             title = "Impact of Posts Reaching Out", width = NULL, status = "warning", solidHeader = TRUE,
             plotOutput("plot4")
           ),
           box(
             width = NULL, solidHeader = TRUE,
             plotOutput("plot5")
           )
    )
  )
)

# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  dashboardHeader(title = "Analysis of FB Page"),
  sidebar,
  body
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    page_likes_barplot  # Render the first plot within the plotOutput
  })
  
  output$plot2 <- renderPlot({
    post_Eng_Plot  # Render the second plot within the plotOutput
  })
  output$plot3 <- renderPlot({
    Category_Interactions  # Render the second plot within the plotOutput
  })
  
  output$plot4 <- renderPlot({
    TotalReach_Month  # Render the first plot within the plotOutput
  })
  output$plot5 <- renderPlot({
    TotalImpressions_Month  # Render the second plot within the plotOutput
  })
}


# Preview the UI in the console
shinyApp(ui = ui, server = server)
