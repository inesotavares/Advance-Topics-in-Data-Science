
#Required Libraries

# Necessary packages
install.packages("dlookr")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("stringr")
install.packages("reshape2")
install.packages("dplyr")
install.packages("DataExplorer")
install.packages('DescTools')
install.packages('DMwR')
install.packages('VIM')

install.packages('textrank')
install.packages('seededlda')

if (!require("tidyverse")) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")


# Loading libraries
library(dlookr)
library(ggplot2)
library(ggpubr)
library(stringr)
library(reshape2)
library(dplyr)
library(DataExplorer)
library(tidyverse)
library(corrplot)
library(DescTools)
library(lubridate)
library(VIM)  
library(plotly)

library(arules)
library(arulesViz)
library(tm)
library (text2vec)
library(wordcloud)
library(tm)
library(textstem)
library(textrank)
library(seededlda)
library(quanteda)

# Set Working Directory and Load Data:

# Set working directory
setwd("C:/Users/farya/OneDrive/Desktop/UoP/DM2/Project")

# Load data
data <- read.csv("HEIs.csv")
attach(data)

# Removing Wrong Values:

# Remove rows where 'id' is 'complutense.csv' as this seems like a wrong value with just one row count
data <- data %>%
  filter(id != "complutense.csv")

# Handling Missing Values:

# Applying log transformations and handling NA values to avoid errors in log(0)
data <- data %>%
  mutate(
    log_view_count = ifelse(is.na(view_count), NA, log1p(view_count)),
    log_bookmark_count = ifelse(is.na(bookmark_count), NA, log1p(bookmark_count)),
    log_favorite_count = ifelse(is.na(favorite_count), NA, log1p(favorite_count)),
    log_retweet_count = ifelse(is.na(retweet_count), NA, log1p(retweet_count)),
    log_reply_count = ifelse(is.na(reply_count), NA, log1p(reply_count))
  )

# Applying KNN imputation only for view_count using log-transformed predictors
data <- kNN(data, variable = "log_view_count", k = 5, imp_var = FALSE)

# Transforming the imputed log_view_count back to its original scale and update the original view_count
data <- data %>%
  mutate(
    view_count = ifelse(is.na(view_count), expm1(log_view_count), view_count)
  )

# Now, removing all the temporary log columns, keeping only the original columns
data <- data %>%
  select(
    id, tweet_id, text, type,
    bookmark_count, favorite_count, retweet_count, reply_count,
    view_count, created_at, hashtags, urls, media_type, media_urls
  )

# Recounting missing values in each column to validate if the imputation worked correctly
missing_counts <- data %>%
  summarise_all(~sum(is.na(.)))

# Printing the counts of missing values
print(missing_counts)


View(data)

# Features Extraction and Engineering:

# Parsing the 'created_at' column from a character type to a POSIXct date-time type
data <- data %>%
  mutate(
    # Replacing 'T' with a space for correct parsing
    # Example: "2023-08-31T20:50:01+0100" becomes "2023-08-31 20:50:01+0100"
    created_at = ymd_hms(sub("(.*)T", "\\1 ", created_at))  
    # The sub function is used here to format the string by replacing 'T' with a space (' ')
    # The ymd_hms function from lubridate parses the string into a POSIXct date-time object
  )

# Extracting new features: hour of the day and day of the week as we will need this for temporal analysis

data <- data %>%
  mutate(
    hour_of_day = hour(created_at),  # Extracts the hour component from 'created_at'
    # The hour function retrieves the hour part of the date-time, useful for analyzing posting time distributions
    
    day_of_week = wday(created_at, label = TRUE, abbr = FALSE)  # Extracts the weekday, with full names
    # The wday function retrieves the day of the week from 'created_at'
    # 'label = TRUE' makes the function return the day name (e.g., Monday, Tuesday)
    # 'abbr = FALSE' specifies that the full names of the days are returned, not abbreviations
  )

data <- data %>%
  mutate(
    year = year(created_at),
    month = month(created_at, label = TRUE, abbr = TRUE),  # 'abbr = TRUE' for abbreviated month names
    year_month = paste(year, formatC(month(created_at), width = 2, flag = "0"))  # Combining year and month for trend analysis
  )

# Verifying the new columns
head(data)
View(data)

# 4. Data Visualizations:

# making a temporary copy
dataF<-data

# 4.1. Temporal Analysis:

# 4.1.1. Posting Frequency Analysis:

# 4.1.1.1. Overall Posting Frequency:
# This is the total number of posts by each institution over the dataset's timeframe 
# to identify the most and least active institutions.

# Aggregating data to count total posts by each institution
institution_post_counts <- dataF %>%
  group_by(id) %>%
  summarise(total_posts = n())  # Counting the number of rows for each group, which corresponds to the number of posts

# Creating a bar chart to visualize posting frequency by institution
ggplot(institution_post_counts, aes(x = id, y = total_posts, fill = id)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Number of Posts by Institution",
       x = "Institution",
       y = "Total Number of Posts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels for better readability

# Saving the plot to a file
#ggsave("institution_post_frequency.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

# 4.1.1.2.Frequency Analysis:

# Hourly/Daily Frequency
# Checking how frequently each institution posts per hour and per day 
# to assess their operational activity hours or peak posting times.

# Hourly frequency of posts by institution
hourly_frequency <- dataF %>%
  group_by(id, hour_of_day) %>%
  summarise(posts_per_hour = n(), .groups = 'drop')

# Daily frequency of posts by institution
daily_frequency <- dataF %>%
  group_by(id, day_of_week) %>%
  summarise(posts_per_day = n(), .groups = 'drop')

# Hourly Posting Frequency
ggplot(hourly_frequency, aes(x = hour_of_day, y = posts_per_hour, group = id, color = id)) +
  geom_line() +
  labs(title = "Hourly Posting Frequency by Institution",
       x = "Hour of Day",
       y = "Number of Posts") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 23, 1))  # Ensuring all hours are shown


# Daily Posting Frequency
ggplot(daily_frequency, aes(x = day_of_week, y = posts_per_day, fill = id)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Daily Posting Frequency by Institution",
       x = "Day of Week",
       y = "Number of Posts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improving label readability


# Converting the ggplot to an interactive plotly object
hourly_plot <- ggplotly(
  ggplot(hourly_frequency, aes(x = hour_of_day, y = posts_per_hour, group = id, color = id)) +
    geom_line() +
    labs(title = "Hourly Posting Frequency by Institution",
         x = "Hour of Day",
         y = "Number of Posts")
)

# Displaying the interactive plot
hourly_plot

# Monthly/yearly Trends:
# Tyring to analyze trends to detect any growth in activity or shifts in posting patterns

monthly_yearly_trends <- dataF %>%
  group_by(id, year, month, year_month) %>%
  summarise(posts_per_month = n(), .groups = 'drop')


# Monthly Trends per Year
# Creating ggplot object first
monthly_trend_plot <- ggplot(monthly_yearly_trends, aes(x = year_month, y = posts_per_month, group = id, color = id)) +
  geom_line() +
  facet_wrap(~year, scales = "free_x") +
  labs(title = "Monthly Posting Frequency by Year and Institution",
       x = "Month",
       y = "Number of Posts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotating month labels for better readability

# Converting ggplot object to plotly object for interactivity
interactive_monthly_trend_plot <- ggplotly(monthly_trend_plot)

# Displaying 
interactive_monthly_trend_plot

# Yearly Trend

yearly_trends <- dataF %>%
  group_by(id, year) %>%
  summarise(posts_per_year = n(), .groups = 'drop')

yearly_trend_plot <- ggplot(yearly_trends, aes(x = year, y = posts_per_year, fill = id)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Yearly Posting Frequency by Institution",
       x = "Year",
       y = "Number of Posts") +
  theme_minimal()


interactive_yearly_trend_plot <- ggplotly(yearly_trend_plot)


interactive_yearly_trend_plot

# 4.2. Content Analysis:

# 4.2.1. Type of Posts:
# We will analyze the proportion of original tweets, retweets, and replies 
# to gauge each institution's approach to content generation 
# versus community engagement.

# Calculating the count of each post type for each institution
type_counts <- dataF %>%
  group_by(id, type) %>%
  summarise(count = n(), .groups = 'drop') %>%
  spread(key = type, value = count, fill = 0)  # Spreading the types into separate columns for easy plotting

# Calculating total posts per institution for normalization
type_counts <- type_counts %>%
  mutate(total = `Tweet` + `Retweet` + `Reply`) %>%
  mutate_at(vars(Tweet, Retweet, Reply), funs(./total))  # Normalizing each type by total posts

# Plotting
plot_ly(type_counts, x = ~id, y = ~Tweet, type = 'bar', name = 'Tweets', marker = list(color = 'blue')) %>%
  add_trace(y = ~Retweet, name = 'Retweets', marker = list(color = 'red')) %>%
  add_trace(y = ~Reply, name = 'Replies', marker = list(color = 'green')) %>%
  layout(yaxis = list(title = 'Proportion of Post Types', tickformat = ",.0%"),
         barmode = 'stack', 
         title = "Proportion of Post Types by Institution",
         xaxis = list(title = "Institution"))

# 4.2.2. Size of Posts:

# Calculating average text length directly within the summarise function
dataF_summary <- dataF %>%
  group_by(id, type) %>%
  summarise(average_length = mean(nchar(text), na.rm = TRUE), .groups = 'drop')  
# Avg length by institution and type

ggplot(dataF_summary, aes(x = id, y = average_length, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  labs(title = "Average Post Length by Institution and Type",
       x = "Institution",
       y = "Average Text Length (characters)",
       fill = "Post Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Calculating the maximum text length for each institution and post type
dataF_max_length <- dataF %>%
  group_by(id, type) %>%
  summarise(max_length = max(nchar(text), na.rm = TRUE), .groups = 'drop')  

# Interactive bar chart
plot_ly(dataF_max_length, x = ~id, y = ~max_length, type = 'bar', color = ~type,
        text = ~paste('Max Length:', max_length), hoverinfo = 'text+x+y') %>%
  layout(title = 'Maximum Post Length by Institution and Type',
         xaxis = list(title = 'Institution'),
         yaxis = list(title = 'Maximum Text Length (characters)'),
         barmode = 'group')  

# 4.2.3. Media Usage:
# Now, let's determine the correlation between media types used ( photo, video, animated_gif) 
# and engagement metrics to see which media form attracts more attention

# Summarizing engagement metrics by media type
media_engagement <- dataF %>%
  group_by(media_type) %>%
  summarise(
    total_favorites = sum(favorite_count, na.rm = TRUE),
    total_retweets = sum(retweet_count, na.rm = TRUE),
    total_replies = sum(reply_count, na.rm = TRUE),
    total_engagement = total_favorites + total_retweets + total_replies,
    .groups = 'drop'
  ) %>%
  filter(media_type %in% c("photo", "video", "animated_gif"))

summary(media_engagement)

# Creating a pie chart
media_pie_chart <- plot_ly(media_engagement, labels = ~media_type, values = ~total_engagement, type = 'pie',
                           textinfo = 'label+percent',
                           insidetextorientation = 'radial') %>%
  layout(title = 'Impact of Media Type on Engagement',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



print(media_pie_chart)

# 4.3. Engagement Analysis:

# 4.3.1. Analyzing Which Day and Time of the Day Has High Engagement:


# Calculating average engagement per day and hour
engagement_summary <- dataF %>%
  group_by(day_of_week, hour_of_day) %>%
  summarise(average_engagement = mean(favorite_count + retweet_count + reply_count, na.rm = TRUE),
            .groups = 'drop')



ggplot(engagement_summary, aes(x = hour_of_day, y = day_of_week, size = average_engagement)) +
  geom_point(alpha = 0.6) +
  scale_size_continuous(range = c(1, 15)) +
  labs(title = "Bubble Chart of Engagement by Day and Hour",
       x = "Hour of Day", y = "Day of the Week") +
  theme_minimal() +
  guides(size = guide_legend(title = "Average Engagement"))

ggplot(engagement_summary, aes(x = hour_of_day, y = average_engagement, fill = day_of_week)) +
  geom_area(position = 'stack') +
  labs(title = "Stacked Area Chart of Engagement by Day and Hour",
       x = "Hour of Day", y = "Cumulative Engagement") +
  theme_minimal()

# 4.3.2. Identifying and Visualizing High-Impact Posts per Institution:


# Calculating total engagement and determining high-impact posts per institution
dataF <- dataF %>%
  mutate(total_engagement = favorite_count + retweet_count + reply_count)

high_impact_thresholds <- dataF %>%
  group_by(id) %>%
  summarise(high_impact_threshold = quantile(total_engagement, 0.95), .groups = 'drop')

high_impact_posts <- dataF %>%
  left_join(high_impact_thresholds, by = "id") %>%
  filter(total_engagement > high_impact_threshold)

# Identifying regular posts by excluding high-impact posts
regular_posts <- dataF %>%
  left_join(high_impact_thresholds, by = "id") %>%
  filter(total_engagement <= high_impact_threshold)

# Scatter plot
interactive_plot <- plot_ly() %>%
  add_trace(data = regular_posts, x = ~id, y = ~total_engagement, 
            type = 'scatter', mode = 'markers',
            marker = list(size = 5, color = 'lightblue'),
            name = 'Regular Posts') %>%
  add_trace(data = high_impact_posts, x = ~id, y = ~total_engagement,
            mode = 'markers', marker = list(size = 10, color = 'red'),
            name = 'High-Impact Posts') %>%
  layout(title = 'Engagement by Institution',
         xaxis = list(title = 'Institution', type = 'category'),  # x-axis treats institutions as categories
         yaxis = list(title = 'Total Engagement'),
         hovermode = 'closest')


interactive_plot
