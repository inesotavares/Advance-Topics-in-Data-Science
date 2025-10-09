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
library(syuzhet)
library(purrr)
library(reshape2)
library(viridis)
        
# Set Working Directory and Load Data:

# Set working directory
setwd("C:/Users/farya/OneDrive/Desktop/UoP/DM2/Project")

# Load data
data <- read.csv("HEIs.csv")
attach(data)

# Remove rows where 'id' is 'complutense.csv' as this seems like a wrong value with just one row count
data <- data %>%
  filter(id != "complutense.csv")

# Applying log transformations and handling NA values to avoid errors in log(0)
data <- data %>%
  mutate(
    log_view_count = ifelse(is.na(view_count), NA, log1p(view_count)),
    log_bookmark_count = ifelse(is.na(bookmark_count), NA, log1p(bookmark_count)),
    log_favorite_count = ifelse(is.na(favorite_count), NA, log1p(favorite_count)),
    log_retweet_count = ifelse(is.na(retweet_count), NA, log1p(retweet_count)),
    log_reply_count = ifelse(is.na(reply_count), NA, log1p(reply_count))
  )

# Apply KNN imputation only for view_count using log-transformed predictors
data <- kNN(data, variable = "log_view_count", k = 5, imp_var = FALSE)

# Transform the imputed log_view_count back to its original scale and update the original view_count
data <- data %>%
  mutate(
    view_count = ifelse(is.na(view_count), expm1(log_view_count), view_count)
  )

# Now, remove all the temporary log columns, keeping only the original columns
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

# View the updated dataset in RStudio
View(data)

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


# 8. Sentiment Analysis

# Converting date-time to POSIXct format for easier manipulation
data$created_at <- as.POSIXct(data$created_at, format = "%Y-%m-%d %H:%M:%S")

# Cleaning text data
data$text <- tolower(data$text)
data$text <- gsub("[^[:alnum:][:space:]]", "", data$text)

# 8.1. Overall Sentiment Analysis Over Time

# What are the general sentiment trends in the communication of each HEI?
# We will see the avg sentiment of posts per HEI over time. 
# This can show trends in sentiment (positive, neutral, negative) across different periods, 
# such as academic terms or in response to specific events like Covid.


# Calculating sentiment for each post
data$sentiment_score <- sapply(data$text, function(x) get_sentiment(x, method = "syuzhet"))

# Avg sentiment by day
daily_sentiment <- data %>%
  group_by(date = as.Date(created_at)) %>%
  summarise(average_sentiment = mean(sentiment_score, na.rm = TRUE))

# Monthly aggregation
monthly_sentiment <- data %>%
  group_by(year_month = format(created_at, "%Y-%m")) %>%
  summarise(average_sentiment = mean(sentiment_score, na.rm = TRUE))

# Plotting daily sentiment
ggplot(daily_sentiment, aes(x = date, y = average_sentiment)) +
  geom_line() +
  labs(title = "Daily Sentiment Trend", x = "Date", y = "Average Sentiment") +
  theme_minimal()

# Plotting monthly sentiment
ggplot(monthly_sentiment, aes(x = year_month, y = average_sentiment)) +
  geom_point(size = 3, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  labs(title = "Monthly Sentiment Trend", x = "Month", y = "Average Sentiment") +
  theme_minimal()


print(monthly_sentiment)

# Emotions for each post

# Assuming 'data$text' contains the text entries
data$emotions_list <- lapply(data$text, function(x) get_nrc_sentiment(x))

# Combine the list of data frames into one large data frame
emotions_df <- bind_rows(data$emotions_list)

View(emotions_df)

# Visualizing the distribution of each emotion
emotion_sums <- colSums(do.call(rbind, data$emotions))
barplot(sort(emotion_sums), horiz = TRUE, las = 1, main = "Distribution of Emotions", xlab = "Frequency")

# 8.2. Emotion Analysis

# Which emotions are most prevalent in the social media communications of HEIs?
# We will visualize different emotions detected (like joy, trust, anticipation, etc.) in the posts from each HEI. 
# Try to understand the predominant emotions associated with each institution.
# Also see the intensity of different emotions over time or correlated with specific events or announcements.

emotions_df$hei <- data$id  

# Emotions by HEI
emotion_summary <- emotions_df %>%
  group_by(hei) %>%
  summarise(across(everything(), sum, .names = "sum_{.col}"))

# Calculating percentages
emotion_summary <- emotion_summary %>%
  mutate(across(starts_with("sum_"), ~ ./sum(.)))

# Melting data for easier plotting with ggplot2
emotion_melted <- melt(emotion_summary, id.vars = "hei")

# Plotting
ggplot(emotion_melted, aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~hei, scales = "free") +  
  labs(title = "Distribution of Emotions in HEIs", x = "Emotion", y = "Frequency") +
  theme_minimal()


# 8.3. Time Series Analysis
# As learnt in class, we will employ narrative time series analysis, such as the "Syuzhet Plot", 
# to analyze the progression of emotional valence over the narrative arc of content posted by each HEI,
# reflecting on how sentiment evolves in institutional communication.


# Plotting time series for each HEI
ggplot(dataf, aes(x = narrative_order, y = sentiment_score)) +
  geom_line() +  # Connect points with a line
  facet_wrap(~ id, scales = "free_x") +  # Create a separate plot for each HEI
  labs(title = "Time Series of Sentiment by HEI",
       x = "Order",
       y = "Sentiment Score") +
  theme_minimal()  # Using a clean minimalistic theme

# 8.4. Sentiment and Emotion Correlation with Engagement Metrics
# How do engagement metrics interact with different sentiments and emotions? 
# Does more positive sentiment correlate with higher engagement?
# We will analyze how different sentiments or emotions correlate with engagement metrics 
# like retweets, favourites, and replies. 


# Scatter plot for sentiment scores vs favorites
ggplot(data, aes(x = sentiment_score, y = favorite_count)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +  # Adds a regression line
  labs(title = "Correlation Between Sentiment and Favourite Count",
       x = "Sentiment Score",
       y = "Favorite Count") +
  theme_minimal()

# Scatter plot for sentiment scores vs retweets
ggplot(data, aes(x = sentiment_score, y = retweet_count)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +  # Adds a regression line
  labs(title = "Correlation Between Sentiment and Retweet Count",
       x = "Sentiment Score",
       y = "Retweet Count") +
  theme_minimal()

# Scatter plot for sentiment scores vs retweets
ggplot(data, aes(x = sentiment_score, y = reply_count)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +  # Adds a regression line
  labs(title = "Correlation Between Sentiment and Reply Count",
       x = "Sentiment Score",
       y = "Reply Count") +
  theme_minimal()


# emotions vs engagement

dataE <- data %>%
  mutate(
    # Extracting individual emotions from the list
    anger = map_dbl(emotions_list, ~ .x$anger),
    anticipation = map_dbl(emotions_list, ~ .x$anticipation),
    disgust = map_dbl(emotions_list, ~ .x$disgust),
    fear = map_dbl(emotions_list, ~ .x$fear),
    joy = map_dbl(emotions_list, ~ .x$joy),
    sadness = map_dbl(emotions_list, ~ .x$sadness),
    surprise = map_dbl(emotions_list, ~ .x$surprise),
    trust = map_dbl(emotions_list, ~ .x$trust),
    negative = map_dbl(emotions_list, ~ .x$negative),
    positive = map_dbl(emotions_list, ~ .x$positive),
    # Calculating total engagement
    total_engagement = favorite_count + retweet_count + reply_count
  ) %>%
  select(-emotions_list)
  
View(dataE)

# Creating a list of emotion names to iterate over
emotions <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive")

# Using lapply to create plots for each emotion
plot_list <- lapply(emotions, function(emotion) {
  ggplot(dataE, aes_string(x = emotion, y = "total_engagement")) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "blue") +
    labs(title = paste(emotion, "vs Total Engagement"), x = emotion, y = "Total Engagement") +
    theme_minimal()
})


plot_list
