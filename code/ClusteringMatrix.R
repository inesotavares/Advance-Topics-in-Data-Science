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


variables <- names(data)
classes <- sapply(data, class)
print(classes)



# Preprocessing and feature engineering
features <- data %>%
  mutate(
    date = as.Date(created_at),
    hour = hour(created_at),
    day_of_week = wday(created_at, week_start = 1),  # Ensure consistency in week start (Monday = 1)
    is_weekend = ifelse(day_of_week %in% c(6, 7), 1, 0),  # Weekend days as Saturday (6) and Sunday (7)
    text_length = nchar(text),
    has_media = ifelse(!is.na(media_type) & media_type != "", 1, 0)  # Check for non-empty media_type entries
  ) %>%
  group_by(id) %>%
  summarise(
    avg_post_length = mean(text_length, na.rm = TRUE),
    peak_post_hour = which.max(table(hour)),  # Most common hour for posting
    avg_posts_per_day = n() / length(unique(date)),
    weekday_vs_weekend_ratio = sum(is_weekend == 0) / sum(is_weekend == 1),
    avg_engagement_per_post = mean(favorite_count + retweet_count + reply_count, na.rm = TRUE),
    hashtag_use_frequency = mean(str_detect(text, "#")),
    media_use_rate = mean(has_media),
    evening_post_proportion = mean(hour >= 18 & hour <= 23),  # Posts between 6 PM and 11 PM
    .groups = 'drop'
  )

View(features)

# Scaling: See if this is needed. Because after scaling, some columns have NaN values. Check why.

install.packages("scale")
library(scale)


# Standardizing features
features_scaled <- scale(features[, -1])  # Exclude 'id' from scaling

View(features_scaled)
