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
