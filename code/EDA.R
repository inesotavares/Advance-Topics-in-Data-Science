if (!require("tidyverse")) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")

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
library(textstem)
library(textrank)
library(seededlda)
library(quanteda)




# Set Working Directory and Load Data:

# Set working directory
setwd("C:/Users/maria/OneDrive - Universidade do Porto/Faculdade/CD/TACD/GitHub/TACD-project")

# Load data
data <- read.csv("HEIs.csv")
attach(data)



##################################################################
########################### EDA ##################################
##################################################################

#
#
# 1. Preliminary Data Analysis:
#
#

# Printing dimensions and variable classes
print(dim(data)) # e.g., 11728 rows and 14 columns
variables <- names(data)
classes <- sapply(data, class)
print(classes)

# Generating a diagnostic report
diagnose_paged_report(data, output_format = 'html')

# Initial data visualization:
plot_intro(data)
plot_missing(data)
plot_correlation(data)
plot_density(data)
plot_histogram(data)
plot_qq(data)


#
#
# 2. Checking for Unclean Data:
#
#

# The following is only to understand the unclean data. 
# The data cleaning process will be completed later in Clean.R.

# Checking for Duplicate Rows and Values:

# We are only looking at complete duplicated rows i.e. rows with all columns having the same/duplicated values
# or rows that have the same tweet ID.
# Any other column may have same/duplicated values, which is highly possible in this dataset and does not affect the analysis.

# Identifying complete duplicates
complete_duplicates <- data[duplicated(data) | duplicated(data, fromLast = TRUE), ]
complete_dup_count <- nrow(complete_duplicates)

print(paste("Total complete duplicate rows:", complete_dup_count))


# Calculating the number of unique tweet_ids
unique_tweet_ids_count <- n_distinct(data$tweet_id)

print(paste("Number of unique tweet_ids:", unique_tweet_ids_count))

# Observation: There are no complete duplicated rows and no 
# duplicated tweet ids since unique count is 11728 (equal to total rows)

# Checking for Incorrect/Wrong Values:

# We need to make sure there are no incorrect or wrong values in the data.
# E.g The view count column cannot have a negative value.

# Checking for any negative counts in numeric columns where negatives don't make sense
negative_checks <- data %>%
  summarise(
    Negative_Bookmark_Count = sum(bookmark_count < 0),
    Negative_Favorite_Count = sum(favorite_count < 0),
    Negative_Retweet_Count = sum(retweet_count < 0),
    Negative_Reply_Count = sum(reply_count < 0),
    Negative_View_Count = sum(view_count < 0)
  )

print(negative_checks)

# Date validation for 'created_at' (assuming ISO 8601 format: yyyy-mm-dd)
data$created_at <- as.Date(data$created_at, format="%Y-%m-%d")
date_checks <- summarise(data, Invalid_Dates = sum(is.na(created_at)))

print(date_checks)

# Checking for non-numeric values in numeric columns (this should not happen if data types are declared correctly)
non_numeric_checks <- data %>%
  summarise(
    NonNumeric_Tweet_ID = sum(!is.numeric(tweet_id)),
    NonNumeric_Bookmark_Count = sum(!is.numeric(bookmark_count)),
    NonNumeric_Favorite_Count = sum(!is.numeric(favorite_count)),
    NonNumeric_Retweet_Count = sum(!is.numeric(retweet_count)),
    NonNumeric_Reply_Count = sum(!is.numeric(reply_count)),
    NonNumeric_View_Count = sum(!is.numeric(view_count))
  )

print(non_numeric_checks)

# Now, we need to check for any potential wrong values in the columns 'Id' and 'media type'
# We can do so by checking the distinct values of these two columns  
# and detecting any values that do not make sense for these two columns

# Unique values check for 'id' and 'media_type'
distinct_ids <- distinct(data, id)
distinct_media_types <- distinct(data, media_type)

# Printing distinct 'id' values 
print("Distinct IDs:")
print(distinct_ids)

# Count occurrences of each unique 'id'
id_counts <- data %>%
  count(id)

# Print the counts
print(id_counts)


# Print distinct 'media_type' values to check for any unusual entries
print("Distinct media types:")
print(distinct_media_types)

# Observation: There are no negative values for the columns related to counts
#  There are no non-numeric values in numeric columns      
#  And there are no wrong values in column 'media_type' 
#  as all distinct values are valid. 
#  There is one value of 'Id' which is 'complutense.csv' that only has one count
#  which means that it can be a wrong value, hence it will be removed.
# Hence, the data has no wrong/incorrect values.

# Detecting Outliers:

# In our case, we could have outliers for the columns related to different counts
# but these outliers are meaningful and must be kept to detect viral posts/trends 
# etc. during the analysis phase
# These may be removed later on when clustering, if needed.


# Function to calculate the percentage of outliers in a vector
# Using the IQR Method to detect outliers. 
# Values below Q1 - 1.5 * IQR or above Q3 + 1.5 * IQR are considered outliers.
calculate_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  sum(x < lower_bound | x > upper_bound, na.rm = TRUE) / length(na.omit(x))
}

# Applying the function to each numerical column
outlier_percentages <- data %>% select(bookmark_count, favorite_count, retweet_count, reply_count, view_count) %>% 
  summarise_all(calculate_outliers)

# Printing the percentage of outliers in each column
print(outlier_percentages)

# Visualizing Outliers
# Defining a vector of column names that we want to create boxplots for
count_columns <- c("bookmark_count", "favorite_count", "retweet_count", "reply_count", "view_count")

# Function to create and print a boxplot for each count column
plot_boxplots <- function(data, columns) {
  for (col in columns) {
    p <- ggplot(data, aes_string(y = col)) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", col), y = col, x = "") +
      theme_minimal()  # Adds a minimalistic theme to the plot
    
    print(p)  # Print the plot
  }
}


plot_boxplots(data, count_columns)


# Checking for Missing Values:

# Count missing values in each column
missing_counts <- data %>%
  summarise_all(~sum(is.na(.)))

# Print the counts of missing values
print(missing_counts)

# Result: view_count has 4839 missing values that need model-based imputation
# We will use KNN for imputation in the file Clean.R
# We will start with correlation analysis

# Reasoning: Imputing using KNN based on columns highly correlated with view_count will likely yield more accurate results 
# because KNN uses the nearest neighbors in the feature space to estimate missing values. 
# Columns that do not correlate well with view_count may introduce noise into the imputation process.

# Checking correlation:

numeric_data <- data %>% select_if(is.numeric)  # Select only numeric columns for correlation analysis

# Computing correlations
correlations <- cor(numeric_data, use = "complete.obs")  # Exclude NA values for correlation calculation

# Printing correlation of all columns with view_count
# Identifying the index or name 'view_count' correctly within the matrix
if("view_count" %in% colnames(numeric_data)) {
  view_count_correlations <- correlations[, "view_count"]
  print(view_count_correlations)  # Printing the correlations specifically for view_count
} else {
  print("view_count column not found in the data")
}

# Result: We can see that bookmark_count, favorite_count, retweet_count, & reply_count
#         are the columns/features that are highly correlated to view_count

# We will be handling outliers temporarily during data cleaning since KNN is affected by outliers


#
#
# 3. Post Type and Size Analysis
#
#

# Types of posts:

type_values <- unique(data$type)
type_values #possible types of a post

freq_types_post <- data %>%
  group_by(type) %>%
  summarise(counts = n())

print(freq_types_post$type)
print(freq_types_post$counts)

posts_types <- data.frame(type = freq_types_post$type, counts = freq_types_post$counts)

barplot(freq_types_post$counts,
        names.arg = freq_types_post$type,
        col = "#1b98e0",
        main = "Types of post",
        ylab = "Count")


#Sizes of posts
post_sizes <- nchar(data$text)
print(summary(post_sizes))
boxplot(post_sizes) 


#
#
# 3. Post Frequencies Analysis
#
#

# Frequency of the hour of the posts created

time_created_at <- str_split_i(data$created_at, "T", 2)
time_created_at <- str_split_i(time_created_at, "\\+", 1)
data$hour_created_at <- str_split_i(time_created_at, ":", 1)

freq_hour <- data %>%
  group_by(hour_created_at) %>%
  summarise(counts = n())
freq_hour

barplot(freq_hour$counts,                                
        names.arg = freq_hour$hour_created_at,
        col = "#1b98e0")



# 4. Frequency of the days of the week:

Sys.setlocale("LC_TIME", "en_US.UTF-8")
dates <- as.Date(strptime(data$created_at, format = "%Y-%m-%dT%H:%M:%S%z"))
# Extract the day of the week
data$days_of_week <- factor(format(dates, "%A"), levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# Calculate frequency of each day of the week
freq_days_week <- data %>%
  group_by(days_of_week) %>%
  summarise(counts = n())

barplot(freq_days_week$counts,
        names.arg = levels(freq_days_week$days_of_week),
        col = "#1b98e0",
        main = "Counts by Day of the Week",
        xlab = "Day of the Week",
        ylab = "Count")

#
#
# 5. Analysis of the top 10 posts for most liked, retweeted, viewed, bookmarked posts and it's hour
#
#

#Most liked posts

favourite_count_hours <- data.frame(data$favorite_count, data$hour_created_at)
sorted_data <- favourite_count_hours[order(favourite_count_hours$data.favorite_count, decreasing = TRUE), ]
top_10_favorite_count <- sorted_data[1:10, ]
top_10_favorite_count <- top_10_favorite_count[order(top_10_favorite_count$data.hour_created_at, decreasing = FALSE),]
#print(as.vector(top_10_favorite_count$data.hour_created_at))


barplot(top_10_favorite_count$data.favorite_count,
        names.arg = top_10_favorite_count$data.hour_created_at,
        col = "#1b98e0",
        main = "Posting Hours vs Favorite Count of the Top 10 Most Liked Posts",
        ylab = "Count")

tweet_ids <- as.vector(top_10_bookmarked_count$data.tweet_id)
print("Top 10 most liked posts:")
for (i in 1:10) {
  cat("Tweet ID:", tweet_ids[i], ", Hour:", top_10_bookmarked_count$data.hour_created_at[i], "\n")
}

#Most retweeted posts

retweeted_count_hours <- data.frame(data$retweet_count, data$hour_created_at)
sorted_data <- retweeted_count_hours[order(retweeted_count_hours$data.retweet_count, decreasing = TRUE), ]
top_10_retweeted_count <- sorted_data[1:10,]
top_10_retweeted_count <- top_10_retweeted_count[order(top_10_retweeted_count$data.hour_created_at, decreasing = FALSE),]
#print(as.vector(top_10_retweeted_count$data.hour_created_at))


barplot(top_10_retweeted_count$data.retweet_count,
        names.arg = top_10_retweeted_count$data.hour_created_at,
        col = "#1b98e0",
        main = "Posting Hours vs Retweeted Count of the Top 10 Most Retweeted Posts",
        ylab = "Count")

tweet_ids <- as.vector(top_10_bookmarked_count$data.tweet_id)
print("Top 10 most retweeted posts:")

for (i in 1:10) {
  cat("Tweet ID:", tweet_ids[i], ", Hour:", top_10_bookmarked_count$data.hour_created_at[i], "\n")
}

#Most bookmarked posts


bookmarked_count_hours <- data.frame(data$tweet_id, data$bookmark_count, data$hour_created_at)
sorted_data <- bookmarked_count_hours[order(bookmarked_count_hours$data.bookmark_count, decreasing = TRUE), ]
top_10_bookmarked_count <- sorted_data[1:10,]
top_10_bookmarked_count <- top_10_bookmarked_count[order(top_10_bookmarked_count$data.hour_created_at, decreasing = FALSE),]
#print(as.vector(top_10_bookmarked_count$data.hour_created_at))


barplot(top_10_bookmarked_count$data.bookmark_count,
        names.arg = top_10_bookmarked_count$data.hour_created_at,
        col = "#1b98e0",
        main = "Posting Hours vs Bookmark Count of the Top 10 Most Bookmarked Posts",
        ylab = "Count")

tweet_ids <- as.vector(top_10_bookmarked_count$data.tweet_id)
print("Top 10 most bookmarked posts:")
for (i in 1:10) {
  cat("Tweet ID:", tweet_ids[i], ", Hour:", top_10_bookmarked_count$data.hour_created_at[i], "\n")
}
