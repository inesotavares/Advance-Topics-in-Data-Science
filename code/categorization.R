# Higher Education Institutions (HEI)
# ----> Publication Strategies

# Social Media Presence
# -> Promote the HEI
# -> Talent Acquisition: Students + Faculty
# -> Share Research
# -> Engagement Activities
# -> Society

# Data-> Tweets from HEI 

# 1st Part:
# Frequency: When to post?, Followers, Likes, ...
# Every Post needs to follow these categories: Image, Research
# Education, Engagement and Society

# 2nd Part:
# 

##### Libraries ####
library(dlookr)
library(ggplot2)
library(ggpubr)
library(stringr)
library(reshape2)
library(dplyr)
library(arules)
library(arulesViz)
library(tm)
library (text2vec)
library(wordcloud)
library(wordcloud2)
library(textstem)
library(textrank)
library(seededlda)
library(quanteda)
library(tokenizers)
library(topicmodels)
library(tidytext)
library(LDAvis)
library(textmineR)
library(texter)
library(RColorBrewer)
library(ggplot2)


setwd("C:/Users/maria/OneDrive - Universidade do Porto/Faculdade/CD/TACD/GitHub/TACD-project")
data<-read.csv("HEIs.csv")
attach(data)
dim(data) #11728    14


##########################################################################
############################# Categorization ############################# 
##########################################################################


# Define the cleanup function
cleanup <- function(docs, spec.words=NULL) {
  # Lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove English common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove whitespaces
  docs <- tm_map(docs, stripWhitespace)
  # Remove emojis and other non-ASCII characters
  docs <- tm_map(docs, content_transformer(function(x) gsub("[^\x01-\x7F]", "", x)))
  # Remove newline characters
  docs <- tm_map(docs, content_transformer(function(x) gsub("[\n]", "", x)))
  # Remove hashtags
  docs <- tm_map(docs, content_transformer(function(x) gsub("#\\S+", "", x)))
  #remove http: or https:
  # Apply the custom transformer to the corpus
  remove_patterns <- content_transformer(function(x, pattern) {
    gsub(pattern, "", x)
  })
  docs <- tm_map(docs, remove_patterns, "http://")
  docs <- tm_map(docs, remove_patterns, "https://")
  #remove slashes
  docs <- tm_map(docs, content_transformer(function(x) gsub("/", " ", x)))
  # Remove punctuation except hyphens
  docs <- tm_map(docs, content_transformer(function(x) gsub("[[:punct:]&&[^-]]", "", x)))
  #remove words with "yale"
  docs <- tm_map(docs, content_transformer(function(x) gsub("*yale*", "", x)))
  # Remove all non-alphanumeric characters
  docs <- tm_map(docs, content_transformer(function(x) gsub("[^[:alnum:] ]", "", x)))
  # Remove specific words
  docs <- tm_map(docs, removeWords, spec.words)
  # Text lemmatization
  docs <- tm_map(docs, content_transformer(lemmatize_strings))
  # Show final text
  docs
}



categorize_documents <- function(documents, top_words, word_categories) {
  doc_category <- character(length(documents$text))
  
  word_to_category <- unlist(lapply(names(word_categories), function(cat) {
    setNames(rep(cat, length(word_categories[[cat]])), word_categories[[cat]])
  }))
  
  for (i in seq_along(documents$text)) {
    text <- documents$text[i]
    
    # Initialize category counts based on word_categories
    category_count <- setNames(rep(0, length(word_categories)), names(word_categories))
    
    # Iterate through each word in the top words list
    for (word in top_words) {
      # Check if the word is in the dictionary and appears in the document
      if (word %in% names(word_to_category) && any(grepl(word, text, ignore.case = TRUE))) {
        # Get the category of the word
        category <- word_to_category[[word]]
        #print("************word******************")
        #print(word)
        #print("************category******************")
        #print(category)
        # Increment the category count
        category_count[category] <- category_count[category] + 1
      }
    }
    
    # If all category counts are zero, set engagement category to 1
    if (all(category_count == 0)) { 
      category_count["engagement"] <- 1 
    }
    
    # Assign the category with the highest count to the document
    doc_category[i] <- names(which.max(category_count))
  }
  
  return(doc_category)
}


dtm_remove_lowfreq <- function(dtm, remove_emptydocs = TRUE, min_term_freq = 1) {
  # Remove low-frequency terms
  dtm_freq <- rowSums(as.matrix(dtm))
  dtm <- dtm[dtm_freq >= min_term_freq, ]
  
  if (remove_emptydocs) {
    # Find non-empty documents
    non_empty_docs <- rowSums(as.matrix(dtm)) > 0
    # Filter out empty documents
    dtm <- dtm[non_empty_docs, ]
  }
  
  return(dtm)
}


institution_ids <- c("yale.csv", "wv.csv", "trinity.csv", "stanford.csv", "sb.csv", "mit.csv",
                     "manchester.csv", "leicester.csv", "harvard.csv", "goe.csv", "epfl.csv", "duke.csv")

institution_word_categories <- list(
  yale.csv = list(
    education = c("med", "school", "college", "learn", "work", "campus"),
    society = c("health"),
    image = c("new", "insight"),
    engagement = c("discuss", "help", "lead", "change", "year"),
    research = c("study", "find", "medicine", "researcher", "research", "patient")
  ),
  
  wv.csv = list(
    education = c("campus"),
    society = c("wvuhoops"),
    image = c("new", "good", "happy", "welcome", "love", "congratulation", "great","winner", "mountaneer"),
    engagement = c("thank", "week", "today", "birthday", "day","week"),
    research = c("")
  ),
  
  trinity.csv = list(
    education = c("campus","read", "work"),
    society = c("ireland"),
    image = c("new", "good", "congratulation"),
    engagement = c("amp", "today", "visit", "event", "support", "great", "week", "year", "information"),
    research = c("research", "find", "researcher")
  ),
  
  stanford.csv = list(
    education = c("campus", "quarter", "prof"),
    society = c("community", "andrew", "stanfords"),
    image = c("new", "big", "brodhead", "power"),
    engagement = c("congratulation", "week", "year", "game", "day", "fall", "spring"),
    research = c("research")
  ),
  
  sb.csv = list(
    education = c("campus","work", "professor"),
    society = c("gaucho","ucsantabarbara"),
    image = c("new", "good", "now", "future"),
    engagement = c("congrats", "week", "welcome","congratulation","story"),
    research = c("research", "work", "researcher")
  ),
  
  mit.csv = list(
    education = c("learn","work","material", "science", "work"),
    society = c("president", "help"),
    image = c("new","model"),
    engagement = c(""),
    research = c("study","research", "researcher", "find","engineer", "develop", "design","system","brain")
  ),
  
  manchester.csv = list(
    education = c("university", "campus","read"),
    society = c("support", "just", "help"),
    image = c("new", "good", "congratulation"),
    engagement = c("read", "thank", "hello", "send", "welcome"),
    research = c("research", "find", "study")
  ),
  
  leicester.csv = list(
    education = c("university", "campus", "professor"),
    society = c("day"),
    image = c("news", "good"),
    engagement = c("welcome", "today","open", "join","year"),
    research = c("research", "find", "space")
  ),
  
  harvard.csv = list(
    education = c("campus","harvardmed", "work","book"),
    society = c("health", "community", "people", "change"),
    image = c("new", "good", "first"),
    engagement = c("say", "may", "summer", "day","year", "study"),
    research = c("research", "researcher")
  ),
  
  goe.csv = list(
    education = c("uni", "postdoc"),
    society = c("career", "support", "international"),
    image = c("new", "good", "thank"),
    engagement = c("welcome", "event", "meet", "network", "amp", "lead", "show", "career"),
    research = c("research", "researcher", "find", "project")
  ),
  
  epfl.csv = list(
    education = c("professor", "campus", "work"),
    society = c("day"),
    image = c("new", "good"),
    engagement = c("amp", "day", "year", "now"),
    research = c("research", "researcher", "find", "develop", "project", "science", "scientist", "study")
  ),
  
  duke.csv = list(
    education = c("learn", "campus", "work", "faculty", "class"),
    society = c("help", "climate"),
    image = c("new", "first"),
    engagement = c("day", "year", "share", "member", "week"),
    research = c("research", "study", "climate", "find", "study")
  )
)
par(mfrow = c(3, 4),
    mar = c(0,0,0,0))

categorized_documents <- list()

documents <- data %>% filter(type != 'Reply')
documents <- data %>% filter(id != 'complutense.csv')

for (institution in institution_ids) {
  institution_id <- as.character(institution)
  
  institution_documents <- documents %>% filter(id == institution)
  
  corpus <- VCorpus(VectorSource(institution_documents$text))
  
  #cleanup function applied to the corpus
  docs <- cleanup(corpus, c("use", "can", "not", "have", "don't", "do"))
  docs <- tm_map(docs, removeWords, c("sph", "make", "som", "student", "teacher"))
  
  
  # Convert the Corpus to a Document-Term Matrix (DTM)
  dtm <- DocumentTermMatrix(docs)
  dtm <- dtm_remove_lowfreq(dtm, remove_emptydocs = TRUE)
  
  # Assuming dtm is your document-term matrix
  dtm <- dtm_remove_lowfreq(dtm, remove_emptydocs = TRUE, min_term_freq = 1)
  
  
  dtm.tfidf <- weightTfIdf(dtm)
  
  freq <- data.frame(sort(colSums(as.matrix(dtm.tfidf)), decreasing = TRUE))
  top_words <- rownames(head(freq, 20))
  
  print("#########################################")
  print(institution_id)
  print("---------------top words-------------------------")
  print(top_words)
  
  #wordcloud of the 20 most frequent words
  wordcloud(rownames(freq), freq[,1], max.words = 20, colors=brewer.pal(5, "Dark2"),scale=c(3,0.2))
  
  
  
  categories <- categorize_documents(institution_documents, top_words, institution_word_categories[[institution_id]])
 
  # Store categorized documents in the list
  categorized_documents <- c(categorized_documents, categories)
}


#dataset with the documents categories in the "category" column
documents$category <- categorized_documents
doc_text_category <- documents[, c("id", "text", "category")]



#
#
# Category frequency for each post
#
#

#most frequent category in yale posts
cat_yale <- doc_text_category %>% filter(id == 'yale.csv')

freq_cat_yale <- cat_yale %>%
  group_by(category) %>%
  summarise(counts = n())
freq_cat_yale <- freq_cat_yale[order(freq_cat_yale$counts, decreasing = TRUE),]
freq_cat_yale$id <- "yale.csv"


#wv.csv
cat_wv <- doc_text_category %>% filter(id == 'wv.csv')

freq_cat_wv <- cat_wv %>%
  group_by(category) %>%
  summarise(counts = n())
freq_cat_wv <- freq_cat_wv[order(freq_cat_wv$counts, decreasing = TRUE),]
freq_cat_wv$id <- "wv.csv"


# trinity.csv
cat_trinity <- doc_text_category %>% filter(id == 'trinity.csv')

freq_cat_trinity <- cat_trinity %>%
  group_by(category) %>%
  summarise(counts = n())
freq_cat_trinity <- freq_cat_trinity[order(freq_cat_trinity$counts, decreasing = TRUE), ]
freq_cat_trinity$id <- "trinity.csv"

# stanford.csv
cat_stanford <- doc_text_category %>% filter(id == 'stanford.csv')

freq_cat_stanford <- cat_stanford %>%
  group_by(category) %>%
  summarise(counts = n())
freq_cat_stanford <- freq_cat_stanford[order(freq_cat_stanford$counts, decreasing = TRUE), ]
freq_cat_stanford$id <- "stanford.csv"

# sb.csv
cat_sb <- doc_text_category %>% filter(id == 'sb.csv')

freq_cat_sb <- cat_sb %>%
  group_by(category) %>%
  summarise(counts = n())
freq_cat_sb <- freq_cat_sb[order(freq_cat_sb$counts, decreasing = TRUE), ]
freq_cat_sb$id <- "sb.csv"

# mit.csv
cat_mit <- doc_text_category %>% filter(id == 'mit.csv')

freq_cat_mit <- cat_mit %>%
  group_by(category) %>%
  summarise(counts = n())
freq_cat_mit <- freq_cat_mit[order(freq_cat_mit$counts, decreasing = TRUE), ]
freq_cat_mit$id <- "mit.csv"

# manchester.csv
cat_manchester <- doc_text_category %>% filter(id == 'manchester.csv')

freq_cat_manchester <- cat_manchester %>%
  group_by(category) %>%
  summarise(counts = n())
freq_cat_manchester <- freq_cat_manchester[order(freq_cat_manchester$counts, decreasing = TRUE), ]
freq_cat_manchester$id <- "manchester.csv"

# leicester.csv
cat_leicester <- doc_text_category %>% filter(id == 'leicester.csv')

freq_cat_leicester <- cat_leicester %>%
  group_by(category) %>%
  summarise(counts = n())
freq_cat_leicester <- freq_cat_leicester[order(freq_cat_leicester$counts, decreasing = TRUE), ]
freq_cat_leicester$id <- "leicester.csv"

# harvard.csv
cat_harvard <- doc_text_category %>% filter(id == 'harvard.csv')

freq_cat_harvard <- cat_harvard %>%
  group_by(category) %>%
  summarise(counts = n())
freq_cat_harvard <- freq_cat_harvard[order(freq_cat_harvard$counts, decreasing = TRUE), ]
freq_cat_harvard$id <- "harvard.csv"

# goe.csv
cat_goe <- doc_text_category %>% filter(id == 'goe.csv')

freq_cat_goe <- cat_goe %>%
  group_by(category) %>%
  summarise(counts = n())
freq_cat_goe <- freq_cat_goe[order(freq_cat_goe$counts, decreasing = TRUE), ]
freq_cat_goe$id <- "goe.csv"

# epfl.csv
cat_epfl <- doc_text_category %>% filter(id == 'epfl.csv')

freq_cat_epfl <- cat_epfl %>%
  group_by(category) %>%
  summarise(counts = n())
freq_cat_epfl <- freq_cat_epfl[order(freq_cat_epfl$counts, decreasing = TRUE), ]
freq_cat_epfl$id <- "epfl.csv"

# duke.csv
cat_duke <- doc_text_category %>% filter(id == 'duke.csv')
  
freq_cat_duke <- cat_duke %>%
  group_by(category) %>%
  summarise(counts = n())
freq_cat_duke <- freq_cat_duke[order(freq_cat_duke$counts, decreasing = TRUE), ]
freq_cat_duke$id <- "duke.csv"
  

list_cat <- list(
  freq_cat_trinity,
  freq_cat_stanford,
  freq_cat_sb,
  freq_cat_mit,
  freq_cat_manchester,
  freq_cat_leicester,
  freq_cat_harvard,
  freq_cat_goe,
  freq_cat_epfl,
  freq_cat_duke
)

# Concatenate the dataframes from the list
all_freq <- do.call(rbind, list_cat)
all_freq$category <- sapply(all_freq$category, function(x) x[[1]])
all_freq$category <- unlist(all_freq$category)
all_freq <- all_freq[order(all_freq$category), ]



ggplot(all_freq, aes(fill=id, y=counts, x=category)) + 
  geom_bar(position='dodge', stat='identity') +
  labs(x='Category', y='Count', fill='Institution', title="Count of posts' categoies for each univirsity")






