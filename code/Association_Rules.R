####### Association Rules ######

selected_data <- data[, c(1,4,5,6,7,8,9,11,13,15,16,18)]

detach(data)
attach(selected_data)
selected_data <- selected_data %>% mutate_if(is.character, as.factor)
bookmarks<-ifelse(selected_data$bookmark_count==0,'No Bookmarks',
                 ifelse(selected_data$bookmark_count<=10,
                        'Few Bookmarks','High Bookmarks'))
selected_data$bookmark_count<-as.factor(bookmarks)
names(selected_data)[names(selected_data) == "bookmark_count"]<-'bookmarks'
attach(selected_data)


favorites<-ifelse(selected_data$favorite_count==0,'No Favorite',
                 ifelse(selected_data$favorite_count<=100,
                        'Few Favorites','High Favorites'))
selected_data$favorite_count<-as.factor(favorites)
names(selected_data)[names(selected_data) == "favorite_count"]<-'favorites'
attach(selected_data)


retweets<-ifelse(selected_data$retweet_count==0,'No Retweets',
                 ifelse(selected_data$retweet_count<=100,
                        'Few Retweets','High Retweets'))
selected_data$retweet_count<-as.factor(retweets)
names(selected_data)[names(selected_data) == "retweet_count"]<-'retweets'
attach(selected_data)


replies<-ifelse(selected_data$reply_count==0,'No Replies',
                ifelse(selected_data$reply_count<=100,
                       'Few Replies','High Replies'))
selected_data$reply_count<-as.factor(replies)
names(selected_data)[names(selected_data) == "reply_count"]<-'replies'
attach(selected_data)


views<-ifelse(selected_data$view_count==0,'No Replies',
                ifelse(selected_data$view_count<=100,
                       'Few Replies','High Replies'))
selected_data$view_count<-as.factor(views)
names(selected_data)[names(selected_data) == "view_count"]<-'views'
attach(selected_data)


selected_data$media_type <- as.character(selected_data$media_type)
selected_data$media_type[selected_data$media_type == ""] <- "None"
selected_data$media_type <- as.factor(selected_data$media_type)

selected_data$hashtags <- as.character(selected_data$hashtags)
selected_data$hashtags[selected_data$hashtags == ""] <- "No Hashtag"
selected_data$hashtags <- as.factor(selected_data$hashtags)

attach(selected_data)





selected_data <- discretizeDF(selected_data,methods = list(
  hour_of_day=list(method="interval",4,labels=c("dawn","morning","afternoon","evening/night"))))

  
dft = as(selected_data, "transactions")
summary(dft)


rules <- apriori(dft, parameter = list(supp = 0.01, conf = 0.95))
sorted_rules <- sort(rules, by = "confidence")
arules::inspect(sorted_rules[1:10])
plot(sorted_rules, method = "graph", control = list(type = "items"))

rules = apriori(dft,parameter = list(confidence =1))
sorted_rules <- sort(rules, by = "support")
arules::inspect(sorted_rules[1:10])
plot(sorted_rules, method = "graph", control = list(type = "items"))

