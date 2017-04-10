'''
The purpose of this script is to investigate evidence of an internet Outage in Gabon.

v2. Change to reflect longer run data, Gabon\'s nightly shut down.
'''

library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)

header <- c('tweet_created_at','tweet_id','tweet_id_str','tweet_in_reply_to_status_id','tweet_in_reply_to_status_id_str','tweet_in_reply_to_user_id','tweet_in_reply_to_user_id_str','tweet_lang','tweet_retweet_count','tweet_source','tweet_text','tweet_truncated','user_created_at','user_favourites_count','user_followers_count','user_following','user_friends_count','user_id','user_id_str','user_lang','user_location','user_screen_name','user_statuses_count','user_utc_offset','coordinates_lat','coordinates_long','country_code','full_name','id','name','place_type','SW_long','SW_lat','NW_long','NW_lat','NE_long','NE_lat','SE_long','SE_lat','hashtag1','hashtag2','hashtag3','hashtag4','hashtag5','hashtag6','hashtag7','hashtag8','hashtag9','hashtag10','hashtag11','hashtag12','hashtag13','hashtag14','hashtag15','user_mention1','user_mention2','user_mention3','user_mention4','user_mention5','user_mention6','user_mention7','user_mention8','user_mention9','user_mention10')


data <- read.csv('/Users/DonginKim/Dropbox/Censorship/Data/GA_2016-08-25_2016-10-28.csv', stringsAsFactors=FALSE)
names(data) <- header

# To create date column
data$tweet_created_at <- as.character(sub(" +0000", "", data$tweet_created_at, fixed=T))
data$date <- as.Date(data$tweet_created_at, format="%a %b %d %H:%M:%S %Y")

d_agg <- data %>% group_by(date) %>% summarize(tweets = sum(count), retweets = sum(retweet), mentions = sum(mention), hashtags = sum(hashtag), followers = mean(user_followers_count))
d_agg <- as.data.frame(d_agg)
d_users <- data %>% group_by(date) %>% summarize(users = length(unique(user_screen_name)))
d_users <- as.data.frame(d_users)
d_agg$tweets_per_user <- d_agg$tweets/d_users$users


# To create an hour column
data$hour <- as.POSIXct(data$tweet_created_at, format="%a %b %d %H:%M:%S %Y")  # Convert to PoSIXct
data$hour <- format(data$hour, "%H")  # Convert date to hour:minute %H

#head(data[,c(1, 11, 40:54, 65, 66)])

data$count <- 1
data$retweet <- ifelse('RT' %in% data$tweet_text, 1, 0)
data$mention <- ifelse(is.na(data$user_mention1) == TRUE, 0, 1)
data$hashtag <- ifelse(data$hashtag1 != '', 1, 0)


########################
# To aggragate_by hour #
########################

h_agg <- data %>% group_by(hour) %>% summarize(tweets = sum(count), retweets = sum(retweet), mentions = sum(mention), hashtags = sum(hashtag), followers = mean(user_followers_count))
h_agg <- as.data.frame(h_agg)

# Add users and tweets per users column to h_agg
h_users <- data %>% group_by(hour) %>% summarize(users = length(unique(user_screen_name)))
h_users <- as.data.frame(h_users)
h_agg$tweets_per_user <- h_agg$tweets/h_users$users
h_agg$users <- h_users$users

# Convert Hour
# To Create Empty dataframe
na_values <- rep(NA,84)
hour_convert=c(0,1,2,3,4,5,6,7,8,9,10,11,na_values)
dim(hour_convert) <- c(12,8)
colnames(hour_convert) = c("hour","tweets","retweets","mentions","hashtags","followers","tweets_per_user","users")
h_agg = rbind(h_agg,hour_convert)

# To Swap
for(i in 1:12){
  h_agg[i+24,2:length(h_agg)] <- h_agg[i,2:length(h_agg)]
}
h_agg <- h_agg[13:36,]
h_agg$hour[1:12]=as.numeric(h_agg$hour[1:12])-24

# GMT to Gabon Time
gabon_convert=c(11,NA,NA,NA,NA,NA,NA,NA)
dim(gabon_convert) <- c(1,8)
colnames(gabon_convert) = c("hour","tweets","retweets","mentions","hashtags","followers","tweets_per_user","users")
h_agg = rbind(h_agg,gabon_convert)

# To Swap
for(i in 24:1){  
  h_agg[i+1,2:length(h_agg)] <- h_agg[i,2:length(h_agg)] 
} 
# from the 24th row to 1st row 
# to 24+1th row to 1st+1 row 
# to add one more hour to the current hour

h_agg[1,] <- h_agg[25,] # 25th row moves to the 1st row for date change
h_agg <- h_agg[1:24,]   
h_agg$hour[1]=as.numeric(h_agg$hour[1])-23  # to adjust new hour for the 1st row

h_periods = c(-6, 6)
# new labels for x-axis
new_xlim = c("Noon", "6PM", "Midnight", "6AM", "11AM")


##########################
# To plot Tweets-by-hour #
##########################

pdf('/Users/DonginKim/Dropbox/Censorship/Figures/Gabon_TrafficAggregated_EntireTime.pdf')

ggplot(data=h_agg, aes(x=as.integer(hour), y=tweets)) +
  theme_calc() +
  scale_color_calc() +
  labs(title="Traffic for Entire Time", x="", y="Total number of tweets") +
  theme(plot.title = element_text(size=15, face="bold", margin=margin(8,0,8,0))) +
  geom_line() +
  scale_x_discrete(labels=new_xlim, limits = c(-12,-6,0,6,11), expand=c(0.025,0)) +
  scale_y_continuous(limits=c(0,500), expand = c(0,0)) +
  geom_vline(xintercept=h_periods,linetype="dotted",lwd=.5) +
  geom_text(aes(label="Outage", x=0, y=400), colour="black", angle=360) +
  annotate("rect", xmin=-6, xmax=6, ymin=0, ymax=Inf, alpha=.2)

dev.off()


###############
# Before 8/30 #
###############

before_data <- data[data$date < as.Date('2016-08-30'),] # set the date range

before_agg <- before_data %>% group_by(hour) %>% summarize(tweets = sum(count), retweets = sum(retweet), mentions = sum(mention), hashtags = sum(hashtag), followers = mean(user_followers_count))
before_agg <- as.data.frame(before_agg)

# Add users and tweets per users column to before_agg
before_users <- before_data %>% group_by(hour) %>% summarize(users = length(unique(user_screen_name)))
before_users <- as.data.frame(before_users)
before_agg$tweets_per_user <- before_agg$tweets/before_users$users
before_agg$users <- before_users$users

# Convert Hour
# To Create Empty dataframe
na_values <- rep(NA,84)
hour_convert=c(0,1,2,3,4,5,6,7,8,9,10,11,na_values)
dim(hour_convert) <- c(12,8)
colnames(hour_convert) = c("hour","tweets","retweets","mentions","hashtags","followers","tweets_per_user","users")
before_agg = rbind(before_agg,hour_convert)

# To Swap
for(i in 1:12){
  before_agg[i+24,2:length(before_agg)] <- before_agg[i,2:length(before_agg)]
}
before_agg <- before_agg[13:36,]
before_agg$hour[1:12]=as.numeric(before_agg$hour[1:12])-24

# GMT to Gabon Time
gabon_convert=c(11,NA,NA,NA,NA,NA,NA,NA)
dim(gabon_convert) <- c(1,8)
colnames(gabon_convert) = c("hour","tweets","retweets","mentions","hashtags","followers","tweets_per_user","users")
before_agg = rbind(before_agg,gabon_convert)

# To Swap
for(i in 24:1){  
  before_agg[i+1,2:length(before_agg)] <- before_agg[i,2:length(before_agg)] 
} 
# from the 24th row to 1st row 
# to 24+1th row to 1st+1 row 
# to add one more hour to the current hour

before_agg[1,] <- before_agg[25,] # 25th row moves to the 1st row for date change
before_agg <- before_agg[1:24,]   
before_agg$hour[1]=as.numeric(before_agg$hour[1])-23  # to adjust new hour for the 1st row


#####################
# Plot: Before 8/30 #
#####################

pdf('/Users/DonginKim/Dropbox/Censorship/Figures/Gabon_TrafficAggregated_Before08302016.pdf')

ggplot(data=before_agg, aes(x=as.integer(hour), y=tweets)) +
  theme_calc() +
  scale_color_calc() +
  labs(title="Traffic Before 8/30", x="", y="Total number of tweets") +
  theme(plot.title = element_text(size=15, face="bold", margin=margin(8,0,8,0))) +
  scale_x_discrete(labels=new_xlim, limits = c(-12,-6,0,6,11), expand=c(0.025,0)) +
  scale_y_continuous(limits=c(0,500),expand = c(0,0)) +
  geom_line() +
  geom_vline(xintercept=h_periods,linetype="dotted",lwd=.5) +
  geom_text(aes(label="Outage", x=0, y=0.8*500), colour="black", angle=360) +
  annotate("rect", xmin=-6, xmax=6, ymin=0, ymax=Inf, alpha=.2)

dev.off()


##############
# After 8/30 #
##############

after_data <- data[data$date >= as.Date('2016-08-30'),]

after_agg <- after_data %>% group_by(hour) %>% summarize(tweets = sum(count), retweets = sum(retweet), mentions = sum(mention), hashtags = sum(hashtag), followers = mean(user_followers_count))
after_agg <- as.data.frame(after_agg)

# Add users and tweets per users column to after_agg
after_users <- after_data %>% group_by(hour) %>% summarize(users = length(unique(user_screen_name)))
after_users <- as.data.frame(after_users)
after_agg$tweets_per_user <- after_agg$tweets/after_users$users
after_agg$users <- after_users$users

# Convert Hour
# To Create Empty dataframe
na_values <- rep(NA,84)
hour_convert=c(0,1,2,3,4,5,6,7,8,9,10,11,na_values)
dim(hour_convert) <- c(12,8)
colnames(hour_convert) = c("hour","tweets","retweets","mentions","hashtags","followers","tweets_per_user","users")
after_agg = rbind(after_agg,hour_convert)

# To Swap
for(i in 1:12){
  after_agg[i+24,2:length(after_agg)] <- after_agg[i,2:length(after_agg)]
}
after_agg <- after_agg[13:36,]
after_agg$hour[1:12]=as.numeric(after_agg$hour[1:12])-24

# GMT to Gabon Time
gabon_convert=c(11,NA,NA,NA,NA,NA,NA,NA)
dim(gabon_convert) <- c(1,8)
colnames(gabon_convert) = c("hour","tweets","retweets","mentions","hashtags","followers","tweets_per_user","users")
after_agg = rbind(after_agg,gabon_convert)

# To Swap
for(i in 24:1){  
  after_agg[i+1,2:length(after_agg)] <- after_agg[i,2:length(after_agg)] 
} 
# from the 24th row to 1st row 
# to 24+1th row to 1st+1 row 
# to add one more hour to the current hour

after_agg[1,] <- after_agg[25,] # 25th row moves to the 1st row for date change
after_agg <- after_agg[1:24,]   
after_agg$hour[1]=as.numeric(after_agg$hour[1])-23  # to adjust new hour for the 1st row


####################
# Plot: After 8/30 #
####################

pdf('/Users/DonginKim/Dropbox/Censorship/Figures/Gabon_TrafficAggregated_After08302016.pdf')
ggplot(data=after_agg, aes(x=as.integer(hour), y=tweets)) +
  theme_calc() +
  scale_color_calc() +
  labs(title="Traffic After 8/30", x="", y="Total number of tweets") +
  theme(plot.title = element_text(size=15, face="bold", margin=margin(8,0,8,0))) +
  scale_x_discrete(labels=new_xlim, limits=c(-12,-6,0,6,11), expand=c(0.025,0)) +
  scale_y_continuous(limits=c(0,500),expand = c(0,0)) +
  geom_line() +
  geom_vline(xintercept=h_periods,linetype="dotted",lwd=.5) +
  geom_text(aes(label="Outage", x=0, y=0.8*500), colour="black", angle=360) +
  annotate("rect", xmin=-6, xmax=6, ymin=0, ymax=Inf, alpha=.2)
dev.off()



######################
# Censorship Traffic #
######################

# set the censorship date range
censor_data <- data[data$date >= as.Date('2016-08-31') & data$date <= as.Date('2016-09-29'),]

# aggragate the censorship data by hour
censor_agg <- censor_data %>% group_by(hour) %>% summarize(tweets = sum(count), retweets = sum(retweet), mentions = sum(mention), hashtags = sum(hashtag), followers = mean(user_followers_count))
censor_agg <- as.data.frame(censor_agg)

# Add users and tweets per users column to censor_agg
censor_agg$tweets_per_user <- censor_agg$tweets/censor_users$users
censor_agg$users <- censor_users$users

# Convert Hour
# To Create Empty dataframe
na_values <- rep(NA,84)
hour_convert=c(0,1,2,3,4,5,6,7,8,9,10,11,na_values)
dim(hour_convert) <- c(12,8)
colnames(hour_convert) = c("hour","tweets","retweets","mentions","hashtags","followers","tweets_per_user","users")
censor_agg = rbind(censor_agg,hour_convert)

# To Swap
for(i in 1:12){
  censor_agg[i+24,2:length(censor_agg)] <- censor_agg[i,2:length(censor_agg)]
}
censor_agg <- censor_agg[13:36,]
censor_agg$hour[1:12]=as.numeric(censor_agg$hour[1:12])-24

# GMT to Gabon Time
gabon_convert=c(11,NA,NA,NA,NA,NA,NA,NA)
dim(gabon_convert) <- c(1,8)
colnames(gabon_convert) = c("hour","tweets","retweets","mentions","hashtags","followers","tweets_per_user","users")
censor_agg = rbind(censor_agg,gabon_convert)

# To Swap
for(i in 24:1){  
  censor_agg[i+1,2:length(censor_agg)] <- censor_agg[i,2:length(censor_agg)] 
} 
# from the 24th row to 1st row 
# to 24+1th row to 1st+1 row 
# to add one more hour to the current hour

censor_agg[1,] <- censor_agg[25,] # 25th row moves to the 1st row for date change
censor_agg <- censor_agg[1:24,]   
censor_agg$hour[1]=as.numeric(censor_agg$hour[1])-23  # to adjust new hour for the 1st row



# Convert hour for censor_users

censor_users <- censor_data %>% group_by(hour) %>% summarize(users = length(unique(user_screen_name)))
censor_users <- as.data.frame(censor_users)

# To Create Empty dataframe
na_values <- rep(NA,12)
hour_convert=c(0,1,2,3,4,5,6,7,8,9,10,11,na_values)
dim(hour_convert) <- c(12,2)
colnames(hour_convert) = c("hour","users")
censor_users = rbind(censor_users,hour_convert)

# To Swap
for(i in 1:12){
  censor_users[i+24,2:length(censor_users)] <- censor_users[i,2:length(censor_users)]
}
censor_users <- censor_users[13:36,]
censor_users$hour[1:12]=as.numeric(censor_users$hour[1:12])-24

# GMT to Gabon Time
gabon_convert=c(11,NA)
dim(gabon_convert) <- c(1,2)
colnames(gabon_convert) = c("hour","users")
censor_users = rbind(censor_users,gabon_convert)

# To Swap
for(i in 24:1){  
  censor_users[i+1,2:length(censor_users)] <- censor_users[i,2:length(censor_users)] 
} 
# from the 24th row to 1st row 
# to 24+1th row to 1st+1 row 
# to add one more hour to the current hour

censor_users[1,] <- censor_users[25,] # 25th row moves to the 1st row for date change
censor_users <- censor_users[1:24,]   
censor_users$hour[1]=as.numeric(censor_users$hour[1])-23  # to adjust new hour for the 1st row


#####################
# Plots: Censorship #
#####################

# tweets
pdf('/Users/DonginKim/Dropbox/Censorship/Figures/Gabon_censored_tweets.pdf')
ggplot(data=censor_agg, aes(x=as.integer(hour), y=tweets)) +
  theme_calc() +
  scale_color_calc() +
  labs(title="Censored Traffic", x="", y="Total number of tweets") +
  theme(plot.title = element_text(size=15, face="bold", margin=margin(8,0,8,0))) +
  scale_x_discrete(labels=new_xlim, limits=c(-12,-6,0,6,11), expand=c(0.025,0)) +
  scale_y_continuous(limits=c(0,500),expand = c(0,0)) +
  geom_line() +
  geom_vline(xintercept=h_periods,linetype="dotted",lwd=.5) +
  geom_text(aes(label="Outage", x=0, y=0.8*500), colour="black") +
  annotate("rect", xmin=-6, xmax=6, ymin=0, ymax=Inf, alpha=.2)
dev.off()

# followers
pdf('/Users/DonginKim/Dropbox/Censorship/Figures/Gabon_censored_followers.pdf')
ggplot(data=censor_agg, aes(x=as.integer(hour), y=followers)) +
  theme_calc() +
  scale_color_calc() +
  labs(title="Censored Traffic", x="", y="Total number of followers") +
  theme(plot.title = element_text(size=15, face="bold", margin=margin(8,0,8,0))) +
  scale_x_discrete(labels=new_xlim, limits=c(-12,-6,0,6,11), expand=c(0.025,0)) +
  scale_y_continuous(limits=c(0,50000), expand = c(0,0)) +
  geom_line() +
  geom_vline(xintercept=h_periods,linetype="dotted",lwd=.5) +
  geom_text(aes(label="Outage", x=0, y=0.8*50000), colour="black") +
  annotate("rect", xmin=-6, xmax=6, ymin=0, ymax=Inf, alpha=.2)


dev.off()

# mentions/tweets
pdf('/Users/DonginKim/Dropbox/Censorship/Figures/Gabon_censored_mentions.pdf')
ggplot(data=censor_agg, aes(x=as.integer(hour), y=mentions/tweets)) +
  theme_calc() +
  scale_color_calc() +
  labs(title="Censored Traffic", x="", y="mentions/tweets") +
  theme(plot.title = element_text(size=15, face="bold", margin=margin(8,0,8,0))) +
  scale_x_discrete(labels=new_xlim, limits=c(-12,-6,0,6,11), expand=c(0.025,0)) +
  scale_y_continuous(limits=c(0,1), expand = c(0,0)) +
  geom_line() +
  geom_vline(xintercept=h_periods,linetype="dotted",lwd=.5) +
  geom_text(aes(label="Outage", x=0, y=0.8), colour="black") +
  annotate("rect", xmin=-6, xmax=6, ymin=0, ymax=Inf, alpha=.2)
dev.off()

# hashtags/tweets
pdf('/Users/DonginKim/Dropbox/Censorship/Figures/Gabon_censored_hashtags.pdf')
ggplot(data=censor_agg, aes(x=as.integer(hour), y=hashtags/tweets)) +
  theme_calc() +
  scale_color_calc() +
  labs(title="Censored Traffic", x="", y="hashtags/tweets") +
  theme(plot.title = element_text(size=15, face="bold", margin=margin(8,0,8,0))) +
  scale_x_discrete(labels=new_xlim, limits=c(-12,-6,0,6,11), expand=c(0.025,0)) +
  scale_y_continuous(limits=c(0,1), expand = c(0,0)) +
  geom_line() +
  geom_vline(xintercept=h_periods,linetype="dotted",lwd=.5) +
  geom_text(aes(label="Outage", x=0, y=0.8), colour="black") +
  annotate("rect", xmin=-6, xmax=6, ymin=0, ymax=Inf, alpha=.2)
dev.off()

# tweets_per_user
pdf('/Users/DonginKim/Dropbox/Censorship/Figures/Gabon_censored_tweets_per_user.pdf')
ggplot(data=censor_agg, aes(x=as.integer(hour), y=tweets_per_user)) +
  theme_calc() +
  scale_color_calc() +
  labs(title="Censored Traffic", x="", y="Tweets per user") +
  theme(plot.title = element_text(size=15, face="bold", margin=margin(8,0,8,0))) +
  scale_x_discrete(labels=new_xlim, limits=c(-12,-6,0,6,11), expand=c(0.025,0)) +
  scale_y_continuous(limits=c(0,20), expand = c(0,0)) +
  geom_line() +
  geom_vline(xintercept=h_periods,linetype="dotted",lwd=.5) +
  geom_text(aes(label="Outage", x=0, y=0.8*20), colour="black") +
  annotate("rect", xmin=-6, xmax=6, ymin=0, ymax=Inf, alpha=.2)
dev.off()

# users
pdf('/Users/DonginKim/Dropbox/Censorship/Figures/Gabon_censored_users.pdf')
ggplot(data=censor_users, aes(x=as.integer(hour), y=users)) +
  theme_calc() +
  scale_color_calc() +
  labs(title="Censored Traffic", x="", y="Users") +
  theme(plot.title = element_text(size=15, face="bold", margin=margin(8,0,8,0))) +
  scale_x_discrete(labels=new_xlim, limits=c(-12,-6,0,6,11), expand=c(0.025,0)) +
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
  geom_line() +
  geom_vline(xintercept=h_periods,linetype="dotted",lwd=.5) +
  geom_text(aes(label="Outage", x=0, y=80), colour="black") +
  annotate("rect", xmin=-6, xmax=6, ymin=0, ymax=Inf, alpha=.2)
dev.off()


##########################
# Non-Censorship Traffic #
##########################

# set the non-censorship date range
non_data <- data[data$date < as.Date('2016-08-31') | data$date > as.Date('2016-09-29'),]


# aggragate the censorship data by hour
non_agg <- non_data %>% group_by(hour) %>% summarize(tweets = sum(count), retweets = sum(retweet), mentions = sum(mention), hashtags = sum(hashtag), followers = mean(user_followers_count))
non_agg <- as.data.frame(non_agg)

# Add users and tweets per users column to non_agg
non_agg$tweets_per_user <- non_agg$tweets/non_users$users
non_agg$users <- non_users$users

# Convert Hour
# To Create Empty dataframe
na_values <- rep(NA,84)
hour_convert=c(0,1,2,3,4,5,6,7,8,9,10,11,na_values)
dim(hour_convert) <- c(12,8)
colnames(hour_convert) = c("hour","tweets","retweets","mentions","hashtags","followers","tweets_per_user","users")
non_agg = rbind(non_agg,hour_convert)

# To Swap
for(i in 1:12){
  non_agg[i+24,2:length(non_agg)] <- non_agg[i,2:length(non_agg)]
}
non_agg <- non_agg[13:36,]
non_agg$hour[1:12]=as.numeric(non_agg$hour[1:12])-24

# GMT to Gabon Time
gabon_convert=c(11,NA,NA,NA,NA,NA,NA,NA)
dim(gabon_convert) <- c(1,8)
colnames(gabon_convert) = c("hour","tweets","retweets","mentions","hashtags","followers","tweets_per_user","users")
non_agg = rbind(non_agg,gabon_convert)

# To Swap
for(i in 24:1){  
  non_agg[i+1,2:length(non_agg)] <- non_agg[i,2:length(non_agg)] 
} 
# from the 24th row to 1st row 
# to 24+1th row to 1st+1 row 
# to add one more hour to the current hour

non_agg[1,] <- non_agg[25,] # 25th row moves to the 1st row for date change
non_agg <- non_agg[1:24,]   
non_agg$hour[1]=as.numeric(non_agg$hour[1])-23  # to adjust new hour for the 1st row



# Convert hour for non_users

non_users <- non_data %>% group_by(hour) %>% summarize(users = length(unique(user_screen_name)))
non_users <- as.data.frame(non_users)

# To Create Empty dataframe
na_values <- rep(NA,12)
hour_convert=c(0,1,2,3,4,5,6,7,8,9,10,11,na_values)
dim(hour_convert) <- c(12,2)
colnames(hour_convert) = c("hour","users")
non_users = rbind(non_users,hour_convert)

# To Swap
for(i in 1:12){
  non_users[i+24,2:length(non_users)] <- non_users[i,2:length(non_users)]
}
non_users <- non_users[13:36,]
non_users$hour[1:12]=as.numeric(non_users$hour[1:12])-24

# GMT to Gabon Time
gabon_convert=c(11,NA)
dim(gabon_convert) <- c(1,2)
colnames(gabon_convert) = c("hour","users")
non_users = rbind(non_users,gabon_convert)

# To Swap
for(i in 24:1){  
  non_users[i+1,2:length(non_users)] <- non_users[i,2:length(non_users)] 
} 
# from the 24th row to 1st row 
# to 24+1th row to 1st+1 row 
# to add one more hour to the current hour

non_users[1,] <- non_users[25,] # 25th row moves to the 1st row for date change
non_users <- non_users[1:24,]   
non_users$hour[1]=as.numeric(non_users$hour[1])-23  # to adjust new hour for the 1st row


#########################
# Plots: Non-Censorship #
#########################

# tweets
pdf('/Users/DonginKim/Dropbox/Censorship/Figures/Gabon_normal_tweets.pdf')
ggplot(data=non_agg, aes(x=as.integer(hour), y=tweets)) +
  theme_calc() +
  scale_color_calc() +
  labs(title="Normal Traffic", x="", y="Total number of tweets") +
  theme(plot.title = element_text(size=15, face="bold", margin=margin(8,0,8,0))) +
  scale_x_discrete(labels=new_xlim, limits=c(-12,-6,0,6,11), expand=c(0.025,0)) +
  scale_y_continuous(limits=c(0,500),expand = c(0,0)) +
  geom_line() +
  geom_vline(xintercept=h_periods,linetype="dotted",lwd=.5) +
  geom_text(aes(label="Outage", x=0, y=0.8*500), colour="black", angle=360) +
  annotate("rect", xmin=-6, xmax=6, ymin=0, ymax=Inf, alpha=.2)
dev.off()

# followers
pdf('/Users/DonginKim/Dropbox/Censorship/Figures/Gabon_normal_followers.pdf')
ggplot(data=non_agg, aes(x=as.integer(hour), y=followers)) +
  theme_calc() +
  scale_color_calc() +
  labs(title="Normal Traffic", x="", y="Total number of followers") +
  theme(plot.title = element_text(size=15, face="bold", margin=margin(8,0,8,0))) +
  scale_x_discrete(labels=new_xlim, limits=c(-12,-6,0,6,11), expand=c(0.025,0)) +
  scale_y_continuous(limits=c(0,50000), expand = c(0,0)) +
  geom_line() +
  geom_vline(xintercept=h_periods,linetype="dotted",lwd=.5) +
  geom_text(aes(label="Outage", x=0, y=0.8*50000), colour="black", angle=360) +
  annotate("rect", xmin=-6, xmax=6, ymin=0, ymax=Inf, alpha=.2)
dev.off()

# mentions/tweets
pdf('/Users/DonginKim/Dropbox/Censorship/Figures/Gabon_normal_mentions.pdf')
ggplot(data=non_agg, aes(x=as.integer(hour), y=mentions/tweets)) +
  theme_calc() +
  scale_color_calc() +
  labs(title="Normal Traffic", x="", y="mentions/tweets") +
  theme(plot.title = element_text(size=15, face="bold", margin=margin(8,0,8,0))) +
  scale_x_discrete(labels=new_xlim, limits=c(-12,-6,0,6,11), expand=c(0.025,0)) +
  scale_y_continuous(limits=c(0,1), expand = c(0,0)) +
  geom_line() +
  geom_vline(xintercept=h_periods,linetype="dotted",lwd=.5) +
  geom_text(aes(label="Outage", x=0, y=0.8), colour="black", angle=360) +
  annotate("rect", xmin=-6, xmax=6, ymin=0, ymax=Inf, alpha=.2)
dev.off()

# hashtags/tweets
pdf('/Users/DonginKim/Dropbox/Censorship/Figures/Gabon_normal_hashtags.pdf')
ggplot(data=non_agg, aes(x=as.integer(hour), y=hashtags/tweets)) +
  theme_calc() +
  scale_color_calc() +
  labs(title="Normal Traffic", x="", y="hashtags/tweets") +
  theme(plot.title = element_text(size=15, face="bold", margin=margin(8,0,8,0))) +
  scale_x_discrete(labels=new_xlim, limits=c(-12,-6,0,6,11), expand=c(0.025,0)) +
  scale_y_continuous(limits=c(0,1), expand = c(0,0)) +
  geom_line() +
  geom_vline(xintercept=h_periods,linetype="dotted",lwd=.5) +
  geom_text(aes(label="Outage", x=0, y=0.8), colour="black", angle=360) +
  annotate("rect", xmin=-6, xmax=6, ymin=0, ymax=Inf, alpha=.2)
dev.off()

# tweets_per_user
pdf('/Users/DonginKim/Dropbox/Censorship/Figures/Gabon_normal_tweets_per_user.pdf')
ggplot(data=non_agg, aes(x=as.integer(hour), y=tweets_per_user)) +
  theme_calc() +
  scale_color_calc() +
  labs(title="Normal Traffic", x="", y="Tweets per user") +
  theme(plot.title = element_text(size=15, face="bold", margin=margin(8,0,8,0))) +
  scale_x_discrete(labels=new_xlim, limits=c(-12,-6,0,6,11), expand=c(0.025,0)) +
  scale_y_continuous(limits=c(0,20), expand = c(0,0)) +
  geom_line() +
  geom_vline(xintercept=h_periods,linetype="dotted",lwd=.5) +
  geom_text(aes(label="Outage", x=0, y=0.8*20), colour="black", angle=360) +
  annotate("rect", xmin=-6, xmax=6, ymin=0, ymax=Inf, alpha=.2)
dev.off()

# users
pdf('/Users/DonginKim/Dropbox/Censorship/Figures/Gabon_normal_users.pdf')
ggplot(data=non_users, aes(x=as.integer(hour), y=users)) +
  theme_calc() +
  scale_color_calc() +
  labs(title="Normal Traffic", x="", y="Users") +
  theme(plot.title = element_text(size=15, face="bold", margin=margin(8,0,8,0))) +
  scale_x_discrete(labels=new_xlim, limits=c(-12,-6,0,6,11), expand=c(0.025,0)) +
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) + 
  geom_line() +
  geom_vline(xintercept=h_periods,linetype="dotted",lwd=.5) +
  geom_text(aes(label="Outage", x=0, y=80), colour="black", angle=360) +
  annotate("rect", xmin=-6, xmax=6, ymin=0, ymax=Inf, alpha=.2)
dev.off()

