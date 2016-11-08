maintenant <- read.csv("#Maintenant2016  - Archive.csv")
library(lubridate)
library(ggplot2)
library(magrittr)
library(data.table)
library(dplyr)
library(scales)
library(stringr)
data <- unique(maintenant)
data$time <- dmy_hms(data$time)
data$time <- data$time + 3600
data$created_day <- format(data$time, "%Y-%m-%d")
data <- subset(data, created_day >= as.Date("2016-09-05") & created_day <= as.Date("2016-11-05"))
data$isRetweet <- NA
data[grep("^RT.*", data$text),19] <- "Retweet"
data[-grep("^RT.*", data$text),19] <- "Direct"
table(data$created_day)
palette <- c("#973232", "#1E5B5B", "#6D8D2F", "#287928", "#E18C8C", "#548787", "#B8D283", "#70B470", "#B75353", "#326E6E", "#8CAA4E", "#439243", "#711515", "#0D4444", "#4D6914", "#115A11", "#490101", "#012C2C", "#2E4401", "#013A01")
ggtheme <- theme(axis.text=element_text(size=10),
                 axis.title=element_text(size=15),
                 title=element_text(size=18),
                 plot.title=element_text(margin=margin(0,0,20,0), size=18),
                 axis.title.x=element_text(margin=margin(20,0,0,0)),
                 axis.title.y=element_text(margin=margin(0,20,0,0)),
                 legend.text=element_text(size = 12),
                 plot.margin=margin(20,20,20,20), 
                 panel.background = element_rect(fill = "white"), 
                 panel.grid.major = element_line(colour = "grey"))
ggplot(data, aes(x = as.Date(created_day), fill = isRetweet)) + 
  geom_bar() +
  scale_fill_manual(values= palette) +
  xlab("Jour") +
  ylab("Volume") +
  labs(fill = "Nature") +
  ggtitle("Tweets et Retweets par jour avec #Maintenant2016") +
  scale_x_date(date_breaks = "1 week") +
  ggtheme

length(unique(data$from_user))
users <- as.data.frame(table(data$from_user))
users <- users[order(users$Freq, decreasing = TRUE),]
ggplot(users[1:10,], aes(x = reorder(Var1, Freq), y = Freq)) + 
  geom_bar(stat = "identity", fill = "#973232") +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("Twittos qui ont le plus tweeté et retweeté #Maintenant2016") +
  ggtheme

hash <- as.data.table(data)
hash <- hash[,.(follower_mean=mean(user_followers_count)),by=from_user]
hashdeux <- arrange(hash, desc(follower_mean))
hashdeux <- hashdeux[1:25,]
hashdeux$from_user <- factor(hashdeux$from_user, levels = hashdeux$from_user[order(hashdeux$follower_mean)])
hashdeux <- merge(x = hashdeux, y = users, by.x = "from_user", by.y = "Var1")
hashdeux$Vol_Tweet <- NA
for(i in 1:nrow(hashdeux)){
  if(hashdeux[i, "Freq"] >= 20){
    hashdeux[i, "Vol_Tweet"] <- "Plus de 20"
  } else if(hashdeux[i, "Freq"] >= 10){
    hashdeux[i, "Vol_Tweet"] <- "Entre 10 et 20"
  } else if(hashdeux[i, "Freq"] >= 5){
    hashdeux[i, "Vol_Tweet"] <- "Entre 5 et 10"
  } else {
    hashdeux[i, "Vol_Tweet"] <- "Moins de 10"
  }
}
ggplot(hashdeux, aes(x=from_user, y=follower_mean, fill = Vol_Tweet)) + 
  geom_bar(stat="identity")+
  scale_fill_manual(values = palette) +
  xlab(" ") + 
  ylab(" ") + 
  coord_flip() +
  labs(fill = "Volume de Tweets") +
  ggtitle("Utilisateurs les plus suivis et volume de tweets") + 
  scale_y_continuous(labels = comma) +
  ggtheme

nonrt <- subset(data, isRetweet == "Direct")
length(unique(nonrt$from_user))
usersnonrt <- as.data.frame(table(nonrt$from_user))
usersnonrt <- merge(x = usersnonrt, y = hash, by.x = "Var1", by.y = "from_user")
usersnonrt <- usersnonrt[order(usersnonrt$Freq, decreasing = TRUE),]
hashdeux$Vol_Follow <- NA
for(i in 1:nrow(usersnonrt)){
  if(usersnonrt[i, "follower_mean"] >= 10000){
    usersnonrt[i, "Vol_Follow"] <- "Plus de 10.000"
  } else if(usersnonrt[i, "follower_mean"] >= 1000){
    usersnonrt[i, "Vol_Follow"] <- "Entre 1000 et 10.000"
  } else {
    usersnonrt[i, "Vol_Follow"] <- "Moins de 1000"
  }
}
ggplot(usersnonrt[1:20,], aes(x = reorder(Var1, Freq), y = Freq, fill = Vol_Follow)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = palette) +
  coord_flip() +
  xlab("") +
  ylab("") +
  labs(fill = "Nombre de followers") +
  ggtitle("20 Twittos ayant le plus tweeté #Maintenant2016") +
  ggtheme

per <- subset(data, created_day >= ymd("2016-10-07") & created_day <= ymd("2016-10-16")) 
ggplot(per, aes(x = as.Date(created_day), fill = isRetweet)) + 
  geom_bar() +
  scale_fill_manual(values= palette) +
  xlab("Jour") +
  ylab("Volume") +
  labs(fill = "Nature") +
  ggtitle("Tweets et Retweets par jour pendant le festival #Maintenant2016") +
  ggtheme

hash <- c()
for(i in 1:nrow(data)){
  hash[i] <- str_extract_all(data[i, "text"], "#[0-9A-Za-z]* ")
}
hash <- unlist(hash) %>%
  tolower() %>%
  table() %>%
  as.data.frame() %>%
  dplyr::arrange(desc(Freq))
ggplot(hash[2:21,], aes(x = reorder(., Freq), y = Freq)) + 
  geom_bar(stat = "identity", fill = "#973232") +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("20 hashtags les plus récurrents") +
  ggtheme
