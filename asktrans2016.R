library(twitteR)
library(magrittr)
setup_twitter_oauth("", "", "", "")
hashtag <- "#AskTrans2016"
srch <- searchTwitter(hashtag, n=1500) %>%
  twListToDF() %>% 
  unique() 
data <- unique(srch)

library(lubridate)
library(ggplot2)

data$created <- ymd_hms(data$created)
data$created <- data$created + 3600
data$created_day <- format(data$created, "%Y-%m-%d")
table(data$created_day)

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
ggplot(data, aes(x = created_day)) + 
  geom_bar(fill = "#973232") +
  xlab("Jour") +
  ylab("Volume") +
  ggtitle("Tweets par jour avec #AskTrans2016") +
  ggtheme

length(unique(data$screenName))
users <- as.data.frame(table(data$screenName))
users <- users[order(users$Freq, decreasing = TRUE),]
ggplot(users[1:10,], aes(x = reorder(Var1, Freq), y = Freq)) + 
  geom_bar(fill = "#973232", stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("Twittos les plus vus sur #AskTrans2016") +
  ggtheme

nonrt <- subset(data, isRetweet == FALSE)
length(unique(nonrt$screenName))
usersnonrt <- as.data.frame(table(nonrt$screenName))
usersnonrt <- usersnonrt[order(usersnonrt$Freq, decreasing = TRUE),]
ggplot(usersnonrt, aes(x = reorder(Var1, Freq), y = Freq)) + 
  geom_bar(fill = "#973232", stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("Twittos ayant tweeté une fois ou plus #AskTrans2016") +
  ggtheme

per <- subset(data, created >= ymd_hms("2016-11-03 17:00:00") & created <= ymd_hms("2016-11-03 19:00:00")) %>%
  subset(isRetweet == FALSE)
sort(table(per$screenName))
usersper <- as.data.frame(table(per$screenName))
usersper <- usersper[order(usersper$Freq, decreasing = TRUE),]
ggplot(usersper, aes(x = reorder(Var1, Freq), y = Freq)) + 
  geom_bar(fill = "#973232", stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtheme

trans <- subset(nonrt, screenName == "TransMusicales") %>%
  subset(isRetweet == FALSE)
trans$statusSource = substr(trans$statusSource, regexpr('>', trans$statusSource) + 1, regexpr('</a>', trans$statusSource) - 1)
transsource <- as.data.frame(table(trans$statusSource))
transsource <- transsource[order(transsource$Freq, decreasing = TRUE),]
ggplot(transsource, aes(x = reorder(Var1, Freq), y = Freq)) + 
  geom_bar(fill = "#973232", stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("Source de publication de @TransMusicales") + 
  ggtheme
