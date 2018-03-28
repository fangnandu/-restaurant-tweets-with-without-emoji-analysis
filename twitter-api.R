

# 1 envrionment set up
install.packages("twitteR")
library(twitteR) #for the rest API
#https://cran.r-project.org/web/packages/twitteR/twitteR.pdf
install.packages("streamR")
library(streamR) #for the streaming API
install.packages("ROAuth")
library(ROAuth)
#https://cran.r-project.org/web/packages/streamR/streamR.pdf

library(tidyverse)
install.packages("rtweet")
install.packages("tidytext")
install.packages("syuzhet")
library(rtweet) #https://cran.r-project.org/web/packages/rtweet/rtweet.pdf
library(tidyverse)
library(lubridate) #functions for date/time data
library(scales) #scales for data visualization
library(stringr) #string manipulation
library(tidytext) #for text mining
library(syuzhet) #corpus


#To get your API credentials:

#1. go to https://apps.twitter.com/ and hit the "Create New App" button.

#2. Give your app a name, a description, and a website. For the website, you can use a placeholder such as "http://thisisaplaceholder.com".

#3. When you're done, hit the "Create new Twitter application" button.

#4. Next, go to the "Keys and Access Tokens" tab and hit the Create my access token button at the bottom.

#5. Copy the information from the next page into the four fields below.


access_token <- "963087889818095616-oVdJkzPB1OCVrWVys3YvEBmJM3E3pWI"
access_secret <-"brYIiaEDlVpJ8F4OATCy4etIxXRR9vux7rUeusW8LLG9b"
consumer_key <- "t4yXoZMohTtuCA6N4gUd5nG3r"
consumer_secret <- "WskB4TNopyEg4h5mdrFtzDLrPvzxj6rpnbVRdSNl3bi8LdN4Ky"

#Why are there four keys?

#The primary use for the Twitter API is for websites that want to integrate with Twitter.
#One set of keys belongs to the website. The other set are generated for each of the site's users.

#We are using the same double-key authentication method, so we are going to use both sets.

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
#When it prompts you with the question below, answer 2: No
#    Use a local file to cache OAuth access credentials between R sessions?
#    1: Yes
#    2: No



# *** STREAMING API ***

oathfunction <- function(consumer_key, consumer_secret, access_token, access_secret){
  my_oauth <- ROAuth::OAuthFactory$new(consumerKey=consumer_key,
                                       consumerSecret=consumer_secret,
                                       oauthKey=access_token,
                                       oauthSecret=access_secret,
                                       needsVerifier=FALSE, handshakeComplete=TRUE,
                                       verifier="1",
                                       requestURL="https://api.twitter.com/oauth/request_token",
                                       authURL="https://api.twitter.com/oauth/authorize",
                                       accessURL="https://api.twitter.com/oauth/access_token",
                                       signMethod="HMAC")
  return(my_oauth)
}

my_oauth <- oathfunction(consumer_key, consumer_secret, access_token, access_secret)

# Set the parameters of your stream
# Keep in mind that many of these parameters do not work together
# For example, the location search cannot be paired with other parameters

file = "~/Desktop/max r/MUSA-620-Week-8/mytwitterstream.json"       #The data will be saved to this file as long as the stream is running
track = c("restaurant")                 #"Search" by keyword(s)
follow = NULL                           #"Search" by Twitter user(s)
loc = NULL #c(-179, -70, 179, 70)             #Geographical bounding box -- (min longitute,min latitude,max longitute,max latitude)
lang = NULL                             #Filter by language
timeout = NULL #1000                          #Maximum time (in miliseconds)
tweets = 15000 #1000                      #Maximum tweets (usually, it will be less)


filterStream(file.name = file, 
             track = track,
             follow = follow, 
             locations = loc, 
             language = lang,
             #timeout = timeout, 
             tweets = tweets, 
             oauth = my_oauth,
             verbose = TRUE)

#parse the file containing the tweets
streamedtweets2 <- parseTweets(file, verbose = FALSE)

#set the proper encoding (UTF8 includes many characters not used in the English language)
streamedtweets2$text <- iconv(streamedtweets2$text, from = "UTF-8", to = "ASCII", sub="")
streamedtweets2$text <- iconv(streamedtweets2$text , from = "latin1", to = "ascii", sub = "byte")

#replace line breaks with spaces
streamedtweets2$text <- gsub("\n", " ", streamedtweets2$text)
                       
View(streamedtweets2)


#select the ones is belongs to restrauantfrom 290-9019
streamedtweets2 <- streamedtweets2[(291:9019),]

write.csv(streamedtweets2,"thefinalscraptdata.csv")




#join it with the emoji
emojiEncodings <- read.csv("emoji-encodings.csv")
tweetTexts<- streamedtweets2$text
results <- sapply(emojiEncodings$streamApiEncodings, regexpr,  tweetTexts ) %>%
  data.frame()
colnames(results) <- emojiEncodings$emojiDescription


write.csv(results,"resultsfinalwhole.csv")


results2 <- read.csv("resultsfinalwhole.csv")
results2<-results2[,(2:843)]


#visualization 1 
# the sentimental analysis for twitters has emoji expression and do not has emoji expression
# filter the ones has the emoji expression

results2[8730,(1:842)] <- colSums(results2[1:842],na.rm = TRUE)

results2[(1:8730),843] <- rowSums(results2,na.rm = TRUE)

#newtylor<- filter(newtylor,V843 == -842)
i<-which(results2$V843>-842)
slected<-streamedtweets2[i,]

head(slected)

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

twitterplotdata1 <- slected %>%
  filter(!str_detect(text, '^RT|^"')) %>%                                      # filter tweets starting w/ "rt"
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%   # remove links
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%                     # parse the text into "tokens"
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))                     # remove stop words

#the score with emoji
nrcScores <- data.frame()

withemojiScores <- get_nrc_sentiment(twitterplotdata1$word) %>%
  summarise_all(funs(mean))
nrcScores <- rbind(nrcScores,withemojiScores)
#the score without emoji

i<-which(results2$V843==-842)
slected2<-streamedtweets2[i,]


twitterplotdata2 <- slected2 %>%
  filter(!str_detect(text, '^RT|^"')) %>%                                      # filter tweets starting w/ "rt"
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%   # remove links
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%                     # parse the text into "tokens"
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))                     # remove stop words

withnoemojiScores <- get_nrc_sentiment(twitterplotdata2$word) %>%
  summarise_all(funs(mean))
nrcScores <- rbind(nrcScores,withnoemojiScores)

nrcScoresback<-nrcScores
#nrcScores<-nrcScoresback
nrcScores$emoji<-"hasemoji"
nrcScores[2,]$emoji<-"noemoji"
tallFormat = gather(nrcScores, key=emotion, value=score, anger:positive)
tallFormat$emoji = "noemoji"
tallFormat[c(1,3,5,7,9,11,13,15,17,19),]$emoji = "emoji"


p <- ggplot(tallFormat,aes(x=emoji, y=score), fill = "black") +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Sentiment") +
  labs(title = "NRC sentiment of restaurant tweets whether have emoji or not")+

scale_fill_manual(name = "", labels = c("",""),
                  values = c( "lightblue","orange"))

p + facet_wrap(~emotion, ncol = 5)




# Comparison 2: emoji tweets with images or no emoji tweets do not have images

# tweets with emoji for pic plot
tweet_picture_counts <- slected %>%
  filter(!str_detect(text, '^"')) %>%
  filter(!str_detect(text, '^RT')) %>%
  count( picture = ifelse(str_detect(text, "t.co"), "Picture/link", "No picture/link"))

ggplot(tweet_picture_counts, aes(picture, n, fill = picture)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "",title="Tweets with emoji")


# tweets with emoji for pic plot


tweet_picture_counts <- slected2 %>%
  filter(!str_detect(text, '^"')) %>%
  filter(!str_detect(text, '^RT')) %>%
  count( picture = ifelse(str_detect(text, "t.co"), "Picture/link", "No picture/link"))

ggplot(tweet_picture_counts, aes(picture, n, fill = picture)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "",title="Tweets with no emoji")

# Comparison 3: word frequency

# to tokenize the tweets into words, we will use a regular expression
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# [^ CHARACTERGROUP ] = Matches any single character that is not in CHARACTERGROUP
# A-Za-z = any letter
# \d     = any numeric digit
# #@     = the # or @ symbols

# [^A-Za-z\\d#@'] = Match any character that is not alphanumeric and is not # or @
# This is how it determines where the breaks are between words


# Stop words - common words that convey little meaning, should be removed
stop_words$word
reg <- "<[a-zA-Z0-9][a-zA-Z0-9]>"
slected3$text <- gsub(reg," ",slected$text)



twitterwordfre_withoutemoji <- slected %>%
  filter(!str_detect(text, '^RT|^"')) %>%                                      # filter tweets starting w/ "rt"
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%   # remove links
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%                     # parse the text into "tokens"
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))                     # remove stop words


install.packages("stringr")
library(stringr)
i<-which(str_length(twitterwordfre_withoutemoji$word)>2)

twitterwordfre_withoutemojiselected<-twitterwordfre_withoutemoji[i,]


twitterwordfre_withoutemojiselected %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip() +
  labs(title = "Word frequency for tweets has emoji ")



# tweets do not has emoji word frequency

# to tokenize the tweets into words, we will use a regular expression
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# [^ CHARACTERGROUP ] = Matches any single character that is not in CHARACTERGROUP
# A-Za-z = any letter
# \d     = any numeric digit
# #@     = the # or @ symbols

# [^A-Za-z\\d#@'] = Match any character that is not alphanumeric and is not # or @
# This is how it determines where the breaks are between words


# Stop words - common words that convey little meaning, should be removed
stop_words$word
reg <- "<[a-zA-Z0-9][a-zA-Z0-9]>"



twitterwordfre_withoutemoji2 <- slected2 %>%
  filter(!str_detect(text, '^RT|^"')) %>%                                      # filter tweets starting w/ "rt"
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%   # remove links
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%                     # parse the text into "tokens"
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))                     # remove stop words


install.packages("stringr")
library(stringr)

           
i<-which(str_length(twitterwordfre_withoutemoji2$word)>3)

twitterwordfre_withoutemojiselected2<-twitterwordfre_withoutemoji2[i,]


twitterwordfre_withoutemojiselected2 %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip() +
  labs(title = "Word frequency for tweets has no emoji ")




#the most frequent emoji

result2backup<-results2
results2<-result2backup
results2<-results2[(1:8729),]
#select the most frequent emoji
results2[results2 == -1] <- 0
results2[results2 > 0] <- 1

results2[8730,(1:842)] <- colSums(results2[1:842],na.rm = TRUE)
emoji_frequency<-results2[8730,]
i<-which(emoji_frequency>0)
emoji_frequency<-emoji_frequency[,i]


emoji_frequency2  <- data.frame(t(emoji_frequency[-1]))

emoji_frequency2 = t(emoji_frequency[-1])
emoji_frequency2<-data.frame(emoji_frequency2)


write.csv(emoji_frequency2,"emoji_frequency.csv")
emoji_frequency3<-read.csv("emoji_frequency.csv")
#try
emoji_frequency3$name_emoji <- as.factor(emoji_frequency3$name_emoji)
emoji_frequency3$emoji <- as.factor(emoji_frequency3$emoji)
i<- which(emoji_frequency3$n>50)

devtools::install_github("dill/emoGG")
install.packages("ggplot2")
library(ggplot2)
library(emoGG)
emoji_search("ASTONISHED.FACE")

emoji_frequency3<-emoji_frequency3[i,]

ggplot(emoji_frequency3, aes(name_emoji, n)) + 
  geom_bar(stat = "identity") +
  coord_flip()+
  labs(title="the most frequent emoji")


#choose the most frequent emoji: face with tears of joy

i<-which(results2$FACE.WITH.TEARS.OF.JOY==1)

frequentemoji<-streamedtweets2[i,]

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

twitterplotdataemoji <- frequentemoji %>%
  filter(!str_detect(text, '^RT|^"')) %>%                                      # filter tweets starting w/ "rt"
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%   # remove links
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%                     # parse the text into "tokens"
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))                     # remove stop words

#the score with emoji-face of tear of joy
nrcScores2 <- data.frame()

emojiscore <- get_nrc_sentiment(twitterplotdataemoji$word) %>%
  summarise_all(funs(mean))
nrcScores2 <- rbind(nrcScores2,emojiscore)


nrcScoresback2<-nrcScores2
#nrcScores<-nrcScoresback
nrcScores2$emoji<-"hasemoji"
tallFormat = gather(nrcScores2, key=emotion, value=score, anger:positive)
tallFormat$emoji = "noemoji"
tallFormat[c(1,3,5,7,9,11,13,15,17,19),]$emoji = "emoji"


ggplot(tallFormat,aes(x=emotion, y=score), fill = "black") +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Sentiment") +
  labs(title = "NRC sentiment of restaurant tweets has emoji of tear of joy")+
  scale_fill_manual(name = "", labels = c("",""),
                    values = c( "lightblue","orange"))




