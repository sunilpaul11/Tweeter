install.packages("RColorBrewer")
install.packages('base64enc')
install.packages("SnowballC")
install.packages("dismo")
#connect all libraries

library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(base64enc)
library(dismo)


#connect to API
######  Authentication Setup Tweeter #####
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem") #downloads the certificate


consumer_key <- 'xxxx'
consumer_secret <- 'xxxx'
access_token <- 'xxxx'
access_secret <- 'xxxx'



setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

cred <- OAuthFactory$new(consumerKey=consumer_key, 
                         consumerSecret=consumer_secret,
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')



cred$handshake(cainfo="cacert.pem")
#save(cred, file='twitter authentication.Rdata')
#load('twitter authentication.Rdata') #Once you launch the code first time, you can start from this line in the future (libraries should be connected)
# registerTwitterOAuth(Cred)
#################################################################

cancertweet <- NULL
z<- c('#Cancer + #United States','#Cancer + #Northeast Region','#Cancer + #Midwest Region','#Cancer + #South Region','#Cancer + #West Region','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Puerto Rico','#Cancer + #United States','#Cancer + #Northeast Region','#Cancer + #Midwest Region','#Cancer + #South Region','#Cancer + #West Region','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Puerto Rico','#Cancer + #United States','#Cancer + #Northeast Region','#Cancer + #Midwest Region','#Cancer + #South Region','#Cancer + #West Region','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Puerto Rico','#Cancer + #United States','#Cancer + #Northeast Region','#Cancer + #Midwest Region','#Cancer + #South Region','#Cancer + #West Region','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Puerto Rico','#Cancer + #United States','#Cancer + #Northeast Region','#Cancer + #Midwest Region','#Cancer + #South Region','#Cancer + #West Region','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Puerto Rico','#Cancer + #United States','#Cancer + #Northeast Region','#Cancer + #Midwest Region','#Cancer + #South Region','#Cancer + #West Region','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Puerto Rico','#Cancer + #United States','#Cancer + #Northeast Region','#Cancer + #Midwest Region','#Cancer + #South Region','#Cancer + #West Region','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Puerto Rico','#Cancer + #United States','#Cancer + #Northeast Region','#Cancer + #Midwest Region','#Cancer + #South Region','#Cancer + #West Region','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Puerto Rico','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #American Samoa','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Guam','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Northern Mariana Islands','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Puerto Rico','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #U.S. Virgin Islands','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #American Samoa','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Guam','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Northern Mariana Islands','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Puerto Rico','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #U.S. Virgin Islands','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #American Samoa','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Guam','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Northern Mariana Islands','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Puerto Rico','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #U.S. Virgin Islands','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #American Samoa','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Guam','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Northern Mariana Islands','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Puerto Rico','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #U.S. Virgin Islands','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #American Samoa','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Guam','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Northern Mariana Islands','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Puerto Rico','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #U.S. Virgin Islands','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #American Samoa','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Guam','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Northern Mariana Islands','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Puerto Rico','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #U.S. Virgin Islands','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #American Samoa','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Guam','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Northern Mariana Islands','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Puerto Rico','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #U.S. Virgin Islands','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #American Samoa','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Guam','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Northern Mariana Islands','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Puerto Rico','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #U.S. Virgin Islands','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #American Samoa','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Guam','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Northern Mariana Islands','#Cancer + #Ohio','#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Puerto Rico','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #U.S. Virgin Islands','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming','#Cancer + #Alabama','#Cancer + #Alaska','#Cancer + #American Samoa','#Cancer + #Arizona','#Cancer + #Arkansas','#Cancer + #California','#Cancer + #Colorado','#Cancer + #Connecticut','#Cancer + #Delaware','#Cancer + #District of Columbia','#Cancer + #Florida','#Cancer + #Georgia','#Cancer + #Guam','#Cancer + #Hawaii','#Cancer + #Idaho','#Cancer + #Illinois','#Cancer + #Indiana','#Cancer + #Iowa','#Cancer + #Kansas','#Cancer + #Kentucky','#Cancer + #Louisiana','#Cancer + #Maine','#Cancer + #Maryland','#Cancer + #Massachusetts','#Cancer + #Michigan','#Cancer + #Minnesota','#Cancer + #Mississippi','#Cancer + #Missouri','#Cancer + #Montana','#Cancer + #Nebraska','#Cancer + #Nevada','#Cancer + #New Hampshire','#Cancer + #New Jersey','#Cancer + #New Mexico','#Cancer + #New York','#Cancer + #North Carolina','#Cancer + #North Dakota','#Cancer + #Northern Mariana Islands','#Cancer + #Ohio',
      '#Cancer + #Oklahoma','#Cancer + #Oregon','#Cancer + #Pennsylvania','#Cancer + #Puerto Rico','#Cancer + #Rhode Island','#Cancer + #South Carolina','#Cancer + #South Dakota','#Cancer + #Tennessee','#Cancer + #Texas','#Cancer + #U.S. Virgin Islands','#Cancer + #Utah','#Cancer + #Vermont','#Cancer + #Virginia','#Cancer + #Washington','#Cancer + #West Virginia','#Cancer + #Wisconsin','#Cancer + #Wyoming'
)
for (x in z) 
  {
  cancertweet <- searchTwitter(x, n=1, since = '2017-11-08', retryOnRateLimit = 1e3, lang='en')
  }

Location <- twListToDF(cancertweet)
tweetFrame <- twListToDF(cancertweet)
userInfo <- lookupUsers(tweetFrame$screenName)  # Batch lookup of user info
userFrame <- twListToDF(userInfo)  # Convert to a nice dF

locatedUsers <- !is.na(userFrame$location)  # Keep only users with location info

locations <- geocode(userFrame$location[locatedUsers])  # Use amazing API to guess
write.csv(locations, "TweetLocation.csv")
#####################################

tweets_geolocated.df <- twListToDF(cancertweet)
cancertweet <- do.call("rbind", lapply(cancertweet, as.data.frame)) 
head(cancertweet)

cancertweet$text <- sapply(cancertweet$text,function(row) iconv(row, "latin1", "ASCII", sub="")) #remove emoticon
cancertweet$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", cancertweet$text) #remove URL
sample <- cancertweet$text

pos = scan('positive-words.txt', what='character', comment.char=';')
neg = scan('negative-words.txt', what='character', comment.char=';')
pos=c(pos, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader')
neg = c(neg, 'Stage4', 'Stage 4', 'Stage3', 'Stage 3', 'palliative', 'hospice')

##################################################################
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  list=lapply(sentences, function(sentence, pos.words, neg.words)
  {
    sentence = gsub('[[:punct:]]',' ',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)  #removes decimal number
    sentence = gsub('\n','',sentence)    #removes new lines
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)  #changes a list to character vector
    pos.matches = match(words, pos)
    neg.matches = match(words, neg)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp = sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1 = c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  score_new = lapply(list, `[[`, 1)
  pp1 = lapply(list, `[[`, 2)
  nn1 = lapply(list, `[[`, 3)
  
  scores.df = data.frame(score = score_new, text=sentences)
  positive.df = data.frame(Positive = pp1, text=sentences)
  negative.df = data.frame(Negative = nn1, text=sentences)
  
  list_df = list(scores.df, positive.df, negative.df)
  return(list_df)
}

result = score.sentiment(sample, pos.words, neg.words)

library(reshape)
test1=result[[1]]
test2=result[[2]]
test3=result[[3]]

#Creating three different data frames for Score, Positive and Negative
#Removing text column from data frame
test1$text=NULL
test2$text=NULL
test3$text=NULL
#Storing the first row(Containing the sentiment scores) in variable q
q1=test1[1,]
q2=test2[1,]
q3=test3[1,]
qq1=melt(q1, ,var='Score')
qq2=melt(q2, ,var='Positive')
qq3=melt(q3, ,var='Negative') 
qq1['Score'] = NULL
qq2['Positive'] = NULL
qq3['Negative'] = NULL
#Creating data frame
table1 = data.frame(Text=result[[1]]$text, Score=qq1)
table2 = data.frame(Text=result[[2]]$text, Score=qq2)
table3 = data.frame(Text=result[[3]]$text, Score=qq3)

#Merging three data frames into one
table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)
head(table_final)
# write.csv(table_final, file = "MyData.csv")

#Making percentage columns

#Positive Percentage

#Renaming
posSc=table_final$Positive
negSc=table_final$Negative

#Adding column
table_final$PosPercent = posSc/ (posSc+negSc)

#Replacing Nan with zero
pp = table_final$PosPercent
pp[is.nan(pp)] <- 0
table_final$PosPercent = pp

#Negative Percentage

#Adding column
table_final$NegPercent = negSc/ (posSc+negSc)

#Replacing Nan with zero
nn = table_final$NegPercent
nn[is.nan(nn)] <- 0
table_final$NegPercent = nn

write.csv(table_final, file = "Tweet.csv")
table_final


#Histogram
hist(table_final$Positive, col=rainbow(10))
hist(table_final$Negative, col=rainbow(10))
hist(table_final$Score, col=rainbow(10))

#Pie
slices <- c(sum(table_final$Positive), sum(table_final$Negative))
labels <- c("Positive", "Negative")
library(plotrix)
#pie(slices, labels = labels, col=rainbow(length(labels)), main="Sentiment Analysis")
pie3D(slices, labels = labels, col=rainbow(length(labels)),explode=0.0, main="Sentiment Analysis")
