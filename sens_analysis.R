
library('dplyr')
library('tidyr')
library('ggplot2')
library('tm')
library('wordcloud2')
library('syuzhet')
library('class')
library('e1071')

air=read.csv('air.csv')

#1
View(air)
dim(air)
##there are 5000 obs(rows) and 16 columns

#2-plot that can represent for each airline total number of tweets
air%>%
  group_by(airline)%>%
  summarise(Ntweets=n())%>%
  ggplot(aes(x=airline,y=Ntweets))+geom_bar(stat='identity', fill='purple')+theme_bw()

#3remove columns: negativereason_gold, airline_sentiment_gold, negativereason_confidence, negativereason and airline_sentiment_confidence.
air3=air[,-c(4,5,6,8,10)]
dim(air3)
names(air3)
View(air3)

#4find total unique airlines present in the dataset
n_airline=length(unique(air3$airline))
n_airline #six airlines are presented

airlines=unique(air3$airline) #here they are
airlines

#5
#NAs in each column
NAs=sapply(air3, function(y) sum(length(which(is.na(y)))))
NAs
#total number of NAs
Total_NA=sum(is.na(air3))
Total_NA

#6
#from my point of view, the first column is does not give any information,
#also tweet_coord and tweet_location are very similar variables and tweet_coord could 
#be dropped since it has 4642 NA out of 5000 rows. In addition, user_timezone is not needed
#when tweet_created givesexact time when tweet was created and it does not have any NA.

#7
air7=as.data.frame(air[,-c(1,8,11)])
class(air7)
View(air7)

#8
sentiment_graph=air7%>%
  group_by(airline,airline_sentiment)%>%
  summarise(counts=n())%>%
  ggplot(aes(x=airline, y=counts,fill=airline_sentiment))+geom_bar(stat='identity',position = 'dodge')+theme_bw()

sentiment_graph


#before next task prepwork shoul be done

text=iconv(air7$text,to='utf-8','utf-8',sub='')
corpus=Corpus(VectorSource(text))
##cleaning corpus
removeurl=function(x)
gsub('https://[[:alnum:]|[:punct:]]*', '', x)
corpus_nourl=tm_map(corpus,content_transformer(removeurl))
inspect(corpus_nourl[1:5])

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

airlines_low=sapply(unique(air7$airline),tolower)
airlines_low_nowhitespace=gsub(' ','',airlines_low)
clean_dt=tm_map(cleanset, removeWords,c(airlines_low,airlines_low_nowhitespace))
clean_dt=tm_map(clean_dt,stripWhitespace)
inspect(cleanset[1:5])

dt=DocumentTermMatrix(clean_dt)
inspect(dt)
dt=as.data.frame(as.matrix(dt))
dt$AAirline=air7$airline

word_count=dt%>%
  group_by(AAirline)%>%
  summarise_all(sum)
word_count

gathered_words=gather(word_count, 'word', 'amount',2:ncol(word_count))
View(gathered_words)



#9Word Cloud of tweets for each airline separately
airlines

set.seed(2)
wordcloud_Us_Airways=gathered_words%>%
  filter(AAirline=='US Airways')%>%
  select(word, amount)%>%
  wordcloud2(size = 0.6, shape = 'square',rotateRatio = 0.5,minSize = 10)
wordcloud_Us_Airways


set.seed(2)
wordcloud_Southwest=gathered_words%>%
  filter(AAirline=='Southwest')%>%
  select(word, amount)%>%
  wordcloud2(size = 0.6, shape = 'square',rotateRatio = 0.5,minSize = 10)
wordcloud_Southwest

set.seed(2)
wordcloud_Virgin_America=gathered_words%>%
  filter(AAirline=='Virgin America')%>%
  select(word, amount)%>%
  wordcloud2(size = 0.6, shape = 'square',rotateRatio = 0.5,minSize = 10)
wordcloud_Virgin_America

set.seed(2)
wordcloud_United=gathered_words%>%
  filter(AAirline=='United')%>%
  select(word, amount)%>%
  wordcloud2(size = 0.6, shape = 'square',rotateRatio = 0.5,minSize = 10)
wordcloud_United

set.seed(2)
wordcloud_American=gathered_words%>%
  filter(AAirline=='American')%>%
  select(word, amount)%>%
  wordcloud2(size = 0.6, shape = 'square',rotateRatio = 0.5,minSize = 10)
wordcloud_American

set.seed(2)
wordcloud_Delta=gathered_words%>%
  filter(AAirline=='Delta')%>%
  select(word, amount)%>%
  wordcloud2(size = 0.6, shape = 'square',rotateRatio = 0.5,minSize = 10)
wordcloud_Delta

#10Divide the dataset into 70 % for training and 30 % for testing 

dt_knn=as.data.frame(data.matrix(dt), stringsAsfactors=F)
actual_val=air7$airline_sentiment
data_split=round(nrow(dt_knn)/100*70)
rows=sample(nrow(air7),data_split)
train=dt_knn[rows,]
test=dt_knn[-rows,]

#11creating knn model
pred=knn(train, test, actual_val[rows])

#12
confidence_table=table(predictions=pred, actual=actual_val[-rows])
confidence_table
#13
accuracy=sum(diag(confidence_table))/nrow(test)*100
accuracy
