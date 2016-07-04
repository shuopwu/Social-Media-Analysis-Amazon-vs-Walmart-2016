rm(list=ls())
library(DBI)
library(RMySQL)
library(NLP)
library(tm)
library(e1071)
library(SparseM)
library(maxent)
library(topicmodels)
library(RColorBrewer)
library(wordcloud)
library(psych)

#Shuopeng Wu
############ choose the tweets with keywords associated with "wal-mart" or "amazon" ############
driver <- dbDriver("MySQL")

myhost <- "rsmysql.simonnet.rochester.edu"
mydb <- "studb"
myacct <- "swu42"
mypwd <- "29514984" 

conn <- dbConnect(driver, host=myhost, dbname=mydb, myacct, mypwd)

kids="(129,146,412,1027,1155,1119,394,534,1040,193)"
mydata <- dbGetQuery(conn, paste("SELECT keyword, tcreated, tweet, username, ulocation, follower, following, totaltweets FROM final where kid in", kids))
mydata = unique(mydata)
write.csv(mydata, file = "final.csv",row.names=FALSE)

dbDisconnect(conn)

############ choose the tweets around amazon during Thanksgiving of 2015-11 ############
#Levels: @amazon @walmart #amazon #walmart amazon amazon's amazondeals walmart walmart's

mydata=read.csv("final.csv",stringsAsFactors=FALSE)
amazon = mydata[which(mydata$keyword=="amazondeals'" | mydata$keyword=="#amazon" | mydata$keyword=="amazon" | mydata$keyword=="amazon's"| mydata$keyword=="@amazon"),]
walmart = mydata[which(mydata$keyword==" @walmart" | mydata$keyword=="#walmart" | mydata$keyword=="walmart" | mydata$keyword=="walmart's"),]

amazon$tcreated2=as.Date(amazon$tcreated,format= "%Y-%m-%d")
walmart$tcreated2=as.Date(walmart$tcreated,format= "%Y-%m-%d")

amazon=amazon[(amazon$tcreated2>= "2015-11-20" & amazon$tcreated2 <= "2015-11-30"),]
amazon = na.omit(amazon)

walmart=walmart[(walmart$tcreated2>= "2015-11-20" & walmart$tcreated2 <= "2015-12-3"),]
walmart = na.omit(walmart)

old.par <- par(mar=c(5,5,7,3))




############ tweets per day in Thanksgiving ############
par(mfrow=c(2,1))
responded = table(amazon$tcreated2)
barplot(responded,main = 'Number of amazon Tweets Per Day',xlab='day of the month',ylab="num-tweets",
        col = c("darkblue","red"),beside=TRUE)

responded = table(walmart$tcreated2)
barplot(responded,main = 'Number of walmart Tweets Per Day',xlab='day of the month',ylab="num-tweets",
        col = c("darkblue","red"),beside=TRUE)
par(old.par)
dev.off()

print("Amazon tweets per day in Thanksgiving")
print(summary(as.factor(amazon$tcreated2)))
print("Walmart tweets per day in Thanksgiving")
print(summary(as.factor(walmart$tcreated2)))






############ tweets hourly check ############
par(mfrow=c(2,1))
amazon$hour=format(as.POSIXct(amazon$tcreated,format="%Y-%m-%d %H:%M:%S"), format="%H")
walmart$hour=format(as.POSIXct(walmart$tcreated,format="%Y-%m-%d %H:%M:%S"), format="%H")

plot(table(amazon$hour),type="b",main="Distribution of amazon Tweets per Hour of a Day",
     xlab = "hour of the day", ylab = "number of tweets",col="darkblue")
plot(table(walmart$hour),type="b",main="Distribution of walmart Tweets per Hour of a Day",
     xlab = "hour of the day", ylab = "number of tweets",col="darkblue")

par(old.par)
dev.off()






############ Percentage of tweets with different number of followers ############
par(mfrow=c(1,2),las=2,mar=c(9,3,3,3))

amazon$followersgroup[amazon$follower <=100] = "seldom"
amazon$followersgroup[amazon$follower > 100 & amazon$follower <= 1000] = "more than seldom"
amazon$followersgroup[amazon$follower > 1000 & amazon$follower <= 5000] = "less than normal"
amazon$followersgroup[amazon$follower > 5000 & amazon$follower <= 10000] = "normal"
amazon$followersgroup[amazon$follower > 10000 & amazon$follower <= 50000] = "more than normal"
amazon$followersgroup[amazon$follower > 50000] = "popular"
freqtab1 <- table(amazon$followersgroup)
proptab1 <- prop.table(freqtab1) 
barplot(proptab1[1:10], main="amazon prob forGroups", ylim=c(0,0.3),col="blue")

walmart$followersgroup[walmart$follower <=100] = "seldom"
walmart$followersgroup[walmart$follower > 100 & walmart$follower <= 1000] = "more than seldom"
walmart$followersgroup[walmart$follower > 1000 & walmart$follower <= 5000] = "less than normal"
walmart$followersgroup[walmart$follower > 5000 & walmart$follower <= 10000] = "normal"
walmart$followersgroup[walmart$follower > 10000 & walmart$follower <= 50000] = "more than normal"
walmart$followersgroup[walmart$follower > 50000] = "popular"
freqtab2 <- table(walmart$followersgroup)
proptab2 <- prop.table(freqtab2)   
barplot(proptab2[1:10], main="walmart prob for Groups", ylim=c(0,0.3),col="green")

par(old.par)
dev.off()

print("Amazon Percentage of tweets with different number of follower")
print(freqtab1)
print("Walmart Percentage of tweets with different number of follower")
print(freqtab2)






############ location attention analysis ############
mystopwords = c("usa", "united","england","canada","london",",italia", "mexico","australia","worldwide","earth", "everywhere", 
                "global", "world", "city","internet","almost","blog","somewhere","planet","international",
                "around","states","behind","wide","area","dreams","anonymous","check")

amazonloc = as.matrix(amazon$ulocation)
amazonloc[amazonloc==""] = NA
amazonloc = na.omit(amazonloc)
amazonloclocdoc = Corpus(DataframeSource(amazonloc))
isdtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"),stopwords("spanish"),mystopwords),stemming=F,weighting=weightTfIdf)
isdtm = DocumentTermMatrix(amazonloclocdoc, control=isdtm.control)
isdtm = removeSparseTerms(isdtm,0.999)
freq = colSums( as.matrix(isdtm) )
freq.sorted = sort( freq, decreasing=TRUE )
print(freq.sorted[1:50])
wordcloud(names(freq), freq, max.words=100, colors=brewer.pal(6,"Dark2"))


walmartloc = as.matrix(walmart$ulocation)
walmartloc[walmartloc==""] = NA
walmartloc = na.omit(walmartloc)
walmartlocdoc = Corpus(DataframeSource(walmartloc))

isdtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"),stopwords("spanish"),mystopwords),stemming=F,weighting=weightTfIdf)
isdtm = DocumentTermMatrix(walmartlocdoc, control=isdtm.control)
isdtm = removeSparseTerms(isdtm,0.999)
freq = colSums( as.matrix(isdtm) )
freq.sorted = sort( freq, decreasing=TRUE )
print(freq.sorted[1:50])
wordcloud(names(freq), freq, max.words=100, colors=brewer.pal(6,"Dark2"))






############ topic model analysis Amazon ############

amazontweet=amazon$tweet
for (i in 1:length(amazontweet))
{
  dat <- amazontweet[i]
  dat2 <- unlist(strsplit(dat, split=" "))
  dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
  if (length(dat3) | length(grep("http",enc2utf8(dat),ignore.case=TRUE)))
  {
    amazontweet[i] <- NA     
  }
}
amazon$tweet=amazontweet
amazon2=amazon[!is.na(amazon$tweet),]
istweetdoc <- Corpus(DataframeSource(as.matrix(amazon2$tweet)))
mystopwords=c("amazondeals'","#amazon" ,"amazon" ,"amazon's","@amazon")
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"),stopwords("spanish"),mystopwords),stemming=F)
dtm = DocumentTermMatrix(istweetdoc, control=dtm.control)
dtm = removeSparseTerms(dtm,0.999)
idx <- rowSums(as.matrix(dtm))>0
newdocs <- istweetdoc[idx]
dtm <- dtm[idx,]
freq = colSums( as.matrix(dtm) )

############   the LDA model for amazon  ############
lda.model <- LDA(dtm, 10) 
freqterms=terms(lda.model,15) 
print ("15 most freqterms for 10 topics in amazon")
print (freqterms)

############ distribution of the term for each topic of amazon ############
myposterior=posterior(lda.model)
topics=myposterior$topics
terms=myposterior$terms
plot(terms[1,],main="Term distribution Among Different Topics",
     xlab = "index of terms",ylab = "probability", type="l")

############ daily monitor each topic of amazon ############

istweetdoc <- Corpus(DataframeSource(as.matrix(amazon2[amazon2$tcreated2=="2015-11-26",]$tweet)))
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"),stopwords("spanish"),mystopwords),stemming=F)
dtm = DocumentTermMatrix(istweetdoc, control=dtm.control)
dtm = removeSparseTerms(dtm,0.999)
idx <- rowSums(as.matrix(dtm))>0
newdocs <- istweetdoc[idx]
dtm <- dtm[idx,]
freq = colSums( as.matrix(dtm) )
wordcloud(names(freq), freq, max.words=100, colors=brewer.pal(6,"Dark2"))


istweetdoc <- Corpus(DataframeSource(as.matrix(amazon2[amazon2$tcreated2=="2015-11-27",]$tweet)))
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"),stopwords("spanish"),mystopwords),stemming=F)
dtm = DocumentTermMatrix(istweetdoc, control=dtm.control)
dtm = removeSparseTerms(dtm,0.999)
idx <- rowSums(as.matrix(dtm))>0
newdocs <- istweetdoc[idx]
dtm <- dtm[idx,]
freq = colSums( as.matrix(dtm) )
wordcloud(names(freq), freq, max.words=100, colors=brewer.pal(6,"Dark2"))


istweetdoc <- Corpus(DataframeSource(as.matrix(amazon2[amazon2$tcreated2=="2015-11-28",]$tweet)))
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"),stopwords("spanish"),mystopwords),stemming=F)
dtm = DocumentTermMatrix(istweetdoc, control=dtm.control)
dtm = removeSparseTerms(dtm,0.999)
idx <- rowSums(as.matrix(dtm))>0
newdocs <- istweetdoc[idx]
dtm <- dtm[idx,]
freq = colSums( as.matrix(dtm) )
wordcloud(names(freq), freq, max.words=100, colors=brewer.pal(6,"Dark2"))

istweetdoc <- Corpus(DataframeSource(as.matrix(amazon2[amazon2$tcreated2=="2015-11-29",]$tweet)))
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"),stopwords("spanish"),mystopwords),stemming=F)
dtm = DocumentTermMatrix(istweetdoc, control=dtm.control)
dtm = removeSparseTerms(dtm,0.999)
idx <- rowSums(as.matrix(dtm))>0
newdocs <- istweetdoc[idx]
dtm <- dtm[idx,]
freq = colSums( as.matrix(dtm) )
wordcloud(names(freq), freq, max.words=100, colors=brewer.pal(6,"Dark2"))











############ topic model analysis Walmart ############

walmarttweet=walmart$tweet
for (i in 1:length(walmarttweet))
{
  dat <- walmarttweet[i]
  dat2 <- unlist(strsplit(dat, split=" "))
  dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
  if (length(dat3) | length(grep("http",enc2utf8(dat),ignore.case=TRUE)))
  {
    walmarttweet[i] <- NA     
  }
}
walmart$tweet=walmarttweet
walmart2=walmart[!is.na(walmart$tweet),]
istweetdoc <- Corpus(DataframeSource(as.matrix(walmart2$tweet)))
mystopwords= c("#walmart","walmart","walmart's")
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"),stopwords("spanish"),mystopwords),stemming=F)
dtm = DocumentTermMatrix(istweetdoc, control=dtm.control)
dtm = removeSparseTerms(dtm,0.999)
idx <- rowSums(as.matrix(dtm))>0
newdocs <- istweetdoc[idx]
dtm <- dtm[idx,]
freq = colSums( as.matrix(dtm) )
wordcloud(names(freq), freq, max.words=100, colors=brewer.pal(6,"Dark2"))

############   the LDA model  for walmart ############
lda.model <- LDA(dtm, 10) 
freqterms=terms(lda.model,15) 
print ("15 most freqterms for 10 topics in walmart")
print (freqterms)

############ distribution of the term for each topic of walmart ############
myposterior=posterior(lda.model)
topics=myposterior$topics
terms=myposterior$terms
plot(terms[1,],main="Term distribution Among Different Topics",
     xlab = "index of terms",ylab = "probability", type="l")






