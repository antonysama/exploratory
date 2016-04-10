#reference program
# https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html#hierarchal-clustering
install.packages("SnowballC")
library("jsonlite")
library("rjson")
library("rafalib")
library("cluster")
library("fpc")
library("tm")

# create json files
json = '[{"name":"John","age":20, "sex": "M"}, {"name":"Martin","age":30, "sex":"t"},{"name":"Mrin","age":50, "sex":"F"}]]'
Data2 = unlist(fromJSON(json))
# dwnld json files
url2<-paste0("http://jsonstudio.com/wp-content/uploads/2014/02/world_bank.zip")
download.file(url2,dest="world_bank.zip") 
unzip("world_bank.zip")
# Create a file named "texts" where you'll keep your data.
# Save the file to a particular place
#save the folder to your C: drive and use the following code chunk:
cname <- file.path("C:", "texts")   
cname   
dir(cname) 
library("tm")
docs <- Corpus(DirSource(cname))
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument) 
dtm <- DocumentTermMatrix(docs)   
#  Start by removing sparse terms:   
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is 15% empty space, maximum. 
#hclust
library("cluster")   
library("rafalib")
d <- dist(t(dtmss))   
reduced <- dtmss[sample(nrow(dtmss),80),]
hc<-hclust(dist(t(dtmss)))  
plot(hc, hang=-1)
#knn
library("fpc")   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(dist(t(dtmss)), centers= 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  
