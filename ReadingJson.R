#reference program
# https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html#hierarchal-clustering
install.packages("SnowballC")

# either create & save json files
require("RJSONIO")
data("crude")
json<-toJSON(crude, pretty = T)
cat(json, file="C:/texts/crude.json")

json = '[{"name":"John","age":20, "sex": "M"}, {"name":"Martin","age":30, "sex":"t"},{"name":"Mrin","age":50, "sex":"F"}]]'
cat(json, file="C:/texts/cat.json")
# create & save another json file
mylist <- list(a=1:5, b=letters, c="character string here")
mylist_json <- toJSON(mylist)
cat(mylist_json, file="C:/texts/mylist_json.json")
#create a 3rd json file
json3 = '[{"name":"Jean","age":23, "sex": "M"}, {"name":"Min","age":40, "sex":"t"},{"name":"Mr","age":54, "sex":"F"}]]'
cat(json, file="C:/texts/cat3.json")

# or dwnld json files
url<-paste0("http://json.org/example.html")
download.file(url,dest="xampl.json") 
unzip("enron.zip")
# Create a file named "texts" where you'll keep your data.
# Save the file to a particular place
#save the folder to your C: drive and use the following code chunk:
cname <- file.path("C:", "texts")   
dir(cname) 

#now the txt management
library("tm")
docs <- Corpus(DirSource(cname))
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument) 

#create dtm and tdm
dtm <- DocumentTermMatrix(docs) 
tdm <- TermDocumentMatrix(docs)  

#removing sparse terms:  
tdmss <- removeSparseTerms(tdm, 0.15) # This makes a matrix that is 15% empty space, maximum. 
dtmss <- removeSparseTerms(dtm, 0.15)

#hclust
#reduced <- tdmss[sample(1:nrow(tdmss),nrow(tdmss)/5),]
d <- dist(t(tdmss), method = "euclidian")
hc<-hclust(d, method="single")
plot(hc, hang = -1)
rect.hclust(hc,k=5)
library("rafalib", lib.loc="C:/R-3.2.4/library")
myplclust(hc, labels=hc$labels)
dev.off()

#knn
library("cluster")
d <- dist(t(dtmss), method = "euclidian")   
kfit <- kmeans(dist(dtmss), centers=5)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 
clara(as.matrix(d), 4, samples = 50)
clarax

dev.off()

# Levenshtein Distance
d  <- adist(dtmss)
rownames(d) <- dtmss
hc <- hclust(as.dist(d), method="single")
plot(hc)
rect.hclust(hc,k=5)
df <- data.frame(reduced,cutree(hc,k=5))
dev.off()
