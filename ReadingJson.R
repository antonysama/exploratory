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
url2<-paste0("http://jsonstudio.com/wp-content/uploads/2014/02/companies.zip")
download.file(url2,dest="companies.zip") 
unzip("companies.zip")
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
dtm   
inspect(dtm[1, 1:10])
dim(dtm)
#Iinvs matrix
tdm <- TermDocumentMatrix(docs)  
#  Start by removing sparse terms:   
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum. 
#word freq
freq[head(ord)]
freq[tail(ord)]
freq <- colSums(as.matrix(dtms))   
freq 