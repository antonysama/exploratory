#reference program
# https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html#hierarchal-clustering
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
docs <- Corpus(DirSource(cname)
filename2 <- "companies.json"
Data2 <-fromJSON(file=filename2)
#Data2<-unlist(jsonData2)

#write csv
write.csv(Data2, file="Data2.csv")
docs <- Corpus("Data2.csv")
Data2<-read.csv("Data2.csv")

#cluster
plot(hclust(dist(Data2[,2]), method =  "ward.D2"))
#knn
d<-dist(t(Data2))   
kfit<- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#tm
summary(Data2)
docs <- Corpus(Data2) 
docs <- tm_map(Data2, removeNumbers) 
