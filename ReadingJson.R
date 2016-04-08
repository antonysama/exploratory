install.packages("fpc")
install.packages("rjson")
library("rjson")
library("jsonlite")
library("tm")
library("fpc")

# dnld json files
url1<-paste0("https://api.github.com/users/jtleek/repos")
filename1<- readLines(url1, warn = "F")
jsonData1 <- fromJSON(file=filename1)
url2<-
  paste0("http://jsonstudio.com/wp-content/uploads/2014/02/enron.zip")
download.file(url2,dest="data.zip") # worlbank
unzip ("data.zip")
 

str(jsonData1)
names(jsonData1)
#filename2 <- "world_bank.json"
#jsonData2 <- fromJSON(file=filename2)


#creating a char vector
inspect(jsonData1)

docs <- tm_map(docs, removePunctuation)  

dtm <- DocumentTermMatrix(docs) 
