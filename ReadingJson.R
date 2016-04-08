install.packages("fpc")
install.packages("rjson")
library("rjson")
library("jsonlite")
library("tm")
library("fpc")

# dnld json files
url2<-paste0("http://jsonstudio.com/wp-content/uploads/2014/02/companies.zip")
download.file(url2,dest="companies.zip") # worlbank
unzip("companies.zip")
filename2 <- "companies.json"
jsonData2 <-fromJSON(file=filename2)
