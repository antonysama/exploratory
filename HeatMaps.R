#nextsteps: dendroram with highest SVD

#package to colour dendrograms
install.packages("rafalib")
install.packages("data.table")
install.packages("d3heatmap")
packages<-c("rafalib", "d3heatmap", "data.table")
lapply(packages, require, character.only = TRUE)

#list.files()
#load df
#data("mtcars")
#df<-mtcars 
df<-read.csv("mtcars.csv")
#key to create rownames
row.names(df)<-df$communities
df$communities=NULL
df$rn=NULL
df<-df[order(rownames(df)),]
d3heatmap(df, scale = "column", colors = "Blues",
          dendrogram = "none", Rowv = FALSE, Colv = FALSE)

#find distance, and hclust
#hc<-hclust
dev.off()
names(df)[2:16]<-c("popln", "pcnt.popln", "age", "child", "educ", "incm", "bldg", "prjct","non.ress", "res.ress", "non.rt", "res.rt", "s", "m", "l")
hc<-hclust(dist(df))        
#plot(hc)
myplclust(hc, labels=hc$labels, lab.col = df$incm)


#SVD
class(mtcars)
df<-as.matrix(df) 
#datamatrix ordered
df<-df[order(rownames(df)),] 
 # check if df needs to be a matrix

#Patterns in rows and columns
par(mfrow=c(1,3))
image(t(df)[,nrow(df):1])
plot(rowMeans(df),46:1,,xlab="Row Mean",ylab="Row",pch=19)
plot(colMeans(df),xlab="Column",ylab="Column Mean",pch=19)

#Components of the SVD - $u$ and $v$
#library("rafalib")
##impute for nas
indx <- which(is.na(df), arr.ind = TRUE)
df[indx] <- colMeans(df, na.rm = TRUE)[indx[,"col"]]
svd1 <- svd(scale(df))
par(mfrow=c(1,3))
image(t(df)[,nrow(df):1])
plot(svd1$u[,1],46:1,,xlab="Row",ylab="First left singular vector",pch=19)
plot(svd1$v[,1],xlab="Column",ylab="First right singular vector",pch=19)

#Components of the SVD - Variance explained
par(mfrow=c(1,2))
plot(svd1$d,xlab="Column",ylab="Singular value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",
     ylab="Prop. of variance explained",pch=19)

#Relation to PCA
pca1 <- prcomp(df,scale=TRUE)
plot(pca1$rotation[,1],svd1$v[,1],pch=19,xlab="Principal Component 1",ylab="Right Singular Vector 1")
abline(c(0,1))

#max contribuor to variance
plot(svd1$v[,2],pch=19)
maxContrib <-which.max(svd1$v[,2])
#new cluster with maxContributer
distanceMatrix <- dist(df[,5])
distanceMatrix<-distanceMatrix[order(rownames(distanceMatrix)),]
hmax<-hclust(distanceMatrix)
plot(hmax)
myplclust(hmax, labels=hmax$labels, lab.col = df$child)
myplclust(hmax, labels=hmax$labels, lab.col = rep(1,length(df$child)))
d3heatmap(distanceMatrix, scale = "column", colors = "Blues",
           Rowv = FALSE, Colv = FALSE)

#impute
library("impute")
df<-read.csv("mtcars.csv")
#datamatrix ordered
df<-df[order(rownames(df)),]
df<-as.matrix(df)
#df[10,10]<- NA#didnt work
#impute.knn(df, k = 12, rowmax=0.8, colmax = 0.9)#didnt work
svd1 <- svd(scale(df))
indx <- which(is.na(df), arr.ind = TRUE)
df[indx] <- colMeans(df, na.rm = TRUE)[indx[,"col"]]
svd1 <- svd(scale(df))
svd2 <- svd(scale(df2))
par(mfrow=c(1,2))
plot(svd1$v[,1],pch=19)
plot(svd2$v[,1],pch=19)
maxContrib1 <-which.max(svd1$v[,1])
maxContrib2 <-which.max(svd2$v[,3])


