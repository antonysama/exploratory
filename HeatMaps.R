#package to colour dendrograms
install.packages("rafalib")
library("rafalib")
#load df
data("mtcars")
df<-mtcars   
str(df)
class(df)
df<-transform(df, df[,c(2,3,4)] <- factor(df[,c(2,3,4,5)]))
df<-df[order(rownames(df)),] 
#find distance, and hcluct
hc<-hclust(dist(df[,c(2,3,4,5)])
hc<-hclust(dist(df))        
plot(hc)
myplclust(hc, labels = hc$labels, lab.col = unclass(df[,c(4)]),
          hang = 0.1, xlab = "", sub = "")
myplclust(hc,labels = hc$labels,
          lab.col = rep(1,length(hc$labels),
                        hang = 0.1, xlab = "", sub = ""))

#picking datapackage
data("npk")
df<-npk     
str(df)
class(df)
#make matrix, if not yet one , and sort rownames
df<-as.numeric(df)
df<-as.matrix(df)
#heatmap
heatmap(df(c[2:5]), Rowv=FALSE)

#SVD
#load df
data("mtcars")
df<-mtcars 
#datamatrix ordered
df<-df[order(rownames(df)),] 
df<-as.matrix(df)  # check if df needs to be a matrix

#Patterns in rows and columns
image(t(df)[,nrow(df):1])
plot(rowMeans(df),32:1,,xlab="Row Mean",ylab="Row",pch=19)
plot(colMeans(df),xlab="Column",ylab="Column Mean",pch=19)
#Components of the SVD - $u$ and $v$

#Components of the SVD - $u$ and $v$
svd1 <- svd(scale(df))
par(mfrow=c(1,3))
image(t(df)[,nrow(df):1])
plot(svd1$u[,1],32:1,,xlab="Row",ylab="First left singular vector",pch=19)
plot(svd1$v[,1],xlab="Column",ylab="First right singular vector",pch=19)

#Components of the SVD - Variance explained
par(mfrow=c(1,2))
plot(svd1$d,xlab="Column",ylab="Singular value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",
     ylab="Prop. of variance explained",pch=19)
#max contribuor to variance
plot(svd1$v[,1],pch=19)
maxContrib <-which.max(svd1$v[,1])
#new cluster with maxContributer
distanceMatrix <- dist(df[,maxContrib])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col=unclass(df)) 
