#Read Air & select what we need 
df<-read.csv("Air.csv")
df<-df[,c(2,3,5,6)]
names(df)[1:3]<-c("Name","Year","Risk")
nozerodf <- df[cumsum(df[,4]) > 0]

#dplyr to get fold (avg) up risk levels 1-10
library("dplyr") 
e<-df%>% 
  group_by(Name,Year,Risk)%>% 
  summarise_each(funs(sum))

#Replace 0s with NA 
e[e==0] <- NA

#Reshape to take the risk levels to headings 
library("reshape")
fmelt<-melt(e[,c(1,3,4)], id=c("Name","Risk"), measure.vars=c("Value"))
fcast<-dcast(fmelt, Name~ Risk, mean, na.rm=T)
write.csv(fcast,file = "fcastAir.csv")
