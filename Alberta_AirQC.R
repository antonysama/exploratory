#Read Air & select what we need 
df<-read.csv("Air.csv")
df<-df[,c(2,3,5,6)]
names(df)[1:3]<-c("Name","Year","Risk")

#dplyr to get fold (avg) up risk levels 1-10
library("dplyr") 
e<-df%>% 
  group_by(Name,Year,Risk)%>% 
  summarise_each(funs(sum, mean))
#further fold (avg) up the years
f<-e[,c(1:4)]%>% 
  group_by(Name,Risk)%>% 
  summarise_each(funs(mean))

#Reshape to take the risk levels to headings 
library("reshape2")
fmelt<-melt(f[,c(1,2,4)], id=c("Name","Risk"), measure.vars=c("sum"))
fcast<-dcast(fmelt, Name~ Risk)
write.csv(fcast,file = "fcastAir.csv")
