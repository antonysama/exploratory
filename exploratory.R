# to count the number of times (FREQ) a value "x" appears in the vector.
numbers <- c(4,23,4,23...)
as.data.frame(table(numbers))

#Density plots
plot(density(df3$income, na.rm = T))
plot(density(df2$income, na.rm = T))
plot()
summary("df$income")
boxplot(income~ID, data = df, main="incm data", xlab="location", ylab="incm")
