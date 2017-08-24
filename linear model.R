#erase everything
rm(list=ls(all=TRUE))

#import data
df<-read.csv("C:\\Users\\dion\\Desktop\\Default.csv")

df$X<-NULL
df$default<-NULL
df$student<-NULL

#visualization
ggplot(data = df) + 
  geom_point(mapping = aes(x = income, y = balance))
ggplot(data = df) +
 geom_histogram(mapping = aes(x=income),bins=100)

#split train-test set
n<- nrow(df)
shuffled <- df[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]
a<-test$income

#model
model<-lm(income~balance,train)
summary(model)
p<-predict(model,test)

#assess 
mean((a-p)^2)
