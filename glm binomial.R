#import data
df<-read.csv("C:\\Users\\dion\\Desktop\\Default.csv")
df$X<-NULL
df$default<-as.numeric(df$default)
df$student<-as.numeric(df$student)
df$student<-df$student-1
df$default<-df$default-1

#visualization
ggplot(data = df) + 
  geom_point(mapping = aes(x = income, y = balance))
ggplot(data = df) +
  geom_histogram(mapping = aes(x=income),bins=100)
ggplot(data = df) +
  geom_histogram(mapping = aes(x=balance),bins=100)
ggplot(data = df) + 
  geom_point(mapping = aes(x = income, y = balance, color = student))
ggplot(data = df) + 
  geom_point(mapping = aes(x = income, y = balance, size = student))

#split
n<- nrow(df)
shuffled <- df[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]
a<-test$default

#model
model<-glm(default~.,train,family=binomial((link = "logit")))
p<-predict(model,test,type="response")
p<-ifelse(p>0.5,1,0)

#assess
conf<-table(a,p)
accs<- sum(diag(conf))/sum(conf)
