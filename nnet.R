library(caret)
library(nnet)

df<-read.csv("C:\\Users\\dion\\Desktop\\Default.csv")
df$X<-NULL
df$default<-as.numeric(df$default)
df$student<-as.numeric(df$student)
df$student<-df$student-1
df$default<-df$default-1

n<- nrow(df)
shuffled <- df[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]
a<-test$default

train_p <- preProcess(train, method = c("range"))
train <- cbind(predict(train_p,train),Type = train$default)
test <- cbind(predict(train_p,test),Type = test$default)
model<-nnet(default~.,train,size=10,maxit=1000)

p<-predict(model,test)

p<-ifelse(p>0.5,1,0)
conf<-table(a,p)
accs<- sum(diag(conf))/sum(conf)



