#support vector machines model
library(e1071)

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

model<- svm(default ~ ., data = train, kernel = "linear", cost
= 10)
p <- predict(model, test[,-1])

p<-ifelse(p>0.5,1,0)
mean(test[,1] == p)
