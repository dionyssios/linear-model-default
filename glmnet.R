#import data
df<-read.csv("C:\\Users\\dion\\Desktop\\Default.csv")
df$X<-NULL
df$default<-as.numeric(df$default)
df$student<-as.numeric(df$student)
df$student<-df$student-1
df$default<-df$default-1
df$student<-NULL

#split data
n<- nrow(df)
shuffled <- df[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]
a<-test$default

#model
train_m <- model.matrix(default~ .,train)[,-1]
test_m <- model.matrix(default~ .,test)[,-1]
model<-glmnet(train_m,train$default, alpha = 0)

#metaparameter ë
ridge.cv <- cv.glmnet(train_m,train$default, alpha = 0)
lambda_ridge <- ridge.cv$lambda.min

p<-predict(model,s=lambda_ridge,test_m)

#assess
mean((a-p) ^ 2)
p<-ifelse(p>0.5,1,0)
conf<-table(a,p)
accs<- sum(diag(conf))/sum(conf)












