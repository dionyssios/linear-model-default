library(class)


df<-read.csv("C:\\Users\\dion\\Desktop\\Default.csv")
df$X<-NULL
df$default<-as.numeric(df$default)
df$student<-as.numeric(df$student)
df$student<-df$student-1
df$default<-df$default-1


#transformations
normalize <- function(x) {
return ((x - min(x)) / (max(x) - min(x))) }

df_n <- as.data.frame(lapply(df, normalize))

#split
n<- nrow(df_n)
shuffled <- df_n[sample(n),]
train <- shuffled[1:round(0.7 * n),-1]
test <- shuffled[(round(0.7 * n) + 1):n,-1]
train_l <- shuffled[1:round(0.7 * n),1]
test_l <- shuffled[(round(0.7 * n) + 1):n,1]

#model knn
model<- knn(train=train, test = test, cl = train_l, k=10)

#assess
conf<-table(test_l,model)
accs<- sum(diag(conf))/sum(conf)
accs







