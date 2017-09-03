library("e1071")
plot(cats)

plot(cats$Hwt, cats$Bwt, col=cats$Sex)
col<-c("Hwt", "Bwt", "Sex")

s<-sample(140, 100)
cats_train<-cats[s,col]
cats_test<-cats[-s,col]

svmfit <- svm(Sex ~., data = cats_train, kernel = "linear", cost = .24, scale = FALSE)
print(svmfit)
plot(svmfit , cats_train[,col])

tuned <- tune(svm, Sex ~., data = cats_train, kernel = "linear", ranges = list(cost=c(0.001,0.01,.1,1,10,100)))
summary(tuned)

p <- predict(svmfit, cats_test[,col], type="class")
plot(p)

table(p, cats_test[,3])
mean(p== cats_test[,3])
