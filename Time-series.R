library("e1071", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("forecast", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("CEoptim", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")

Y = c(4.8, 4.1, 6, 6.5,5.8,5.2,6.8,7.4,6,5.6,7.5,7.8,6.3,5.9,8,8.4)

data()
dat = AirPassengers
View(dat)
plot(dat)
sm <- ma(Y,order=4)

SI <-c()
for(i in 1:length(sm))
{
  if (is.na(sm[i])){
    SI <- c(SI, NA)
  } else {
    SI <- c(SI,Y[i]/sm[i])
  }
}

S <- c()
for (i in 1:4){
  j = i;
  tmp <- c()
  while(j <= length(SI) ){
    if (!is.na(SI[j])){
      tmp <- c(tmp,SI[j])
    }
    j = j+4
  }
  S <- c(S, mean(tmp))
}

YS <-c()
for (i in 1:length(Y)){
    if (i%%length(S) == 0 ){
      YS <-c(YS, Y[i]/S[4])
    } else {
      YS <-c(YS, Y[i]/S[i%%length(S)])
    }
}
x = 1
x <- seq(x, length(Y),1)




#Support Vector Machine
svmfit <- svm(YS~x, cost=100) #kernel = "linear", cost = 10, scale = FALSE

#Prediction  
t <- 1
t <- seq(t, 16,1)


p <- predict(svmfit, t)
tuned <- tune(svm, YS~x, kernel = "linear", ranges = list(cost=c(0.001,0.01,.1,1,10,100)))
summary(tuned)

predSVM <- c()
for (i in 1:length(p)){
  if (i%%length(S) == 0 ){
    predSVM <- c(predSVM, S[4] * p[i])
  } else {
    predSVM <- c(predSVM, S[i%%length(S)] * p[i])
  }
}


# Linear Regression 
lm <- lm(YS~x)
ThamSo <- c()
ThamSo <- coef(lm)[["x"]]
ThamSo <- c(ThamSo, coef(lm)[["(Intercept)"]])

#calculate T
T <- c()
for(i in 1:length(x)){
  T <- c(T, x[i]*ThamSo[1] + ThamSo[2])
}

#Prediction
t <- 1
t <- seq(t, 16,1)


predLM <- c()
for (i in 1:length(t)){
  if (i%%length(S) == 0 ){
    predLM <- c(predLM, S[4] * (t[i]*ThamSo[1] + ThamSo[2]))
  } else {
    predLM <- c(predLM, S[i%%length(S)] * (t[i]*ThamSo[1] + ThamSo[2]))
  }
}



plot(x, Y, ylim=range(c(Y, predSVM, predLM)), type="o")
par(new = TRUE)
plot(x, predSVM, ylim=range(c(Y,predSVM, predLM)), type = "o", col="red", axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)
plot(x, predLM, ylim=range(c(Y,predSVM, predLM)), type = "o", col="blue", axes = FALSE, xlab = "", ylab = "")

errorSVM = 0
errorLM = 0
for (i in 1:length(Y)){
  errorSVM = errorSVM + (predSVM[i] - Y[i])^2
  errorLM = errorLM + (predLM[i] - Y[i])^2
}



