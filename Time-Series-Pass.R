library("e1071", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("forecast", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("CEoptim", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")


dat = AirPassengers
plot(dat)


#cal moving avg
season = 12  
sm <- ma(dat,order=season) #12 months

#cal SI
SI <-c()
for(i in 1:length(sm))
{
  if (is.na(sm[i])){
    SI <- c(SI, NA)
  } else {
    SI <- c(SI,dat[i]/sm[i])
  }
}

#Cal seasonality 
S <- c()
for (i in 1:season){
  j = i;
  tmp <- c()
  while(j <= length(SI) ){
    if (!is.na(SI[j])){
      tmp <- c(tmp,SI[j])
    }
    j = j+season
  }
  S <- c(S, mean(tmp))
}


#Cal IT
YS <-c()
for (i in 1:length(dat)){
  if (i%%length(S) == 0 ){
    YS <-c(YS, dat[i]/S[4])
  } else {
    YS <-c(YS, dat[i]/S[i%%length(S)])
  }
}
x = 1
x <- seq(x, length(dat),1)

#Support Vector Machine
svmfit <- svm(YS~x, cost=10) #kernel = "linear", cost = 10, scale = FALSE

#Prediction  
t <- 1
t <- seq(t, length(dat),1)


p <- predict(svmfit, t)
tuned <- tune(svm, YS~x, ranges = list(cost=c(0.001,0.01,.1,1,10,100)))
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
t <- seq(t, length(dat),1)


predLM <- c()
for (i in 1:length(t)){
  if (i%%length(S) == 0 ){
    predLM <- c(predLM, S[4] * (t[i]*ThamSo[1] + ThamSo[2]))
  } else {
    predLM <- c(predLM, S[i%%length(S)] * (t[i]*ThamSo[1] + ThamSo[2]))
  }
}



plot(x, dat, ylim=range(c(dat, predSVM, predLM)), type="o")
par(new = TRUE)
plot(x, predSVM, ylim=range(c(dat,predSVM, predLM)), type = "o", 
     col="red", axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)
plot(x, predLM, ylim=range(c(dat,predSVM, predLM)), type = "o", 
     col="blue", axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)
abline(lm)
par(new = TRUE)
points(x, p, col = "pink", pch=4)

errorSVM = 0
errorLM = 0
for (i in 1:length(dat)){
  errorSVM = errorSVM + (predSVM[i] - dat[i])^2
  errorLM = errorLM + (predLM[i] - dat[i])^2
}
