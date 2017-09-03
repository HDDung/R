library("e1071", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("forecast", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("CEoptim", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")


dat = AirPassengers
fit <- stl(dat, t.window=12, s.window="periodic", robust=TRUE)

plot(fit)
summary(fit)
