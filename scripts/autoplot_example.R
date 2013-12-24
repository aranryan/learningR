

n <- 50
y <- rnorm(n)
x <- matrix(rnorm(n*4), ncol=4)
mymodel <- lm(y~x)

class(x)
class(mymodel)

summary(x)
summary(mymodel)




library(ggplot2)

# Timeplotting a ts object
autoplot.ts <- function(x, ..., xlab="", ylab="", title=""){
  time <- attr(x, "tsp")
  time <- seq(time[1], time[2], by=1/time[3])
  df <- data.frame(x=x, time=time)
  ggplot(data=df, aes(time, x)) +
    geom_line() +
    scale_x_continuous(xlab) +
    scale_y_continuous(ylab) +
    labs(title=title)
}

install.packages("Mcomp")
library(Mcomp)


x <- M1[[222]]
class(x$x)
autoplot(x$x)

x <- M1[[893]]
autoplot(x$x)


library(ggplot2)

# Timeplotting an Mdata object
autoplot.Mdata <- function(x, ..., xlab="", ylab="", title=x$sn){
  time <- attr(x$x, "tsp")
  time <- seq(time[1], attr(x$xx, "tsp")[2], by=1/time[3])
  df <- data.frame(x=c(x$x, x$xx),
                   time=time,
                   xx=c(rep(NA, length(x$x)), x$xx)
  )
  ggplot(data=df, aes(time, x)) +
    geom_line() +
    geom_line(data=df[!is.na(df$xx), ], aes(time, xx), color="red", na.rm=TRUE) +
    scale_x_continuous(xlab) +
    scale_y_continuous(ylab) +
    opts(title=title)
}

library(Mcomp)

x <- M1[[222]]
autoplot(x)

x <- M1[[893]]
autoplot(x, xlab="Time")
head(x)

library(forecast)
x <- M1[[222]]
mymodel <- auto.arima(x$x)
myforecast <- forecast(mymodel)

class(myforecast)
plot(myforecast)


# This program is released under the GNU GPL >=2 license.  For Details, see http://librestats.com/licenses

# Timeplotting a forecast object with prediction intervals
library(forecast)
library(ggplot2)

autoplot.forecast <- function(forecast, ...){
  # data wrangling
  time <- attr(forecast$x, "tsp")
  time <- seq(time[1], attr(forecast$mean, "tsp")[2], by=1/time[3])
  lenx <- length(forecast$x)
  lenmn <- length(forecast$mean)
  
  df <- data.frame(time=time,
                   x=c(forecast$x, forecast$mean),
                   forecast=c(rep(NA, lenx), forecast$mean),
                   low1=c(rep(NA, lenx), forecast$lower[, 1]),
                   upp1=c(rep(NA, lenx), forecast$upper[, 1]),
                   low2=c(rep(NA, lenx), forecast$lower[, 2]),
                   upp2=c(rep(NA, lenx), forecast$upper[, 2])
  )
  
  ggplot(df, aes(time, x)) +
    geom_ribbon(aes(ymin=low2, ymax=upp2), fill="yellow") +
    geom_ribbon(aes(ymin=low1, ymax=upp1), fill="orange") +
    geom_line() +
    geom_line(data=df[!is.na(df$forecast), ], aes(time, forecast), color="blue", na.rm=TRUE) +
    scale_x_continuous("") +
    scale_y_continuous("") +
    opts(title=paste("Forecasts from ", forecast$method))
}

library(Mcomp)

myforecast <- forecast(auto.arima(M1[[222]]$x))
autoplot(myforecast)

myforecast <- forecast(auto.arima(M1[[893]]$x))
autoplot(myforecast)


# This program is released under the GNU GPL >=2 license.  For Details, see http://librestats.com/licenses

autoplot.forecast <- function(forecast, ..., holdout=NaN){
  # data wrangling
  time <- attr(forecast$x, "tsp")
  time <- seq(time[1], attr(forecast$mean, "tsp")[2], by=1/time[3])
  lenx <- length(forecast$x)
  lenmn <- length(forecast$mean)
  
  df <- data.frame(time=time,
                   x=c(forecast$x, forecast$mean),
                   x2=c(forecast$x, rep(NA, lenmn-length(holdout)), holdout),
                   forecast=c(rep(NA, lenx), forecast$mean),
                   low1=c(rep(NA, lenx), forecast$lower[, 1]),
                   upp1=c(rep(NA, lenx), forecast$upper[, 1]),
                   low2=c(rep(NA, lenx), forecast$lower[, 2]),
                   upp2=c(rep(NA, lenx), forecast$upper[, 2]),
                   holdout=c(rep(NA, lenx+lenmn-length(holdout)), holdout)
  )
  
  ggplot(df, aes(time, x)) +
    geom_ribbon(aes(ymin=low2, ymax=upp2), fill="yellow") +
    geom_ribbon(aes(ymin=low1, ymax=upp1), fill="orange") +
    geom_line(data=df, aes(time, x2), color="red")+
    geom_line() +
    geom_line(data=df[!is.na(df$forecast), ], aes(time, forecast), color="blue", na.rm=TRUE) +
    geom_line(data=df[!is.na(df$holdout), ], aes(time, holdout), color="red", na.rm=TRUE) +
    scale_x_continuous("") +
    scale_y_continuous("") +
    opts(title=paste("Forecasts from ", forecast$method))
}

library(Mcomp)

x <- M1[[222]]
myforecast <- forecast(auto.arima(x$x))
autoplot(myforecast)
autoplot(myforecast, holdout=x$xx)