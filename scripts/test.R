1+2 #test
install.packages("Quandl")
library(Quandl)
require(xts)

install.packages("ggplot2")
library(ggplot2)

Quandl.auth("K4F4USh2Zu2wKwG5jLJ2")
holddata = Quandl("FRED/GDP")
str(holddata)
head(holddata)

# looking at
# http://blog.revolutionanalytics.com/2013/06/a-mini-tutorial-for-quandl.html

# the following downloads the series as a time series object xts
GDP = Quandl("FRED/GDP",start_date="2000-01-01",end_date="2013-12-31", type="xts")
str(GDP)
head(GDP)
class(GDP)
head(GDP)
ggplot(GDP)
]
autoplot(GDP)
autoplot(log(GDP)) + stat_smooth()
ggplot(GDP) # didn't work because ggplot doesn't work with xts, put then I installed xts package
install.packages("xts", repos="http://R-Forge.R-project.org")
## but I think autoplot runs ggplot but with a different object type

autoplot(log(GDP))
autoplot(log(GDP)) + stat_smooth()
autoplot(log(GDP)) + stat_smooth(level = .90) #changes confidence interval to 90%, not much impact
autoplot(log(GDP)) + stat_smooth(level = .90, n=10) #n is number of points to evaluate smoother at, not much impact
autoplot(log(GDP)) + stat_smooth() + scale_fill_brewer()
autoplot(log(GDP)) + stat_smooth() + scale_fill_brewer(palette="Paired")
RColorBrewer::display.brewer.all()
autoplot(log(GDP)) + stat_smooth() + scale_linetype_manual(values = c("red","blue"))


install.packages('ggthemes', dependencies = TRUE)
require(ggthemes)
autoplot(log(GDP)) + geom_rangeframe() + theme_tufte() + stat_smooth()
autoplot(log(GDP)) + geom_rangeframe() + theme_wsj() + stat_smooth()
autoplot(log(GDP)) + geom_rangeframe() + theme_few() + stat_smooth()
autoplot(log(GDP)) + geom_rangeframe() + theme_economist() + stat_smooth()
autoplot(log(GDP)) + geom_rangeframe() + theme_gdocs() + stat_smooth()
autoplot(log(GDP)) + geom_rangeframe() + theme_calc() + stat_smooth()
autoplot(log(GDP)) + geom_rangeframe() + theme_solarized() + stat_smooth()
autoplot(log(GDP)) + geom_rangeframe() + theme_stata() + stat_smooth()



autoplot(log(GDP)) + geom_rangeframe() + theme_few() + stat_smooth(size=1) + geom_line(size = 1)

JPY = Quandl("QUANDL/USDJPY",start_date="2000-01-01",end_date="2013-06-07", type="xts")
class(JPY)
head(JPY)
holddata <- GDP
head(holddata)
plot(JPY)
