setwd("/Users/mapengfei/Desktop/BU CS/Spring 2021/CS555/Homeworks/Project")

library(ggplot2)


### Simple linear regression of Adj Close and Volume

# Setting data

nasdaq <- read.csv("Nasdaq.csv")
nasdaq_weekly_detail <- read.csv("nasdaq_weekly_return_volatility_detailed.csv")
nasdaq_weekly <- read.csv("nasdaq_weekly_return_volatility.csv")

View(nasdaq)


# scatter plot of the relationship

relationship.adj.volume <- lm(nasdaq$Adj.Close~nasdaq$Volume)

ggplot(nasdaq, aes(x=Adj.Close, y=Volume))+geom_point()+
  labs(title='Relationship of Adj Close and Volume',
       x='Adj Close', y = 'Volume')


# Regression line and equation

ggplot(nasdaq, aes(x=Adj.Close, y=Volume))+geom_point()+geom_smooth()+
  labs(title='Relationship of Adj Close and Volume',
       x='Adj Close', y = 'Volume')

ggplot(nasdaq, aes(x=Adj.Close, y=Volume))+geom_point()+geom_smooth(method='lm', formula= y~x)+
  labs(title='Relationship of Adj Close and Volume',
       x='Adj Close', y = 'Volume')

paste("The least squares regression function is y=",
      coef(relationship.adj.volume)["(Intercept)"],"+",coef(relationship.adj.volume)["nasdaq$Adj.Close"],"x")

summary(relationship.adj.volume)


# correlation of the relationship

cor(nasdaq$Adj.Close, nasdaq$Volume)


# F-test

anova1 <- anova(relationship.adj.volume)
anova1
qf(.95, df1 = 1, df2 = 1257)


### Multiple regression of Adj Close, Volume and Volatility

multiple.relationship <- lm(nasdaq_weekly_detail$Volume ~ nasdaq_weekly_detail$Return + nasdaq_weekly_detail$Adj.Close)

multiple.relationship

summary(multiple.relationship)

data <- data.frame(nasdaq_weekly_detail$Adj.Close, nasdaq_weekly_detail$Return, nasdaq_weekly_detail$Volume)

cor(data)

pairs(data, labels = c("Adj Close", "Return", "Volume"))

anova(multiple.relationship)

qf(.99, df1 = 1, df2 = 1256)

resid(multiple.relationship)

