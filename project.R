library(magrittr)
library(car)
library(broom)
library(ggplot2)
library(BAS)

fileName <- '~/Documents/Bayesian Statistics/project/samples.csv'
samples <- read.csv(fileName)
summary(samples)

regmod <- lm(formula = Sale.Price ~., data = samples)
summary(regmod)
age = samples$Age-mean(samples$Age); college = samples$Pct.College-mean(samples$Pct.College); bedrooms = samples$Bedrooms-mean(samples$Bedrooms); 
size = samples$Lot.Size-mean(samples$Lot.Size); price = samples$Sale.Price-mean(samples$Sale.Price)
plot(price, resid(regmod), xlab = "Price", ylab = "residuals")
hist(resid(regmod), main="", xlab="residuals")
qqnorm(resid(regmod))
qqline(resid(regmod))
lm.fit <-regmod
summary(lm.fit)

confint(regmod)
exp(coef(regmod))

predictedValue <- predict(lm.fit, samples, interval = "confidence") %>%
  tidy()
ggplot(predictedValue, aes(x=samples$Sale.Price, y=fit))+
  geom_point()+
  geom_smooth(aes(color = 'model'))+
  geom_line(aes(x=seq(min(price),max(price), length.out = 25), 
                y=seq(min(price),max(price), length.out = 25), 
                color = 'ideal'))+
  labs(x="actual charges", y="fitted values") + 
  scale_color_manual('linear relation', values = c('red', 'blue')) +
  theme(legend.position = c(0.25, 0.8))+
  ggtitle("Real Estate Price")

price_bas <- bas.lm(samples$Sale.Price ~ .,
                     data = samples,
                     method = "MCMC",
                     prior = "ZS-null",
                     modelprior = uniform())
price_bas
summary(price_bas)
image(price_bas, rotate=F)
coef_price_bas <- coef(price_bas)
confint(coef_price_bas)
plot(price_bas, which = 1, ask=F)
plot(price_bas, which = 2, ask=F)

predictedValue_bas <- predict(price_bas, estimator="BMA", interval = "predict", se.fit=TRUE)
predict1=data.frame(predictedValue_bas$Ybma, samples$Sale.Price)

ggplot(predict1, aes(x=samples$Sale.Price, y=predict_1$Ybma))+
  geom_point()+
  geom_smooth(aes(color = 'model'))+
  geom_line(aes(x=seq(min(price),max(price), length.out = 25), 
                y=seq(min(price),max(price), length.out = 25), 
                color = 'ideal'))+
  labs(x="actual charges", y="fitted values") + 
  scale_color_manual('linear relation', values = c('red', 'blue')) +
  theme(legend.position = c(0.25, 0.8))+
  ggtitle("Real Estate Price")


