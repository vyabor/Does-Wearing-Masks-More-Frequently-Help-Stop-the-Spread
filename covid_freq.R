# Reading in the data
covid <- read.csv('C:/Users/vyabo/OneDrive/covid/covid-cases-masks.xlsb.csv',header=T)
rate <- read.csv('C:/Users/vyabo/OneDrive/covid/united_states_covid19_cases_and_deaths_by_state.csv',header=T)

# Computing the mean mask wearing frequency for each state
avg <- aggregate(covid[,5:9],list(covid$state),mean)
masks <- data.frame(avg,rate$Case.Rate.per.100000)
masks

# Finding the average number of cases per 100000 based on certain frequency criteria
rarely_masked <- mean(subset(masks,never + rarely > 0.2)$rate.Case.Rate.per.100000)
likely_masked <- mean(subset(masks,frequently + always > 0.8)$rate.Case.Rate.per.100000)
covid_means <- data.frame(rarely_masked,likely_masked)
write.csv(covid_means,'C:/Users/vyabo/OneDrive/covid/covid_means.csv')

# Multiple linear regression model of infection rate per 100000 on the five mask wearing frequencies
masks_model <- lm(masks$rate.Case.Rate.per.100000~masks$never+masks$rarely+masks$sometimes+masks$frequently+masks$always)
plot(lm(masks$rate.Case.Rate.per.100000~masks$never+masks$rarely+masks$sometimes+masks$frequently+masks$always))
summary(masks_model)
masks_model

library(leaps)
leap_cov <- regsubsets(rate.Case.Rate.per.100000~never+rarely+sometimes+frequently+always,data=masks)
summary(leap_cov)
plot(leap_cov,scale='Cp')
coef(leap_cov,1:5)

# Creating the five simple linear regression models and plots
nev <- lm(masks$rate.Case.Rate.per.100000~masks$never)
plot(nev)
summary(nev)
coef(nev)
plot(masks$never,masks$rate.Case.Rate.per.100000,xlab='Percent to Never Wear A Mask', ylab='Cases Per 100000 people')
abline(nev)
text(0.09,10000,labels='y = 2954.332 + 24677.809*x',cex = 0.8)

rar <- lm(masks$rate.Case.Rate.per.100000~masks$rarely)
plot(rar)
summary(rar)
plot(masks$rarely,masks$rate.Case.Rate.per.100000,xlab='Percent to Rarely Wear A Mask', ylab='Cases Per 100000 people')
abline(rar)
text(0.08,10000,labels='y = 3028.4 + 22274.2*x',cex = 0.8)

som <- lm(masks$rate.Case.Rate.per.100000~masks$sometimes)
plot(som)
summary(som)
plot(masks$sometimes,masks$rate.Case.Rate.per.100000,xlab='Percent to Sometimes Wear A Mask', ylab='Cases Per 100000 people')
abline(som)
text(0.08,10000,labels='y = 1385.0 + 30055.2*x',cex = 0.8)

freq <- lm(masks$rate.Case.Rate.per.100000~masks$frequently)
plot(freq)
boxplot(masks$frequently)
masks2 <- masks[c(1,3:8,10:51),]
freq2 <- lm(masks2$rate.Case.Rate.per.100000~masks2$frequently)
plot(freq2)
summary(freq2)
plot(masks2$frequently,masks2$rate.Case.Rate.per.100000,xlab='Percent to Frequently Wear A Mask', ylab='Cases Per 100000 people')
abline(freq2)
text(0.16,9500,labels='y = -444.2 + 25841.4*x',cex=0.8)

alw <- lm(masks$rate.Case.Rate.per.100000~masks$always)
plot(alw)
summary(alw)
plot(masks$always,masks$rate.Case.Rate.per.100000,xlab='Percent to Always Wear A Mask', ylab='Cases Per 100000 people')
abline(alw)
text(0.73,10000,labels='y = 8633 - 7217*x',cex=0.8)
