install.packages("stats")
library(stats)
data <- read.csv("C:/Users/User/Desktop/Dysgu_Peirianyddol/data_logistic.csv")
View(data)
str(data)
rhifau <- c(1:1000)
rhifauymarfer <- sample(x = rhifau, size = 700, replace = FALSE)
ymarfer <- data[rhifauymarfer,] 
rhifauprofi <- setdiff(rhifau, rhifauymarfer)
profi <- data[rhifauprofi,]
atchweliad <- glm(Diabetes ~ Uchder + Pwysau + Oed + Rhyw + MaintGwasg, family = binomial, data = ymarfer)
atchweliad$coefficients
canlyniad <- round(predict(object = atchweliad, newdata = profi, type = "response"), digits = 0)
zzz <- round(predict(object = atchweliad, newdata = ymarfer, type = "response"), digits = 0)
canlyniad
head(test)
canlyniad
profi

1-(sum((profi[,6]-unname(canlyniad))**2)/length(profi[,6]))
1-(sum((ymarfer[,6]-unname(zzz))**2)/length(ymarfer[,6]))
str(atchweliad)
unname(round(predict(object = atchweliad, newdata = data.frame( Uchder = 160, Pwysau = 92, MaintGwasg = 34, Oed = 20, Rhyw = "Benyw"), type = "response"), digits = 0))
plot(atchweliad)
plot(x = x, y = 1/(1 + exp(unname(atchweliad$coefficients[1]+unname(atchweliad$coefficients[2] * x)))), col = "white", xlab = "Pwysau", ylab = "Tebygolrwydd fod person gyda diabetes")
x <- 50:150
lines(x = x, y = 1/(1 + exp((unname(atchweliad$coefficients[1]+unname(atchweliad$coefficients[2] * x))))))                            
points(x = x1, y = y1, col = "red")
predict(object = atchweliad, newdata = test, type = "response")
atchweliad2 <- glm(Diabetes ~ Pwysau, family = binomial, data = ymarfer)
canlyniad2 <- round(predict(object = atchweliad, newdata = profi, type = "response"), digits = 0)
profi[,2]
x1 <- c()
x1 <- c(x1, rnorm(n = 50, mean = 110, sd = 10))
x1 <- c(x1, rnorm(n = 50, mean = 90, sd = 8))
x1 <- c(x1, rnorm(n=50, mean = 70, sd =10))
y1 <- c() 
y1 <- c(y1, rbinom(n = 50,size = 1, prob = 0.95 ))
y1 <- c(y1, rbinom(n = 50,size = 1, prob = 0.5 ))
y1 <- c(y1, rbinom(n = 50,size = 1, prob = 0.05 ))
abline(lm(formula = y1 ~ x1 ))
