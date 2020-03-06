uchder <- rnorm(n = 1000, mean = 170,sd = 15)
pwysau <- rnorm(n = 1000, mean = 90, sd =12)
maintgwasg <- rep(x = 0, length.out = 1000)
bmi <- pwysau/(uchder*0.01)**2
for (i in 1:1000)
{
  if (18<bmi[i] & bmi[i]<25)
  {
    maintgwasg[i] <- rnorm(n = 1, mean = 32, sd = 3)
  }
  if (25<=bmi[i] & bmi[i]<30)
  {
    maintgwasg[i] <- rnorm(n = 1, mean = 35, sd = 3)
  }
  if (30<=bmi[i])
  {
    maintgwasg[i] <- rnorm(n = 1, mean = 40, sd = 3)
  }
  if (bmi[i]<=18)
  {
    maintgwasg[i] <- rnorm(n = 1, mean = 28, sd = 3)
  }
}
oed <- runif( n = 1000, min = 18, max = 65)
oed <- round(x = oed, digits = 0)

rhyw <- rep(x = c("Gwryw", "Benyw"), times = 500)
rhyw <- sample(x = rhyw, size = 1000, replace = TRUE)

data <- matrix(c(uchder, pwysau, maintgwasg, oed, rhyw), ncol = 5, nrow = 1000, byrow = FALSE)

bf <- rep(x = 0, times = 1000)
for (i in 1:1000)
{
  if (data[i,5]=="Gwryw")
  {
    bf[i] <- 1.39*bmi[i]+0.16*as.numeric(data[i,4])-9-10.34
  }
  if (data[i,5]=="Benyw")
  {
    bf[i] <- 1.39*bmi[i]+0.16*as.numeric(data[i,4])
  }
}
data <- as.data.frame(data)
data[1,4]
max(bf)
mean(bf)
bf[bf>50]
diabetes <- c()
for (i in 1:1000)
{
  if (bf[i] >= 50)
  {
    diabetes <- c(diabetes,1)
  }
  if (bf[i] < 50 & bf[i] > 40)
  {
    diabetes <- c(diabetes, rbinom(n = 1, size = 1, prob = 0.3))
  }
  if (bf[i] <= 40)
  {
    diabetes <- c(diabetes, 0)
  }
}
bf
diabetes
names(data) <- c("Uchder", "Pwysau", "Maint Gwasg", "Oed", "Rhyw")
data$Diabetes <- diabetes
View(data)

rhifau <- c(1:1000)
rhifauymarfer <- sample(x = rhifau, size = 700, replace = FALSE)

ymarfer <- data[rhifauymarfer,] 

rhifautest <- setdiff(rhifau, rhifauymarfer)
test <- data[rhifautest,]
attach(data)
install.packages("ISLR")
summary(data)
atchweliad <- glm(diabetes ~ uchder + pwysau + oed + rhyw + maintgwasg, family = binomial)
predict(atchweliad)
view(data)
plot(atchweliad)
summary(atchweliad)
which(predict(atchweliad) == min(predict(atchweliad)))

atchweliad
predict.glm(atchweliad,newydd, type = "response")
      newydd <- as.data.frame(matrix(c(170,80,36,20,"Benyw"), nrow = 1, byrow = TRUE))
newydd <- data.frame(Uchder = 170 , Pwysau = 75, "Maint Gwasg" = 36, Oed = 20, Rhyw = "Gwryw" ) 
names(newydd) <- c("Uchder", "Pwysau", "Maint Gwasg", "Oed", "Rhyw")
