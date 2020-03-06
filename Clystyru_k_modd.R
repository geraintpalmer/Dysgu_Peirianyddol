clysteru_tri <- kmeans(iris[,1:4],3, nstart = 20)
clysteru_tri
plot(iris[,1:4], pch = 24, bg=c("red","green3","blue")[unclass(clysteru_tri$cluster)])
plot(iris[,1:4], pch = 24, bg=c("red","green3","blue")[unclass(iris$Species)])

Data <- matrix(c(rnorm(n=100, mean= 170, sd=15),rnorm(n=100, mean=75, sd=8)), ncol=2)
Data2 <- matrix(c(rnorm(n=50, mean= 180, sd=15),rnorm(n=50, mean=90, sd=8)), ncol=2)
Data3 <- matrix(c(rnorm(n=10, mean= 120, sd=15),rnorm(n=00, mean=55, sd=8)), ncol=2)
Height <- c(rnorm(n=100, mean= 150, sd=15),rnorm(n=50, mean= 180,sd=15),rnorm(n=25, mean= 110, sd=15))
Weight <- c(rnorm(n=100, mean=55, sd=8), rnorm(n=50, mean=110, sd=8),rnorm(n=25, mean=145, sd=8))
Datacombined <- matrix(c(Height,Weight), ncol=2)          
plot(Datahead,Dataweight, pch=21)
attach(Datacombined)
Datacombined <- as.data.frame(Datacombined)
Datacombined <- Data
plot(sample(Datacombined))
plot(Datacombined)
clusterexample <- kmeans(Datacombined,3, nstart = 20)
plot  (Datacombined, pch = 24, bg=c("red","green3","blue")[unclass(clusterexample$cluster)], xlabel="Height", ylabel="Weight")

###CYCHWYN O FAN HYN
install.packages("graphics")
install.packages("stats")
install.packages("datasets")
library(graphics)
library(stats)
library(datasets)
heightvsweight <- read.csv("C:/Users/User/Desktop/Dysgu_Peirianyddol/heightvsweight.csv")

View(heightvsweight)
attach(heightvsweight)
plot(Uchder, Pwysau, pch = 21)
kcymedr <- kmeans(heightvsweight,3, nstart = 50)
plot(Uchder, Pwysau, pch = 21, bg=c("red","green","blue")[unclass(kcymedr$cluster)])
heightvsweight$Clwstwr3 <- kcymedr$cluster
View(heightvsweight)
kcymedr <- kmeans(heightvsweight,6, nstart = 50)
plot(Uchder, Pwysau, pch = 21, bg=c("red","green","blue", "yellow", "black", "white")[unclass(kcymedr$cluster)])
heightvsweight$Clwstwr6 <- kcymedr$cluster 
View(heightvsweight)

#Darn gweledol o clystyru k-cymedr

uchderpwysau <- read.csv("C:/Users/User/Desktop/Dysgu_Peirianyddol/heightvsweight.csv")
attach(uchderpwysau)
plot(Uchder, Pwysau )
xclwstwr <- runif(n = 3, min = min(Uchder), max = max(Uchder))
yclwstwr <- runif(n = 3, min = min(Pwysau), max = max(Pwysau))
clystyrau <- matrix(data = c(xclwstwr, yclwstwr), nrow = 3, ncol = 2)
points(x = xclwstwr, y = yclwstwr, pch = 20, col = c("red","yellow", "Blue"), cex = 2)
clystyrau

cychwynol <- sample(x = c(1,2,3), size = 175, replace = TRUE)
plot(Uchder, Pwysau, pch=20, col = c("red","yellow", "Blue")[unclass(cychwynol)])

q1 <- c(0,0)
for (i in which(cychwynol %in% 1))
{
  q1 <- q1 + uchderpwysau[i,]
}
q1 <- q1/length(which(cychwynol %in% 1))

q2 <- c(0,0)
for (i in which(cychwynol %in% 2))
{
  q2 <- q2 + uchderpwysau[i,]
}
q2 <- q2/length(which(cychwynol %in% 2))

q3 <- c(0,0)
for (i in which(cychwynol %in% 3))
{
  q3 <- q3 + uchderpwysau[i,]
}
q3 <- q3/length(which(cychwynol %in% 3))
plot(Uchder, Pwysau)
points(matrix(c(q1,q2,q3), nrow = 3, ncol = 2, byrow = TRUE), pch = 20, col = c("red","yellow", "Blue"), cex = 2)
z1 <- c()
z2 <- c()
z3 <- c()
for (i in 1:175)
{
  z1 <- c(z1, distance(q1, uchderpwysau[i,]) )
}
for (i in 1:175)
{
  z2 <- c(z2, distance(q2, uchderpwysau[i,]) )
}
for (i in 1:175)
{
  z3 <- c(z3, distance(q3, uchderpwysau[i,]) )
}
distance <- function(x,y)
{
  sqrt( (x[1]-y[1])^2 + (x[2]-y[2])^2 )
}
z1 <- as.vector(z1, mode= "numeric")
z2 <- as.vector(z2, mode= "numeric")
z3 <- as.vector(z3, mode= "numeric")

clwstwrn <- c()

for (i in 1:175)
{
  if (min(z1[i],z2[i],z3[i]) == z1[i])
  {
    clwstwrn <- c(clwstwrn, 1)
  }
  if (min(z1[i],z2[i],z3[i]) == z2[i])
  {
    clwstwrn <- c(clwstwrn, 2)
  }
  if (min(z1[i],z2[i],z3[i]) == z3[i])
    clwstwrn <- c(clwstwrn, 3)
}

plot(Uchder, Pwysau, pch = 20, col=c("red","yellow","blue")[unclass(clwstwrn)])
clwstwrn

q1 <- c(0,0)
for (i in which(clwstwrn %in% 1))
{
  q1 <- q1 + uchderpwysau[i,]
}
q1 <- q1/length(which(clwstwrn %in% 1))

q2 <- c(0,0)
for (i in which(clwstwrn %in% 2))
{
  q2 <- q2 + uchderpwysau[i,]
}
q2 <- q2/length(which(clwstwrn %in% 2))

q3 <- c(0,0)
for (i in which(clwstwrn %in% 3))
{
  q3 <- q3 + uchderpwysau[i,]
}
q3 <- q3/length(which(clwstwrn %in% 3))

plot(Uchder, Pwysau)
points(matrix(c(q1,q2,q3), nrow = 3, ncol = 2, byrow = TRUE), pch = 20, col = c("red","yellow", "Blue"), cex = 2)

cyntaf <- matrix(c(q1,q2,q3), nrow = 3, ncol = 2, byrow = TRUE)
ail <- matrix(c(q1,q2,q3), nrow = 3, ncol = 2, byrow = TRUE)
trydydd <- matrix(c(q1,q2,q3), nrow = 3, ncol = 2, byrow = TRUE)
pedwerydd <- matrix(c(q1,q2,q3), nrow = 3, ncol = 2, byrow = TRUE)
pumed <- matrix(c(q1,q2,q3), nrow = 3, ncol = 2, byrow = TRUE)
chweched <- matrix(c(q1,q2,q3), nrow = 3, ncol = 2, byrow = TRUE)

plot(Uchder, Pwysau, pch = 1)
points(cyntaf, pch = 20, col = c("red","yellow", "Blue"), cex = 0.75)
points(ail, pch = 20, col = c("red","yellow", "Blue"),cex = 1)
points(trydydd, pch = 20, col = c("red","yellow", "Blue"), cex = 1.25)
points(pedwerydd, pch = 20, col = c("red","yellow", "Blue"), cex = 1.5)
points(pumed, pch = 20, col = c("red","yellow", "Blue"), cex = 1.75)
points(chweched, pch = 20, col = c("red","yellow", "Blue"), cex = 2)
lines(matrix(c(cyntaf[1,],ail[1,],trydydd[1,],pedwerydd[1,],pumed[1,],chweched[1,]), nrow = 6, ncol = 2, byrow = TRUE), col = "red", pch = 5 )
lines(matrix(c(cyntaf[2,],ail[2,],trydydd[2,],pedwerydd[2,],pumed[2,],chweched[2,]), nrow = 6, ncol = 2, byrow = TRUE), col = "yellow", pch = 5 )
lines(matrix(c(cyntaf[3,],ail[3,],trydydd[3,],pedwerydd[3,],pumed[3,],chweched[3,]), nrow = 6, ncol = 2, byrow = TRUE), col = "blue", pch = 5 )

#Elbow method
elb <- c()
ktest <- kmeans(x = uchderpwysau, centers = 1, nstart = 50)$tot.withinss
ktest
for (i in 1:12)
{
  elb <- c(elb, kmeans(x = uchderpwysau, centers = i, nstart = 50)$tot.withinss)
}
elb
plot(x = 1:12, y = elb, main = "Ffurf Penelin o ddarganfod K", xlab = "Nifer o Clystyrau", ylab = "Swm Sgwariau o fewn Clystyrau")
lines(x = elb)

#Dendrogram

UchderPwysau <- dist(uchderpwysau, method="euclidean")
( dend <- hclust(UchderPwysau, method = "complete") )
dend <- as.dendrogram(dend)
plot(dend, ylab = "Annhebygrwydd", main = "Dendrogram am nifer o Clystyrau")
