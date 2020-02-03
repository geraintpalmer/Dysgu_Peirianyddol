clysteru_tri <- kmeans(iris[,1:4],3, nstart = 20)
clysteru_tri
plot(iris[,1:4], pch = 24, bg=c("red","green3","blue")[unclass(clysteru_tri$cluster)])
plot(iris[,1:4], pch = 24, bg=c("red","green3","blue")[unclass(iris$Species)])