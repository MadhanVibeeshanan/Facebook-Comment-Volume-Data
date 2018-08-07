dataset2 <-read.csv("C:\\Users\\madha\\Documents\\Sec Sem\\Machine Learning\\Assignment 1\\Dataset\\Dataset\\Training\\Features_Variant_1.csv",stringsAsFactors = FALSE,sep = ",",dec=".",header = FALSE)

#Convert every column into numeric.... if not possible, it will be converted to NA
#If there is any NA, it will be set to 0
for (i in 1:ncol(dataset2)){
  dataset2[,i] = as.numeric(as.character(dataset2[,i]))
  dataset2[,i][is.na(dataset2[,i])] <- 0
}

X = dataset2[,1:52]
y = as.matrix(dataset2[,53])

library(caTools)
set.seed(123456) # Setting the seed to a random number 123456
split = sample.split(y,SplitRatio = 0.2) # Splitting the dataset such that 20% is used for training and rest for testing 
xtrain2 = subset(X,split == TRUE)
xtest2 = subset(X,split == FALSE)
ytrain2 = subset(y,split == TRUE)
ytest2 = subset(y,split == FALSE)

lit <- 1:nrow(xtrain2)
lit1 <- rep(1, length(lit))
xtrain2 <- cbind(lit1, xtrain2)


library(Matrix)
xtrain2matrix <- as.matrix(xtrain2)
xtrain2matrixtrans <- t(xtrain2matrix)
xtrain2matrixmul <- xtrain2matrixtrans %*% xtrain2matrix
library(matlib)
xtrain2matrixmulinv <- inv(xtrain2matrixmul)
multi <- xtrain2matrixmulinv %*% xtrain2matrixtrans
w <- multi %*% as.matrix(ytrain2)
wtrans <- t(w)
#wtrans1 <- as.matrix(wtrans)
ypred <-  xtrain2matrix %*% w
tot1 <- 0
for (i in 1:nrow(ypred)) {
  z <- (ypred[i,] - ytrain2[i,])
  z1 <- z * z
  tot1 <- tot1 + z1
}
