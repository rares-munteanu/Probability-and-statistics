vec <- randu

#Statistica descriptiva pentru intreg setul de date
randuMean <- lapply(vec,mean)
variance <- var(vec)
dataSummary <-summary(vec)
boxplot(randu,col = c("red","green","blue"))

#Extragem fiecare coloana din setul de date 
vecx <- vec[,1]
vecy <- vec[,2]
vecz <- vec[,3]

#Pentru variabila x:
meanX <- mean(vecx)
varX <- var(vecx)
qx <- quantile(vecx)
bplotX <- boxplot(vecx,col="red")

#Pentru variabila y:
meanY <- mean(vecy)
varY <- var(vecy)
qy <- quantile(vecy)
bplotY <- boxplot(vecy,col="green")

#Pentru variabila z:
meanZ <- mean(vecz)
varZ <- var(vecz)
qz <- quantile(vecz)
bplotZ <- boxplot(vecz,col="blue")



