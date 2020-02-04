vec <- randu

vecx <- vec[,1]
vecy <- vec[,2]
vecz <- vec[,3]
plot(randu,col=c("red","blue"))
scatter.smooth(x=randu$y, y=randu$z, main="Second ~ Third",col="magenta")
scatter.smooth(x=randu$x, y=randu$z, main="First ~ Third",col="blue")

cor(vecy,vecz)
cor(vecx,vecz)
cor(vec)

scatterplot3d::scatterplot3d(randu[,1], randu[,2], randu[,3],angle=154)

#variabila raspuns e y iar cea predictor e x
#datele sunt imprastiate uniform, corelatia este foarte mica
#y = x * (-0,05431) + 0,50143
#ca modelul sa fie valid p-value trebuie sa fie mai mic decat 0,05. 
#R^2 >0,7 => X
#coeficientul lui x este nesemnificativ din punct de vedere statistic(folosind testul t)
cor(vecx,vecy)
scatter.smooth(x=vecx, y=vecy, main="First ~ Second",col=c("red"))
plot(vecx,vecy)
set.seed(504)
trainingRowIndex <- sample(1:nrow(randu), 0.8 *nrow(randu))
trainingData <- randu[trainingRowIndex,]
testData <- randu[-trainingRowIndex,]
linearMod = lm(y ~ x, trainingData)
predictions <- predict(linearMod, testData)
actual_pred <- data.frame(cbind(actualsY = testData$y,predictedY = predictions))
correlation_accuracy <- cor(actual_pred)
summary(linearMod)


#variabila raspuns e z iar cele predictor sunt x si y
#conform testelor din summary, modelul nu este valid
cor(vec)
linearModMultipla <- lm(z~y+x,trainingData)
predictionsMultipla <- predict(linearModMultipla, testData)
actual_pred_Multipla <- data.frame(cbind(actualZ = testData$z, predictedZ = predictionsMultipla))
correlation_accuracyMultipla <- cor(actual_pred_Multipla)
summary(linearModMultipla)
#z = x*0,06989+y*(-0.06493) + 0,47261

set.seed(100)
origDist <- runif(400,0,sqrt(3))
dataFrameNew <- data.frame(cbind(randu,dist = origDist))
lmDist <- lm(dist~x+y+z, dataFrameNew)
summary(lmDist)

calculatedOrigDist <- vector("numeric",400)
for(row in 1:nrow(randu)){
  x <- randu[row,"x"]
  y <- randu[row,"y"]
  z <- randu[row,"z"]
  calculatedOrigDist[row] <- sqrt(x*x+y*y+z*z)
}
dataFrameNew2 <- data.frame(cbind(randu,dist = calculatedOrigDist))
trainingRowIndex2 <- sample(1:nrow(randu), 0.8*nrow(randu))
trainingValues <- dataFrameNew2[trainingRowIndex2,]
testingValues <- dataFrameNew2[-trainingRowIndex2,]
lmDist2 <- lm(dist~x+y+z, trainingValues)
predictionsRaza <- predict(lmDist2, testingValues)
actualR_predR <- data.frame(cbind(actualR = testingValues$dist, predictedR = predictionsRaza))
cor(actualR_predR)
summary(lmDist2)