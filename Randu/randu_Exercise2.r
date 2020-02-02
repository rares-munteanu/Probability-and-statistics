vec <- randu

vecx <- vec[,1]
vecy <- vec[,2]
vecz <- vec[,3]

scatter.smooth(x=randu$x, y=randu$y, main="First ~ Second",col="red")
scatter.smooth(x=randu$y, y=randu$z, main="Second ~ Third",col="magenta")
scatter.smooth(x=randu$x, y=randu$z, main="First ~ Third",col="blue")
cor(vecx,vecy)
cor(vecy,vecz)
cor(vecx,vecz)

cor

scatterplot3d::scatterplot3d(randu[,1], randu[,2], randu[,3],angle=154)
