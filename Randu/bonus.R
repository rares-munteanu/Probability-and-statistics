n <- as.numeric(readline(prompt="N="))
m <- as.numeric(readline(prompt="M= "))

xVals1 <- -n:n
xVals <- sample(xVals1,n)
yVals1 <- -m:m
yVals <- sample(yVals1,m)
xVals <- sort(xVals)
yVals <- sort(yVals)

# acum avem in xVals valorile variabilei aleatoare
# X fiind niste numere aleatoare din intervalul -n -> n
#analog pentru Y

#in continuare 
#generam probabilitatile variabilelor aleatoare X si Y

xProbs <- runif(n)
xProbs <- round(xProbs/sum(xProbs),2)

yProbs <- runif(m)
yProbs <- round(yProbs/sum(yProbs),2)

#unim vectorii pentru a rezulta variabilele
#aleatoare corespunzatoare

X <- rbind(xVals,xProbs)
Y <- rbind(yVals,yProbs)

tabel <- matrix(Inf,nrow = n+1,ncol = m+1)
tabel[n+1,m+1] <- 1 # Suma (pi) = Suma(qj) = 1

#completam toate qj in afara de ultima 
#care va fi ,, aflata" scazand suma de 1- sum(qj) j=1,n-1
for(i in 1:(m-1)){
  tabel[n+1,i] = Y[2,i]
}

#analog si pentru  pi
for(i in 1:(n-1)){
  tabel[i,m+1] = X[2,i]
}

#Generam destule valori (PI)ij pentru 
#ca tabelul sa poata fi completat ulterior


