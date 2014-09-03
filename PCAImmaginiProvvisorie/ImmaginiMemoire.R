X<-matrix()

n=120
set.seed(11)
X <- cbind(rnorm(n, mean = 0, sd = 10) )
set.seed(13)
y <- cbind(rnorm(n, mean = 0, sd = 3) )
X <- cbind(X , X[,1] + y )

plot(X , xlim= c(-30 , 30) , ylim= c(-30 , 30))

abline(0,0, col="grey")
abline(v =0, col="grey")


abline(0,1,col="red")
abline(0,-1,col="blue")




#due linee con un risultato scarso in PCA :

X<-matrix()

n=120
set.seed(1)
X <- cbind(rnorm(n, mean = 3, sd = 6) )
set.seed(2)

x <- cbind(-5 +rnorm(n, mean = 0, sd = 0.6) )
X <- cbind(X ,  x )

plot(X , xlim= c(-30 , 30) , ylim= c(-30 , 30))

abline(0,0, col="grey")
abline(v =0, col="grey")

Y<-matrix()

n=120
set.seed(5)
Y <- cbind(rnorm(n, mean = -3, sd = 6) )
set.seed(4)

y <- cbind(5 +rnorm(n, mean = 0, sd = 1) )
Y <- cbind(Y ,  y )

points(Y , xlim= c(-30 , 30) , ylim= c(-30 , 30))


pc = princomp(X)
pc$loadings

summary(pc)

abline(0,1,col="red")
abline(0,-1,col="blue")

#arrows(0, 0, x1 = 10, y1 = 10,  angle = 30, code = 2, col = 'red', lty = par("lty"), lwd = par("lwd"))

arrows(0, 0, 10*pc$loadings[,1][1] , 10*pc$loadings[,1][2] , col = 'yellow')

arrows(0, 0, 10*pc$loadings[,2][1] , 10*pc$loadings[,2][2] , col = 'orange')

