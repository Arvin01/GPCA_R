source("GPCA_14.04.22.R")
getwd()


  ###################################################
  #                                                 #
  #                  Caso easy                      #
  #                                                 #
  ###################################################
  
{
  #rm(list=ls(all=TRUE))
  z1 <- c(1:15)
  y1 <- 2*z1
  z2 <- c(5:20)
  y2 <- 3*z2
  
  x1 = c(z1, z2)
  x2 = c(y1, y2)
  X <- cbind(x1,x2)
  plot(x1,x2, ylim=c(-3,30),xlim=c(-3,30))
  
  n=2
  
  Y=t(c(1,16))
  
  GPCA = sebaGPCA(X, 2, Y)
  
  attach(GPCA)
  
  abline(0,2,col="green")
  abline(0,3,col="blue")
  for(i in 1:15){   lines(c(X[i,1], X[i,1]+Dpn[1,i]*10),c(X[i,2], X[i,2]+Dpn[2,i]*10),col="green")}
  for(i in 16:31){  lines(c(X[i,1], X[i,1]+Dpn[1,i]*10),c(X[i,2], X[i,2]+Dpn[2,i]*10),col="blue") }
  
  round ( t(as.vector(B[,1]))%*%t(X) ,10)
  round ( t(as.vector(B[,2]))%*%t(X) ,10)
}
  
  ###################################################
  #                                                 #
  #             Caso easy  + noise                  #
  #                                                 #
  ###################################################
  
{
  rm(GPCA, X, Y, n)
  z1 <- c(1:15)
  y1 <- 1*z1+rnorm(length(z1), mean=0, sd=0.5)
  z2 <- c(5:20)
  y2 <- 3*z2+rnorm(length(z2), mean=0, sd=1)
  
  x1 = c(z1, z2)
  x2 = c(y1, y2)
  X <- cbind(x1,x2)
  plot(x1,x2, ylim=c(-3,60),xlim=c(-3,60))
  
  n=2
  Y=t(c(1,16))
  
  
  GPCA = sebaGPCA(X, 2, Y)
  
  attach(GPCA)
  
  abline(0,1,col="green")
  abline(0,3,col="blue")
  for(i in 1:15){   lines(c(X[i,1], X[i,1]+Dpn[1,i]*10),c(X[i,2], X[i,2]+Dpn[2,i]*10),col="green")}
  for(i in 16:31){  lines(c(X[i,1], X[i,1]+Dpn[1,i]*10),c(X[i,2], X[i,2]+Dpn[2,i]*10),col="blue") }
  
  round ( t(as.vector(B[,1]))%*%t(X) )
  round ( t(as.vector(B[,2]))%*%t(X) ) 
}
  
  
  ###################################################
  #                                                 #
  #             Caso Plane-row simple               #
  #                                                 #
  ###################################################
{
  rm(list=ls(all=TRUE))
  library(rgl)
  
  N1=50
  
  z1 <- rnorm( N1 , mean=0, sd=30)
  y1 <- rnorm( N1 , mean=0, sd=30)
  w1 <- rep(0, N1)  	# +rnorm(length(z1), mean=0, sd=0.5)
  
  z2 <- rep(0,15)
  y2 <- rep(0,15)		# +rnorm(length(z2), mean=0, sd=1)
  w2 <- rnorm( 15 , mean=0, sd=30)
  
  x1 = c(z1, z2)
  x2 = c(y1, y2)
  x3 = c(w1, w2)
  X <- cbind(x1,x2, x3)
  
  plot3d(X , xlim=c(0,200), ylim=c(0,200), zlim=c(0,200))
  n=2
  
  Y=t(c(1,60))
  
  GPCA = sebaGPCA(X, 2, Y)  
  attach(GPCA)
  
  B
}
  
  
  ###################################################
  #                                                 #
  #             Caso Plane-row +  noise             #
  #                                                 #
  ###################################################
{
  rm(list=ls(all=TRUE))
  
  library(rgl)
  
  N1=50
  
  z1 <- rnorm( N1 , mean=0, sd=30)
  y1 <- rnorm( N1 , mean=0, sd=30)
  w1 <- rnorm( N1 , mean=0, sd=0.5)  	# +rnorm(length(z1), mean=0, sd=0.5)
  
  z2 <- rnorm( 15 , mean=0, sd=0.5)
  y2 <- rnorm( 15 , mean=0, sd=0.5)		# +rnorm(length(z2), mean=0, sd=1)
  w2 <- rnorm( 15 , mean=0, sd=30)
  
  x1 = c(z1, z2)
  x2 = c(y1, y2)
  x3 = c(w1, w2)
  X <- cbind(x1,x2, x3)
   
  X[1,]	= c(5, 10 ,0)
  X[51,] = c(0, 0 ,30)
  Y= c(rep("green", 50), rep("blue", 15))
  
  plot3d(X , xlim=c(0,200), ylim=c(0,200), zlim=c(0,200) , col=Y)
  
  n=2
  
  Y=t(c(2,60))
  
  GPCA = sebaGPCA(X, 2, Y)  
  attach(GPCA)
  
  group
  
  for (i in 1:n){
    for ( l in 1:ncol(B) ){
      drawVects3d(X[group == i,], B[,i]*100, couleurs[i])
    }
  }
}
  
  #######################################################################################################################
  
  
  ###################################################
  #                                                 #
  #             Caso Plane-row +  noise             #
  #                                                 #
  ###################################################
{
  rm(GPCA, X, Y, n, B)  
  
  z1 <- c(-15:15)+3
  y1 <- 2*z1+5
  z2 <- c(6:20)
  y2 <- 3*z2
  
  x1 = c(z1, z2)
  x2 = c(y1, y2)
  x3 = rep(1,length(x1))
  X <- cbind(x1,x2, x3)
  plot(x1,x2, ylim=c(-3,40),xlim=c(-3,40))
  
  Y = t(c(1,35))
  GPCA = sebaGPCA(X, 2, Y)
  GPCA$group
  B = GPCA$B
  
  plot3d(X, type="s", col=group, size=0.3 , xlim=c(0,60), ylim=c(0,60), zlim=c(1,60))
  
  planes3d((B[1,1]), (B[2,1]), (B[3,1]), 0, alpha=0.9 , col="blue")
  
  planes3d((B[1,2]) , (B[2,2]) , (B[3,2]), 0, alpha=0.9, col="red")
}
  
  ###################################################
  #                                                 #
  #            spostamento Plane-row  +noise        #
  #                                                 #
  ###################################################
{
  rm(GPCA, X, Y, n, B)
  N1=50
  
  z1 <- rnorm( N1 , mean=0, sd=30)
  y1 <- rnorm( N1 , mean=0, sd=30)
  w1 <- rep(150, N1)    # +rnorm(length(z1), mean=0, sd=0.5)
  
  z2 <- rep(0,15)
  y2 <- rep(0,15)		# +rnorm(length(z2), mean=0, sd=1)
  w2 <- rnorm( 15 , mean=0, sd=30)
  
  x1 = c(z1, z2)
  x2 = c(y1, y2)
  x3 = c(w1, w2)
  X <- cbind(x1,x2, x3)
  X = cbind(X,rep(1, length(X[,1])))
  
  
  Y = t(c(1,60))
  GPCA = sebaGPCA(X, 2, Y)
  GPCA$group
  B = GPCA$B
  
  plot3d(X, type="s", col=GPCA$group, size=0.3 , xlim=c(0,100), ylim=c(0,100), zlim=c(1,100))
  
  planes3d((B[1,1]), (B[2,1]), (B[3,1]), (B[4,1]), alpha=0.9 , col="blue")
  
  planes3d((B[1,2]) , (B[2,2]) , (B[3,2]), (B[4,2]), alpha=0.9, col="red")
}
  
  
  
  
  