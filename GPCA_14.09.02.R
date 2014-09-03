getwd()
setwd("C:/Users/Sebastian/Documents/Informatique/machineLearning/documenti_memoire/VidalVsSeba/funzioneFinale")

library(MASS)
library(ade4)
library(scatterplot3d)
library(gdata)
library(rgl)

source("funzioni/veroneseFunctions.R")
source("funzioni/derivata.R")



sebaGPCA <- function(X,n,Y, spectral = FALSE){
  

  ###################################
  ##########  Execution  ###########
  ###################################
  
  
  D <- ncol(X)
  N <- nrow(X)
  Mn <- choose(n+D-1 , D-1)
  
  ### Veronese Map
  vi = veronese_indexes(n,D)
  vn = veronese_map(X,n)
  
  ####	Left Null Space
  
  cn = svd(vn)$u[,Mn ]
  cn <- as.matrix(cn)
  
  
  l=1
  print( round(t(cn)%*%vn, 10) )
  
  print("USV :")
  print( svd(vn)$u%*%diag(svd(vn)$d)%*%t(svd(vn)$v) )
  
  
  ###  Differentiation A LA FACON DE VIDAL
  
  #matrix( rep(cn, D) , nrow=Mn, ncol=D )	# == (c*ones(1,K))
  Dc = matrix( cn[,l], Mn, D )*vi			# == (c*ones(1,K)).*powers
  Dc = Dc[(vi!=0)]					# == Dc=Dc(:);  Dc = Dc(find(powers>0));
  Dc = matrix(Dc, length(Dc)/D, D)
  Dp = t(Dc)%*%veronese_map(X,n-1)
  round(rowSums(t(Dp)*X) , 10)
  
  
  ###	Normalisation
  
  Dpn = Dp/matrix(sqrt(colSums(Dp^2)), nrow=nrow(Dp), ncol =ncol(Dp), byrow=TRUE)
  X_norm = t(X)/matrix(sqrt(colSums(t(X)^2)), nrow=nrow(t(X)), ncol =ncol(t(X)), byrow=TRUE)
  
  ###	Analyse en Composantes principales
  couleurs=c("green", "blue", "red", "purple", "yellow", "grey")
  
  
  B <- matrix(ncol=n, nrow=D)
  for(i in 1:n){
    B[,i] = prcomp(t(Dp[,Y[,i]]), center = F, scale = F, tol=0.1)$rotation[,1]
  }
  
  ###attribution aux groupes
  
  Dgroup = matrix( ncol=n, nrow=N)
  for (i in 1:n){
    Dgroup[,i] = apply( abs((t(X_norm) %*%   (B[,i]))), 1, function(x) min(x) )
  }
  group = apply( Dgroup, 1, function(x) match(min(x), x) )
  group =as.matrix(group)
  
  results =list()
  results$Mn = Mn
  results$vn=vn
  results$vi = vi
  results$cn = cn
  results$Dpn = Dpn
  results$B = B
  results$group = t(group)
  
  if(spectral==TRUE){
    k=D
    
    affMat <- abs ( t(Dpn) %*% Dpn )
    diag(rowSums(affMat)^(-0.5))
    
    diagMat <- diag(rowSums(affMat)^(-0.5))
    
    LMat <- diagMat %*% affMat %*% diagMat
    
    v = svd(LMat)$v[,c(1:k)]
    d = svd(LMat)$d[c(1:k)]
    v
    d
    
    X = v
    X
    
    normX <- rowSums(X^2)^0.5
    Y <- X / matrix(normX, ncol=dim(X)[2] , nrow =  dim(X)[1])
    
    results$kcluster = t(as.matrix(kmeans(Y, n)$cluster))
    results$kcenters = kmeans(Y, n)$centers
  }
  
  
  return(results)
}