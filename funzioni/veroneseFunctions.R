
###################################
######  FONCTIONS  ###########
###################################

# puissances utiles à la Carte de veronese
veronese_indexes<-function(n,D){
  if(D==1){
    v=n
  }
  else{
    y=list()
    for (i in 1:D ){
      y[[i]]=0:n
    }
    z=expand.grid(y)
    v=z[rowSums(z)==n,]
    for(j in D:1){
      v=v[order(v[,j], decreasing = TRUE),]    
    }
  }
  return(v)
}


# Carte de Veronese
veronese_map <- function(X, n){
  D=ncol(X)
  vi = veronese_indexes(n,D)
  vn = matrix(ncol=nrow(vi), nrow=nrow(X)) 
  for(i in 1:nrow(X)){
    for (j in 1:nrow(vi)){
      vn[i,j]=prod(X[i,]^vi[j,])
    }
  }
  vn=t(vn)
  return(vn)
}