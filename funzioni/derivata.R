# Derivation par rapport à un point y
derivata_seba <- function(y, cn, n ){
  D = length(y)
  vi = veronese_indexes(n,D)
  Dpn = matrix(ncol=nrow(vi), nrow=D) 
  
  for (i in 1:nrow(vi)){
    for(j in 1:length(y)){
      Dpn[j,i]=( (vi[i,j])*y[j]^(vi[i,j]-1) ) * prod( ( y[-j]^(vi[i,-j]) ))
    }
  }
  
  matrix()
  
  return(Dpn)
}