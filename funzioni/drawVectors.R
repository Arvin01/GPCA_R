couleurs=c("green", "blue", "red", "purple", "yellow", "grey")

# fonction qui trace les perpendiculaires aux points pour 2 dimensions
drawVects <- function(Xg, b , couleur ){
  if(is.vector(b)){
    for ( i in 1:nrow(Xg) ){
      lines(c( Xg[i,1] , Xg[i,1] + b[1]*10 ), c( Xg[i,2] , Xg[i,2] + b[2]*10 ) ,col=couleur) 
    }
  }
  else{
    Y = Xg+t(b)*10
    for ( i in 1:nrow(Xg) ){
      lines(c( Xg[i,1] , Y[i,1] ), c( Xg[i,2] , Y[i,2] ) ,col=couleur) 
    }
  }
}

drawVects3d <- function(Xg, b , couleur ){
  for (i in 1:nrow(Xg)){
    segments3d( c(Xg[i,1], Xg[i,1] + b[1] ) , c(Xg[i,2], Xg[i,2] + b[2]) , c(Xg[i,3], Xg[i,3] +  b[3]) ,col=couleur,lwd=2)
  }
}

index_matching_number <- function( m, obj ){
  c = rep( obj, length(m))
  s = pmatch(c, m)
  return(s[(!is.na(s))] ) 
}