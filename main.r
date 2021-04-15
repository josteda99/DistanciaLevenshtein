distancia<-function(str1,str2){
  s1 <-  strsplit(str1, "")
  s2 <-  strsplit(str2, "")
  distance <- matrix(0:0, nrow = lengths(s1)+1, ncol = lengths(s2)+1)

  for (i in 0:lengths(s1)) {
    distance[i+1,1]<-i
  } 

  for (j in 1:lengths(s2)+1) {
    distance[1,j]<-j
  } 

  for(i in 2:lengths(s1)+1){ 
    for(j in 1:lengths(s2)+1){
      if(s1[[1]][i-1]==s2[[1]][j-1]){
        aux <- 0
      }else{
        aux <- 1
      }
      distance[i,j] = min(distance[i-1,j]+1,distance[i,j-1]+1,distance[i-1,j-1]+(aux))
    }
  }
  print(distance)
  print(distance[lengths(s1)+1,lengths(s2)]+1)
}


print("ingrese primer palabra:")
str1<-scan(,what=character(),1)
print("ingrese segunda palabra:")
str2<-scan(,what=character(),1)
distancia(str1,str2)