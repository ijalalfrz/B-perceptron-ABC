
library(data.table)
data <- fread("perceptron-data.csv")
data <- as.matrix(data)

#membuat fungsi
PLA <- function(data,lr){
  n = 63
  b1 = 0
  b2 = 0
  b3 = 0
  w1 = rep(0, n)
  w2 = rep(0, n)
  w3 = rep(0, n)
  err1 <- TRUE
  err2 <- TRUE
  err3 <- TRUE
  fnet1 = 0
  fnet2 = 0
  fnet3 = 0
  k=0
  
  while(err1 == TRUE | err2 == TRUE | err3 == TRUE){
    for(i in 1:nrow(data)){
      #net1
      #t untuk transpose ,%*% untuk perkalian matrix 
      net1 = t(w1) %*% data[i,1:63] + b1
      #net2
      net2 = t(w2) %*% data[i,1:63] + b2
      #net3
      net3 = t(w3) %*% data[i,1:63] + b3
     
      
      #fnet1
      if(net1>0.5) fnet1 = 1
      else if(net1<=0.5 && net1>=-0.5) fnet1 = 0
      else if (net1<=0.5) fnet1 = -1
      
      #fnet2
      if(net2>0.5) fnet2 = 1
      else if(net2<=0.5 && net2>=-0.5) fnet2 = 0
      else if (net2<=0.5) fnet2 = -1
      
      #fnet3
      if(net3>0.5) fnet3 = 1
      else if (net3<=0.5 && net3>=-0.5) fnet3 = 0
      else if (net3<=0.5) fnet3 = -1
      
      #cek fnet1
      if(fnet1 == data[i,64]) err1 = FALSE
      else{
        err1 = TRUE
      }
      #update w
      w1 <- w1 + lr*data[i,64]*data[i,1:63]
      #update b
      b1 <- b1 + lr*data[i,64]
      
      #cek fnet2
      if(fnet2 == data[i,65]) err2 = FALSE
      else{
        err2 = TRUE
      }
      #update w
      w2 <- w2 + lr*data[i,65]*data[i,1:63]
      #update b
      b2 <- b2 + lr*data[i,65]
      
      #cek fnet3
      if(fnet3 == data[i,66]) err3 = FALSE
      else{
        err3 = TRUE
      }
      #update w
      w3 <- w3 + lr*data[i,66] * data[i,1:63]
      #update b
      b3 <- b3 + lr*data[i,66]
      
      
      #print hasil
      k<-k+1
      cat("\n",k,"\n")
      cat("1",net1,"\n")
      cat("2",net2,"\n")
      cat("3",net3,"\n")
      
      cat("FNET\n")
      cat("1",fnet1,"\n")
      cat("2",fnet2,"\n")
      cat("3",fnet3,"\n")
      
      cat("BIAS\n")
      cat("1",b1,"\n")
      cat("2",b2,"\n")
      cat("3",b3,"\n")
  
     
    }

    
  }
  
  #return sebagai data frame
  res<-as.data.frame(rbind(w1,w2,w3))
  return(res)
  
}