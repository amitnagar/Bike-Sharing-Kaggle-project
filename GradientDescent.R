#Gradient Descent implementation
#m - number of training example
#n - number of features
#X - training examples, m x n
#y - m x 1, outcome of training examples

gradientDescent <- function(dataSet, theta, alpha, num_iters){

  #features
  X<-data.frame(dataSet[,1:dim(dataSet)[2]-1]);
    
  #append X0 column, where, X0 = 1
  ones<-data.frame(rep(1,dim(dataSet)[1]));
  
  colnames(ones)<-c("X0")
  X["X0"]<-ones
  
  #reorder columns so X0 is 1st.  @To be improved
  X<-X[c("X0","season","holiday","workingday","weather", "temp", "atemp", "humidity", "windspeed", "casual", "registered")];
  
  #predictor variable
  y<-data.frame(dataSet[,dim(dataSet)[2]]);
  
  #add theta0 corresponding to X0
  theta<-cbind(theta,c(0))

  #initializations
  iter<- 1;
  jCost <- c();
  jndf <- data.frame;

  #note: first theta represents theta(0), which goes with X0 (column of 1s addeded above)
  tmpTheta<-matrix(data = c(0,0), nrow = 1, ncol = dim(dataSet)[2]);
  optCost   <-  c();
  optTheta  <- c();
  
  #repeat for numIters or till cost is monotonically decreasing, whichever comes first
  while(iter <= numIters ){
            
    # n features, jn - parameters to be learnt
    for ( jn in 1:dim(X)[2]){
      #pDerivative - partial derivative
      pDerivative <- 0;
      
      # m training examples, im - indexes over m
      for ( im in 1:dim(X)[1]) {
        pDerivative <- pDerivative + ((theta%*%t(as.matrix(X[im,]))) - as.matrix(y[im,1])) * X[im, jn];
      }  
      
      #alpha - learning rate
      tmpTheta[jn] <- theta[jn] - ( ( alpha / dim(X)[1] ) * pDerivative );
    }
    
    #Compute cost with new theta's (tmpTheta)
    tCost <- 0;
    for ( k in 1:dim(X)[1] ){
      tCost <- tCost + ( ( tmpTheta%*%t(as.matrix(X[k,])) - y[k,1] ) * ( tmpTheta%*%t(as.matrix(X[k,])) - y[k,1] ) ) ;
      
    }
    tCost <- tCost / (2 * dim(X)[1]);
    
    #compare cost of current iteration to prev iteration cost
    if ( (iter > 1) ) 
    {
      print(iter); print(tCost); print(jCost[iter - 1]);
      if (tCost < jCost[iter - 1]) {
        optCost  <- append(jCost,c(tCost));
        optTheta <- append(optTheta,tmpTheta)
        break;
      }
    }
      #add cost to history
      jCost <- append(jCost,c(tCost));
      jndf<- append(jndf,tmpTheta)
      
      #for next iteration
      
      iter <- iter + 1;
      theta<-tmpTheta;
      
  }  

print(optCost);
print(optTheta);

optTheta;

}
