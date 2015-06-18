testPrediction <- function(prediction,df_test){
  
testPredictions<-c();
  
  for ( i in 1:100 ){
    
    testPredictions<-append(testPredictions, t(prediction)%*%t(as.matrix(df_test[i,])));
    
  }

  print(testPredictions);
  
  
}