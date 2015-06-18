uploadData <- function(file){
  df<-read.csv(file, sep=",", head=TRUE);
  #remove date column
  df_1<-df[,-(1)]
  #feature scaling
  bike_df<-scaleFeatures(df_1, c('temp','atemp','humidity', 'windspeed','casual','registered', 'count'));  
  bike_df;
}

scaleFeatures<-function(df_1,features){  
  for (i in 1:length(features)){
    df_1[features[i]]  <- ( ( df_1[features[i]][,1] - mean(df_1[features[i]][,1]) ) / sd(df_1[features[i]][,1]) );
  }
  df_1;
} 
