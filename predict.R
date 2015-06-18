#read data from CSV file
source("dataUtils.R");
bike_df<-uploadData("C:\\temp\\train.csv");

#create train / test set
df_train<-bike_df[1:(nrow(bike_df)*.5),]
df_test<-bike_df[((nrow(bike_df)*.5)+1):nrow(bike_df),]

source("Gradientdescent.R");
#initializations
initialTheta<-matrix(data = c(0,0), nrow = 1, ncol = dim(bike_df)[2]-1);
alpha<-0.05;
numIters<-20;

source("GradientDescent.R");
#gradient descent
prediction<-gradientDescent(df_train, initialTheta, alpha, numIters);

#validate w/ Test data
source("TestWorkbench.R");
testPrediction(prediction,df_test);