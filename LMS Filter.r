featuresExtract<-function(inputTS, featuresCount=3,windowSize=5){
  features<-data.frame()
  normalisingFactor<<-max(inputTS)
  normalisedInputTS<-inputTS/normalisingFactor
  for(i in 1:((dim(inputTS)[1]-windowSize+1))){
    t<-normalisedInputTS[i:(i+windowSize-1),1]
    absolute<-t[windowSize]
    meanFeature<-mean(t)
    variance<-var(t)
    features<-rbind(features,c(absolute,meanFeature,variance,normalisedInputTS[(i+windowSize),1]))
  }
  colnames(features)<-c("absolute","mean","variance","result")
  return(features)
}


network_initialize <- function(inputs, layers=0, neurons=0){
  inputs<<-inputs
  neurons<<-neurons
  errorMatrix <<- data.frame(obsNo=integer(),output=double(),desired=double(),error=double())
  layer<<-data.frame(matrix(0, ncol = layers+1 ,nrow = max(neurons,inputs)),row.names = NULL)
  colnames(layer)<<-c(paste("layer",c(1:(layers+1)),sep=""))
  colnames(layer)[1]<<-"input"
}




LMSweightUpdate<-function(feature, layer, error, obs,mu){
  layer<-t(layer[,1] + (2 * mu * feature[obs,(1:((dim(feature)[2])-1))] * tail(error[,3],n=1)))
  return(layer)
}


init<-function(trainingSet, layer, error,mu){
  for(i in 1:((dim(trainingSet)[1])-1)){
    out<-as.matrix(trainingSet[i,1:3])%*%as.matrix(layer[,1])
    desired<-trainingSet[i,4]
    error<-rbind.data.frame(error,c(i,out,desired,(desired-out)))
    layer<-LMSweightUpdate(trainingSet, layer, error, i,mu)
  }
  errorMatrix<<-error
  return(layer)
}






data<-read.csv("usd-inr.csv",sep = ",")

data<-as.data.frame(data[2])
numberOfFeatures=3
windowSize=5
features<-featuresExtract(data ,featuresCount =  numberOfFeatures, windowSize = windowSize)


network_initialize(3)
layer<-init(features, layer, errorMatrix,mu=0.01)

