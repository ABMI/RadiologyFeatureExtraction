library(keras)

#load sample image
mnist<-keras::dataset_mnist()
tainData<-mnist$train$x
testData<-mnist$test$x
tainData<-tainData[1:4000,,]
testData <- testData[1:1000,,]
dim(testData)
dim(tainData)

#View the original image
sampleN <- 8
par(mfcol=c(2,4))
for(i in seq(sampleN)){
    image(tainData[i,,],col=gray(12:1/12))
}

imageProcessingSettings<-SetImageProcessing(normalization="MinMaxNorm",
                                            maxLimit = 255,
                                            minLimit = 0,
                                            width = 24,
                                            height = 24,
                                            roiWidth = 2:23,
                                            roiHeight = 2:23,
                                            channelDim = NULL,
                                            indexDim = 1)
#apply image processing to the array along the margin 1
tainData<-plyr::alply(tainData,imageProcessingSettings$indexDim,function(x){
    x<-preProcessing(x,imageProcessingSettings)
})
#Visualize processed image
par(mfcol=c(2,4))
for(i in seq(sampleN)){
    image(processed.image[[i]],col=gray(12:1/12))
}

melted.image<-meltDim(tainData,imageProcessingSettings)
dim(melted.image)

autoencoder<-fitVanillaAutoencoder(melted.image,
                                   melted.image,
                                   epochs = 10L,
                                   batch_size = 100L,
                                   latentDim = 32L,
                                   optimizer = 'adadelta', 
                                   loss = 'binary_crossentropy',
                                   imageProcessingSettings = imageProcessingSettings
)


testData.processed<-plyr::alply(testData,imageProcessingSettings$indexDim,function(x){
    x<-preProcessing(x,imageProcessingSettings)
})
#Visualize processed image
par(mfcol=c(2,4))
for(i in seq(sampleN/2)){
    image(testData.processed[[i]],col=gray(12:1/12))
}

testData.melted<-meltDim(testData.processed, imageProcessingSettings)
predicted <- predict(autoencoder$encoderModel, testData.melted, batch_size = 100L)
predictedImages <-reconDim(predicted,imageProcessingSettings)
predictedImages <- reverseProcessing(predictedImages)

for(i in seq(sampleN/2)){
    image(predictedImages[i,,],col=gray(12:1/12))
}
