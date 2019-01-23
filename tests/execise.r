library(keras)

#load sample image
mnist<-keras::dataset_mnist()
trainData<-mnist$train$x
testData<-mnist$test$x

##hyperParameterSetting

trainData<-trainData[1:4000,,]
testData <- testData[1:1000,,]

#View the original image
trainSampleIndex <- sample(nrow(trainData),8)
testSampleIndex <- sample(nrow(testData),4)
sampleViewPanels <- c(2,4)

##Hyperparameter
batch_size = 100
latentDim = 10
epochs = 10

#View function
grayViewer<-function(images,
                     sampleIndex = NULL,
                     sampleViewPanels){
    for(i in sampleIndex){
        image(images[i,,],col=gray(12:1/12))
    }
}
par(mfcol=sampleViewPanels)
grayViewer(trainData,1:8,sampleViewPanels)


####For 1D autoencoder####

#image preprocessing setting
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
trainData.processed<-plyr::alply(trainData,imageProcessingSettings$indexDim,function(x){
    x<-preProcessing(x,imageProcessingSettings)
})

testData.processed<-plyr::alply(testData,imageProcessingSettings$indexDim,function(x){
    x<-preProcessing(x,imageProcessingSettings)
})

#melt dimension
trainData.melted<-meltDim(trainData.processed,imageProcessingSettings)
testData.melted<-meltDim(testData.processed,imageProcessingSettings)

#build autoencoder
encoderSetting<-setVanillaAutoencoder (valProp = 0.3,
                                       epochs = 10,
                                       batch_size = batch_size,
                                       latentDim = latentDim,
                                       optimizer = 'adadelta', 
                                       loss = 'binary_crossentropy',
                                       imageProcessingSettings = imageProcessingSettings)

autoencoder<-fitVanillaAutoencoder(trainData=trainData.melted,
                                   valProp = encoderSetting$valProp,
                                   epochs = encoderSetting$epochs,
                                   batch_size = encoderSetting$batch_size,
                                   latentDim = encoderSetting$latentDim,
                                   optimizer = encoderSetting$optimizer, 
                                   loss = encoderSetting$loss,
                                   imageProcessingSettings = encoderSetting$imageProcessingSettings
)


##Prediction
predicted <- predict(autoencoder$encoderModel, testData.melted, batch_size = 100L)
predictedImages <-reconDim(predicted,imageProcessingSettings)
predictedImages <- reverseProcessing(predictedImages)

par(mfcol=sampleViewPanels)
grayViewer(testData,
           sampleIndex = 1:4,
           sampleViewPanels)
grayViewer(predictedImages,
           sampleIndex = 1:4,
           sampleViewPanels)


####For convolutional autoencoder####

#image preprocessing setting
imageProcessingSettings<-SetImageProcessing(normalization="MinMaxNorm",
                                            maxLimit = 255,
                                            minLimit = 0,
                                            width = 24,
                                            height = 24,
                                            roiWidth = 1:24,
                                            roiHeight = 1:24,
                                            channelDim = NULL,
                                            indexDim = 1)

#apply image processing to the array along the margin 1
trainData.processed<-plyr::alply(trainData,imageProcessingSettings$indexDim,function(x){
    x<-preProcessing(x,imageProcessingSettings)
})

testData.processed<-plyr::alply(testData,imageProcessingSettings$indexDim,function(x){
    x<-preProcessing(x,imageProcessingSettings)
})

#aperm dimension
trainData.ordered<-meltDim(trainData.processed,convolution=T,imageProcessingSettings)
testData.ordered<-meltDim(testData.processed,convolution=T,imageProcessingSettings)
dim(trainData.ordered)
dim(testData.ordered)


#build autoencoder
encoderSetting<-set2DConvAutoencoder (valProp = 0.3,
                                      epochs = epochs,
                                      batch_size = batch_size,
                                      poolingLayerNum = 3,
                                      poolSize = 2,
                                      optimizer = 'adadelta', 
                                      loss = 'binary_crossentropy',
                                      imageProcessingSettings = imageProcessingSettings)

debug(fit2DConvAutoencoder)
autoencoder<-fit2DConvAutoencoder(trainData=trainData.ordered,
                                  valProp = encoderSetting$valProp,
                                  epochs = encoderSetting$epochs,
                                  batch_size = encoderSetting$batch_size,
                                  poolingLayerNum = encoderSetting$poolingLayerNum,
                                  poolSize = encoderSetting$poolSize,
                                  optimizer = encoderSetting$optimizer, 
                                  loss = encoderSetting$loss,
                                  imageProcessingSettings = encoderSetting$imageProcessingSettings
)

##Prediction
predicted <- predict(autoencoder$encoderModel, testData.ordered, batch_size = 100L)

predictedImages <-reconDim(predicted,convolution=TRUE,imageProcessingSettings)
predictedImages <- reverseProcessing(predictedImages)

par(mfcol=sampleViewPanels)
grayViewer(testData,
           sampleIndex = 1:4,
           sampleViewPanels)
grayViewer(predictedImages,
           sampleIndex = 1:4,
           sampleViewPanels)
