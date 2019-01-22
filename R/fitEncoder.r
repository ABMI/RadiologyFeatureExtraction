# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of RadiologyFeatureExtraction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Fit Vanilla autoencoder
#' 
#' 
#' @export
fitVanillaAutoencoder <- function(trainData,
                                  valProp,
                                  epochs = epochs,
                                  batch_size = batch_size,
                                  latentDim = latentDim,
                                  optimizer = 'adadelta', 
                                  loss = 'binary_crossentropy',
                                  imageProcessingSettings = imageProcessingSettings){
    K <- keras::backend()
    startTime <- Sys.time()
    
    originalDim <- c(length(imageProcessingSettings$roiWidth)*length(imageProcessingSettings$roiHeight))
    
    # Splitting train and validation
    if(valProp==0){
        valData<-trainData
    } else {
        valInd<-sample(nrow(trainData) ,round(valProp*nrow(trainData),0 ))
        
        valData <- trainData[valInd,]
        trainData <- trainData [-valInd,]
    }
    
    
    # Model definition --------------------------------------------------------
    input_layer <- keras::layer_input(shape = originalDim)
    # "encoded" is the encoded representation of the input
    encoded<- input_layer %>% 
        keras::layer_dense(latentDim , activation = "relu")
    # "decoded" is the lossy reconstruction of the input
    decoded<- encoded %>% 
        keras::layer_dense(originalDim , activation = "sigmoid")
    
    # this model maps an input to its reconstruction
    autoencoder <- keras::keras_model(input_layer, decoded)
    #Let's also create a separate encoder model:
    # this model maps an input to its encoded representation
    encoder <- keras::keras_model(input_layer, encoded)
    
    #As well as the decoder model:
    # create a placeholder for an encoded (with latent dimension) input
    encodedInput <- keras::layer_input(shape = c(latentDim))
    
    # retrieve the last layer of the autoencoder model
    #decoderLayer = keras::get_layer(autoencoder,index=-1)
    # create the decoder model
    #decoder <- keras::keras_model(encodedInput, decoderLayer(encodedInput))
    
    autoencoder  %>% compile(optimizer = optimizer, loss = loss)
    
    history<-autoencoder %>% fit (trainData, trainData,
                                  epochs=epochs,
                                  batch_size=batch_size,
                                  shuffle=TRUE,
                                  validation_data=list(valData, valData))
    
    encoderModel<-list(encoderModel = autoencoder,
                       encoder = encoder,
                       history = history#,
                       #decoder = decoder
                       )
    
    class(encoderModel) <- "encoderModel"
    delta <- Sys.time() - startTime
    print(delta)
    return(encoderModel)
}


#' Fit 2-dimensional autoencoder
#' 
#' 
#' @export
fit2DConvAutoencoder <- function(trainData,
                                 valProp,
                                 epochs = epochs,
                                 batch_size = batch_size,
                                 layer = layer,
                                 optimizer = 'adadelta', 
                                 loss = 'binary_crossentropy',
                                 imageProcessingSettings = imageProcessingSettings){
    K <- keras::backend()
    startTime <- Sys.time()
    
    originalDim <- c(length(imageProcessingSettings$roiWidth),length(imageProcessingSettings$roiHeight),1)
    
    # Splitting train and validation
    if(valProp==0){
        valData<-trainData
    } else {
        valInd<-sample(nrow(trainData) ,round(valProp*nrow(trainData),0 ))
        
        valData <- trainData[valInd,,]
        trainData <- trainData [-valInd,,]
    }
    
    #add dimension for channel
    dim(trainData)<-c(dim(trainData),1)
    dim(valData)<-c(dim(valData),1)

    #define the model
    input_layer <- 
        keras::layer_input(shape = originalDim) 
    
    encoded<-
        input_layer %>%
        layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = 'relu',padding='same')%>% 
        layer_max_pooling_2d(pool_size = c(2, 2),padding='same')%>% 
        layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',padding='same')%>% 
        layer_max_pooling_2d(pool_size = c(2, 2),padding='same')%>% 
        layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',padding='same')%>% 
        layer_max_pooling_2d(pool_size = c(2, 2),padding='same')
    
    decoded <-
        encoded %>%
        layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',padding='same')%>% 
        layer_upsampling_2d(size = c(2, 2))%>%
        layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',padding='same')%>% 
        layer_upsampling_2d(size = c(2, 2))%>%
        layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = 'relu',padding='same')%>% 
        layer_upsampling_2d(size = c(2, 2))%>%
        layer_conv_2d(filters = 1, kernel_size = c(3,3), activation = 'sigmoid',padding='same') 
    
    # this model maps an input to its reconstruction
    autoencoder <- keras::keras_model(input_layer, decoded)
    #summary(autoencoder)
    
    #Let's also create a separate encoder model:
    # this model maps an input to its encoded representation
    encoder <- keras::keras_model(input_layer, encoded)
    
    #As well as the decoder model:
    # create a placeholder for an encoded (with latent dimension) input
    #encodedInput <- keras::layer_input(shape = c(latentDim))
    
    # retrieve the last layer of the autoencoder model
    #decoderLayer = keras::get_layer(autoencoder,index=-1)
    # create the decoder model
    #decoder <- keras::keras_model(encodedInput, decoderLayer(encodedInput))
    
    autoencoder  %>% compile(optimizer = optimizer, loss = loss)
    
    history<-autoencoder %>% fit (trainData, trainData,
                                  epochs=epochs,
                                  batch_size=batch_size,
                                  shuffle=TRUE,
                                  validation_data=list(valData, valData))
    
    encoderModel<-list(encoderModel = autoencoder,
                       encoder = encoder,
                       history = history#,
                       #decoder = decoder
    )
}
