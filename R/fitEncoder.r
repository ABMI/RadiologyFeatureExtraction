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
                                  valData,
                                  epochs = epochs,
                                  batch_size = batch_size,
                                  latentDim = latentDim,
                                  optimizer = 'adadelta', 
                                  loss = 'binary_crossentropy',
                                  imageProcessingSettings = imageProcessingSettings){
    K <- keras::backend()
    startTime <- Sys.time()
    originalDim <- c(length(imageProcessingSettings$roiWidth)*length(imageProcessingSettings$roiHeight))
    
    # Model definition --------------------------------------------------------
    input <- keras::layer_input(shape = originalDim)
    # "encoded" is the encoded representation of the input
    encoded<- input %>% 
        keras::layer_dense(latentDim , activation = "relu")
    # "decoded" is the lossy reconstruction of the input
    decoded<- encoded %>% 
        keras::layer_dense(originalDim , activation = "sigmoid")
    
    # this model maps an input to its reconstruction
    autoencoder <- keras::keras_model(input, decoded)
    #Let's also create a separate encoder model:
    # this model maps an input to its encoded representation
    encoder <- keras::keras_model(input, encoded)
    
    #As well as the decoder model:
    # create a placeholder for an encoded (with latent dimension) input
    encodedInput <- keras::layer_input(shape = c(latentDim))
    
    # retrieve the last layer of the autoencoder model
    #decoderLayer = keras::get_layer(autoencoder,index=-1)
    # create the decoder model
    #decoder <- keras::keras_model(encodedInput, decoderLayer(encodedInput))
    
    autoencoder  %>% compile(optimizer = optimizer, loss = loss)
    
    autoencoder %>% fit (trainData, trainData,
                         epochs=50,
                         batch_size=batch_size,
                         shuffle=TRUE,
                         validation_data=list(valData, valData))
    
    encoderModel<-list(encoderModel = autoencoder,
                       encoder = encoder#,
                       #decoder = decoder
                       )
    
    class(encoderModel) <- "encoderModel"
    delta <- Sys.time() - startTime
    print(delta)
    return(encoderModel)
}
