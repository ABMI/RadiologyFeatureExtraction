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

#' Creating setting for Vanilla autoencoder
#' @param batchSize
#' @param latentDim
#' @param latentDim
#' 
#' @export
setVanillaAutoencoder <- function(valProp = 0.3,
                                  epochs = 10,
                                  batchSize = batchSize,
                                  latentDim = latentDim,
                                  optimizer = 'adadelta', 
                                  loss = 'binary_crossentropy',
                                  imageProcessingSettings = imageProcessingSettings
                                  ){
    if(valProp>1 | valProp<0)
        stop ("valProp should be between 0 and 1")
    
    encoderSettings <- list(model = 'fitVanillaAutoencoder',
                            valProp = valProp,
                            epochs = as.integer(epochs),
                            batchSize = as.integer(batchSize),
                            latentDim = as.integer(latentDim),
                            optimizer = optimizer,
                            loss = loss,
                            imageProcessingSettings = imageProcessingSettings
                            )
    class(encoderSettings) <- 'encoderSettings'
    return(encoderSettings)
}

#' Creating setting for 2-dimension convolutional autoencoder
#' @param batchSize
#' @param latentDim
#' @param latentDim
#' 
#' @export
set2DConvAutoencoder<-function(valProp = 0.3,
                               epochs = 10,
                               batchSize = batchSize,
                               poolingLayerNum = 3,
                               kernelSize = 3,
                               poolSize = 2,
                               optimizer = 'adadelta', 
                               loss = 'binary_crossentropy',
                               imageProcessingSettings = imageProcessingSettings){
    if(valProp>1 | valProp<0)
        stop ("valProp should be between 0 and 1")
    
    if ( !(length(imageProcessingSettings$roiWidth) %/% (poolSize^poolingLayerNum)==0) & (length(imageProcessingSettings$roiHeight) %/% (poolSize^poolingLayerNum)==0) )
        stop ("the length of roiWidth and roiHeight should be multipliers of (poolSize)^(layerNum)")
    
    encoderSettings <- list(model = 'fit2DConvAutoencoder',
                            valProp = valProp,
                            epochs = as.integer(epochs),
                            batchSize = as.integer(batchSize),
                            poolingLayerNum = as.integer(poolingLayerNum),
                            kernelSize = as.integer(kernelSize),
                            poolSize = as.integer(poolSize),
                            #latentDim = as.integer(latentDim),
                            optimizer = optimizer,
                            loss = loss,
                            imageProcessingSettings = imageProcessingSettings
    )
    class(encoderSettings) <- 'encoderSettings'
    return(encoderSettings)
    
}
