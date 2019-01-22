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
#' @param batch_size
#' @param latentDim
#' @param latentDim
#' 
#' @export
setVanillaAutoencoder <- function(epochs = 10,
                                  batch_size = batch_size,
                                  latentDim = latentDim,
                                  optimizer = 'adadelta', 
                                  loss = 'binary_crossentropy',
                                  imageProcessingSettings = imageProcessingSettings
                                  ){
    encoderSettings <- list(model = 'fitVanillaAutoencoder',
                            epochs = as.integer(epochs),
                            batch_size = as.integer(batch_size),
                            latentDim = as.integer(latentDim),
                            optimizer = optimizer,
                            loss = loss,
                            imageProcessingSettings = imageProcessingSettings
                            )
    class(encoderSettings) <- 'encoderSettings'
}

