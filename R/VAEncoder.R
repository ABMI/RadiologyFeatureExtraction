library(dplyr)
library(keras)

buildRadiologyVae <- function(dataPaths = imagePaths, vaeValidationSplit = 0.2, vaeBatchSize = 200L,
                              vaeLatentDim = 1000L, vaeIntermediateDim = 10000L,
                              vaeEpoch = 500L, vaeEpislonStd = 1.0, dimensionOfTarget = 2,
                              dataFolder,
                              originalDimension = c(128, 128),
                              ROI2D = list(c(10:119), c(10:119)),
                              MaxLimitUnit = 1500,
                              samplingGenerator = FALSE) {
    
    if(dimensionOfTarget != 2)
        stop("Currently only dimesion of Target = 2 is avaiablbe")
    
    originalDim <- length(ROI2D[[1]]) * length(ROI2D[[2]])
    K <- keras::backend()
    x <- keras::layer_input(shape = originalDim)
    h <- keras::layer_dense(x, vaeIntermediateDim, activation = "relu")
    z_mean <- keras::layer_dense(h, vaeLatentDim)
    z_log_var <- keras::layer_dense(h, vaeLatentDim)
    
    sampling <- function(arg) {
        z_mean <- arg[, 1:vaeLatentDim]
        z_log_var <- arg[, (vaeLatentDim + 1):(2 * vaeLatentDim)]
        
        epsilon <- keras::k_random_normal(
            shape = c(keras::k_shape(z_mean)[[1]]),
            mean = 0.,
            stddev = vaeEpislonStd
        )
        
        z_mean + keras::k_exp(z_log_var / 2) * epsilon
    }
    
    z <- keras::layer_concatenate(list(z_mean, z_log_var)) %>%
        keras::layer_lambda(sampling)
    
    # We instantiate these layers separately so as to reuse them later
    decoder_h <- keras::layer_dense(units = vaeIntermediateDim, activation = "relu")
    decoder_mean <- keras::layer_dense(units = originalDim, activation = "sigmoid")
    h_decoded <- decoder_h(z)
    x_decoded_mean <- decoder_mean(h_decoded)
    
    # end-to-end autoencoder
    vae <- keras::keras_model(x, x_decoded_mean)
    # encoder, from inputs to latent space
    encoder <- keras::keras_model(x, z_mean)
    
    # generator, from latent space to reconstruted inputs
    decoder_input <- keras::layer_input(shape = vaeLatentDim)
    h_decoded_2 <- decoder_h(decoder_input)
    x_decoded_mean_2 <- decoder_mean(h_decoded_2)
    generator <- keras::keras_model(decoder_input, x_decoded_mean_2)
    
    vae_loss <- function(x, x_decoded_mean) {
        xent_loss <- (originalDim / 1.0) * keras::loss_binary_crossentropy(x, x_decoded_mean)
        k1_loss <- -0.5 * keras::k_mean(1 + z_log_var - keras::k_square(z_mean) - keras::k_exp(z_log_var), axis = -1L)
        xent_loss + k1_loss
    }
    # if (!is.null(dataValidation)) dataValidation <- list(dataValidation,dataValidation)
    vaeEarlyStopping <- keras::callback_early_stopping(monitor = "val_loss", patience = 10, mode = "auto", min_delta = 1e-1)
    vae %>% keras::compile(optimizer = "rmsprop", loss = vae_loss)
    
    # Paths for data
    actualPaths <- apply(imagePaths, 1, function(x) file.path(dataFolder, x))
    
    if(samplingGenerator) {
        # validation data
        valIndex <- sample(seq(actualPaths), length(actualPaths) * vaeValidationSplit)
        valImages <- lapply(as.array(actualPaths[valIndex]), function(x) {
            try({
                x <- oro.dicom::dicom2nifti(oro.dicom::readDICOM(x, verbose = FALSE))
                x <- EBImage::resize(x, w = originalDimension[1], h = originalDimension[2])[ROI2D[[1]], ROI2D[[2]] ]
            }, silent = T)
        })
        valImages <- array(unlist(valImages), dim = c(originalDimension, length(valImages)))
        valImages <- valImages %>% apply(3, as.numeric) %>% t()
        
        ## Regularization the data with the max ##
        valImages[is.na(valImages)] <- 0
        valImages <- ifelse(valImages > MaxLimitUnit, MaxLimitUnit, valImages)
        valImages <- valImages / MaxLimitUnit
        
        sampling_generator <- function(dataPath, batchSize, MaxLimitUnit) {
            function() {
                # gc()
                index <- sample(length(dataPath), batchSize, replace = FALSE)
                data.mat <- as.array(dataPath[index])
                images <- lapply(data.mat, function(x) {
                    try({
                        x <- oro.dicom::dicom2nifti(oro.dicom::readDICOM(x, verbose = FALSE))
                        x <- EBImage::resize(x, w = originalDimension[1], h = originalDimension[2]) [ROI2D[[1]], ROI2D[[2]] ]
                    }, silent = T)
                })
                images <- array(unlist(images), dim = c(ROI2D[[1]], ROI2D[[2]], length(images)))
                images <- images %>% apply(3, as.numeric) %>% t() # Error
                
                images[is.na(images)] <- 0
                images <- ifelse(images > MaxLimitUnit, MaxLimitUnit, images)
                images <- images / MaxLimitUnit
                list(images, images)
            }
        }
        
        vae %>% keras::fit_generator(
            sampling_generator(actualPaths[-valIndex], vaeBatchSize, MaxLimitUnit),
            steps_per_epoch = length(actualPaths[-valIndex]) / vaeBatchSize
            , epochs = vaeEpoch
            , validation_data = list(valImages, valImages)
            , callbacks = list(vaeEarlyStopping)
        )
    } else {
        data.mat <- as.array(actualPaths)
        images <- lapply(data.mat, function(x) {
            try({
                x <- oro.dicom::dicom2nifti(oro.dicom::readDICOM(x, verbose = FALSE))
                x <- EBImage::resize(x, w = originalDimension[1], h = originalDimension[2]) [ROI2D[[1]], ROI2D[[2]] ]
            }, silent = T)
        })
        images <- array(unlist(images), dim = c(length(ROI2D[[1]]), length(ROI2D[[2]]), length(images)))
        images <- images %>% apply(3, as.numeric) %>% t() # error seems vector size upper 2.0 GB
        
        # saveRDS(images,file.path(dataFolder,"image64.rds"))
        # images<-readRDS(file.path(dataFolder,"image.rds"))
        
        images[is.na(images)] <- 0
        images <- ifelse(images > MaxLimitUnit, MaxLimitUnit, images)
        images <- images / MaxLimitUnit
        
        vae %>% keras::fit(
            images, images,
            shuffle = TRUE,
            epochs = vaeEpoch,
            batch_size = vaeBatchSize,
            validation_split = vaeValidationSplit,
            callbacks = list(vaeEarlyStopping)
        )
    }
    return(list(vae = vae, encoder = encoder, MaxLimitUnit = MaxLimitUnit, vaeBatchSize = vaeBatchSize, vaeLatentDim = vaeLatentDim))
}

testVae <- function(dataPaths = imagePaths, vaeBatchSize = 200L,
                    vaeLatentDim = 1000L, vaeIntermediateDim = 10000L,
                    dimensionOfTarget = 2,
                    dataFolder,
                    originalDimension = c(128, 128),
                    ROI2D = list(c(10:119), c(10:119)),
                    MaxLimitUnit = 1500,
                    samplingN = 10,
                    vae) {
    if(dimensionOfTarget != 2)
        stop("Currently only dimesion of Target = 2 is avaiablbe")
    
    # Original dimenstion calculator
    originalDim <- length(ROI2D[[1]]) * length(ROI2D[[2]])
    
    # Paths for data
    actualPaths <- apply(imagePaths, 1, function(x) file.path(dataFolder, x))
    actualPaths <- actualPaths[seq(samplingN)]
    # actualPaths <- actualPaths[732:832]
    
    data.mat <- as.array(actualPaths)
    
    images <- lapply(data.mat, function(x) {
        try({
            x <- oro.dicom::dicom2nifti(oro.dicom::readDICOM(x, verbose = FALSE))
            x <- EBImage::resize(x, w = originalDimension[1], h = originalDimension[2]) [ROI2D[[1]], ROI2D[[2]] ]
        }, silent = T)
    })
    images <- array(unlist(images), dim = c(length(ROI2D[[1]]), length(ROI2D[[2]]), length(images)))
    images <- images %>% apply(3, as.numeric) %>% t() # error seems vector size upper 2.0 GB
    
    images[is.na(images)] <- 0
    images <- ifelse(images > MaxLimitUnit, MaxLimitUnit, images)
    images <- images / MaxLimitUnit
    
    mergeImage <- function(nifs) {
        resList <- NA
        for(i in 1:dim(nifs)[1]) {
            nif <- oro.nifti::as.nifti(nifs[i,,])
            if(is.na(resList))
                resList <- nif
            else
                resList <- abind::abind(resList, nif, along = 3)
        }
        return(resList)
    }
    
    originalImages <- array(unlist(images), dim = c(samplingN, length(ROI2D[[1]]), length(ROI2D[[2]])))
    
    # Original image view
    # oro.nifti::image(oro.nifti::as.nifti(mergeImage(originalImages)))
    oro.nifti::image(oro.nifti::as.nifti(originalImages[5,,]))
    
    predicted <- predict(VAE$vae, images, batch_size = vaeBatchSize)
    predictedImages <- array(unlist(predicted), dim = c(samplingN, length(ROI2D[[1]]), length(ROI2D[[2]])))
    
    # Predicted image view
    return(mergeImage(predictedImages))
}
