# DB Connection
details <- DatabaseConnector::createConnectionDetails(
    dbms = Sys.getenv("dbms"),
    user = Sys.getenv("user"),
    password = Sys.getenv("pw"),
    server = Sys.getenv("server")
)
connection <- DatabaseConnector::connect(details)

sql <- "SELECT im.image_filepath
FROM @cdm_database_schema.Radiology_Image_QUER im
JOIN @cdm_database_schema.Radiology_Occurrence oc
ON im.radiology_occurrence_id = oc.radiology_occurrence_id
WHERE radiology_phase_concept = '@radiology_phase_concept'
{@cohort_id != -1} ? {AND im.person_id not in (SELECT subject_id from @cohort_table where cohort_definition_id = @exclude_cohort_id)}
AND Image_type = '@image_type'
AND phase_total_no >= @min_resolution_depth
AND image_resolution_rows >= @min_resolution_width
AND image_resolution_columns >= @min_resolution_hight
-- AND oc.radiology_phase_concept_id =@radiology_phase_concept_id
;"
sql <- SqlRender::renderSql(sql,
                            cdm_database_schema = "Radiology_CDM_QUER.dbo",
                            cohort_table = "cohort",
                            # radiology_phase_concept_id = 0,
                            radiology_phase_concept = "Pre contrast",
                            image_type = "PRIMARY",
                            min_resolution_depth = 10,
                            min_resolution_width = 256,
                            min_resolution_hight = 256,
                            cohort_id = -1,
                            exclude_cohort_id = -1)$sql
sql <- SqlRender::translateSql(sql, targetDialect = details$dbms)$sql
imagePaths <- DatabaseConnector::querySql(connection, sql)

VAE <- buildRadiologyVae(
    dataPaths = imagePaths, vaeValidationSplit = 0.2, vaeBatchSize = 200L,
    vaeLatentDim = 1000L, vaeIntermediateDim = 2000L,
    vaeEpoch = 500L, vaeEpislonStd = 1.0, dimensionOfTarget = 2,
    dataFolder = "/data/Radiology",
    originalDimension = c(128, 128),
    ROI2D = list(c(10:119), c(10:119)),
    MaxLimitUnit = 1500,
    samplingGenerator = FALSE
)

save_model_hdf5(VAE$encoder, filepath = "/data/Radiology/quer.ai_result/encoder_sec.hdf5")

# PyCapsule issue
# save_model_hdf5(VAE$vae, filepath = "/data/Radiology/quer.ai_result/vae.hdf5")

vae <- keras::load_model_weights_hdf5(filepath = "/data/Radiology/quer.ai_result/vae_sec.hdf5")

p_img <- testVae(dataFolder = '/data/Radiology', vae = VAE$vae, samplingN = 10, vaeBatchSize = 200L,
                 vaeLatentDim = 1000L, vaeIntermediateDim = 2000L, ROI2D = list(c(10:119), c(10:119)))
dim(p_img)

oro.nifti::image(oro.nifti::as.nifti(p_img))
