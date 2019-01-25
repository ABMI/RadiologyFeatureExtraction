RadiologyFeatureExtraction
==============================


Requirements
============

- A database in [Common Data Model version 5](https://github.com/OHDSI/CommonDataModel) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, or Microsoft APS.
- R version 3.5.0 or newer
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com) (openjdk 1.8 recommended)
- 25 GB of free disk space

See [this video](https://youtu.be/K9_0s2Rchbo) for instructions on how to set up the R environment on Windows.



## Learning Requirements

* Python 3
* Minimum RAM (**this is sampleCT data only**): 16.0 GB (32.0 GB recommended)
* Keras, Tensorflow 



How to run
==========
1. In `R`, use the following code to install the dependencies:

   ```R
   library(devtools)
   install_github("ohdsi/SqlRender", ref = "v1.5.2")
   install_github("ohdsi/DatabaseConnector", ref = "v2.2.0")
     
   #Install EBImage
   source("http://bioconductor.org/biocLite.R")
   biocLite("EBImage")
   ```

   If you experience problems on Windows where rJava can't find Java, one solution may be to add `args = "--no-multiarch"` to each `install_github` call, for example:

   ```R
   install_github("ohdsi/SqlRender", args = "--no-multiarch")
   ```

   Alternatively, ensure that you have installed both 32-bit and 64-bit JDK versions, as mentioned in the [video tutorial](https://youtu.be/K9_0s2Rchbo).

2. If you want to use Sample CT provided by this project, please use the code below.

   ```R
   load(file.path(getwd(), 'extra/sampleCT.rda'))
   ```

   Sample CT data is in nifti format, and both the image width and height are 128.
