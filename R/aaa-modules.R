# Package-level Python module references
# Populated in .onLoad with delay-loaded imports
.osgeo <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {

  .osgeo$gnm  <- reticulate::import("osgeo.gnm",  delay_load = TRUE)
  .osgeo$gdal <- reticulate::import("osgeo.gdal", delay_load = TRUE)
  .osgeo$ogr  <- reticulate::import("osgeo.ogr",  delay_load = TRUE)
  .osgeo$osr  <- reticulate::import("osgeo.osr",  delay_load = TRUE)
  # Silence the GDAL 4.0 deprecation warning
  try(.osgeo$gdal$UseExceptions(), silent = TRUE)
}
