# Test helpers for osgeo0

#' Skip tests if GNM is not available
skip_if_no_gnm <- function() {
  testthat::skip_if_not_installed("reticulate")
  testthat::skip_if(!reticulate::py_available(initialize = TRUE),
                    "Python not available")

  tryCatch({
    gnm <- reticulate::import("osgeo.gnm")
    gdal <- reticulate::import("osgeo.gdal")
    drv <- gdal$GetDriverByName("GNMFile")
    if (is.null(drv)) testthat::skip("GNM driver not available in GDAL")
  }, error = function(e) {
    testthat::skip(paste("osgeo.gnm not available:", e$message))
  })
}


#' Create test data: a small water network with wells and pipes
#'
#' Generates two shapefiles (wells.shp and pipes.shp) in the given directory
#' using osgeo.ogr. The network consists of 4 wells connected by 3 pipes
#' in a simple chain: well0 --pipe0-- well1 --pipe1-- well2 --pipe2-- well3
#'
#' @param dir Directory to write the shapefiles
#' @return List with paths to the created shapefiles
create_test_data <- function(dir) {
  ogr <- reticulate::import("osgeo.ogr")
  osr <- reticulate::import("osgeo.osr")

  sr <- osr$SpatialReference()
  sr$SetFromUserInput("EPSG:4326")

  drv <- ogr$GetDriverByName("ESRI Shapefile")

  # -- Wells (points) --
  wells_path <- file.path(dir, "wells.shp")
  ds <- drv$CreateDataSource(wells_path)
  lyr <- ds$CreateLayer("wells", sr, ogr$wkbPoint)
  lyr$CreateField(ogr$FieldDefn("name", ogr$OFTString))

  well_coords <- list(
    c(0.0, 0.0),
    c(1.0, 0.0),
    c(2.0, 0.0),
    c(3.0, 0.0)
  )

  for (i in seq_along(well_coords)) {
    feat <- ogr$Feature(lyr$GetLayerDefn())
    feat$SetField("name", paste0("well", i - 1L))
    pt <- ogr$Geometry(ogr$wkbPoint)
    pt$AddPoint(well_coords[[i]][1], well_coords[[i]][2])
    feat$SetGeometry(pt)
    lyr$CreateFeature(feat)
  }
  ds$FlushCache()
  ds <- NULL

  # -- Pipes (lines) --
  pipes_path <- file.path(dir, "pipes.shp")
  ds <- drv$CreateDataSource(pipes_path)
  lyr <- ds$CreateLayer("pipes", sr, ogr$wkbLineString)
  lyr$CreateField(ogr$FieldDefn("name", ogr$OFTString))
  lyr$CreateField(ogr$FieldDefn("diameter", ogr$OFTReal))

  pipe_segments <- list(
    list(from = c(0.0, 0.0), to = c(1.0, 0.0)),
    list(from = c(1.0, 0.0), to = c(2.0, 0.0)),
    list(from = c(2.0, 0.0), to = c(3.0, 0.0))
  )

  for (i in seq_along(pipe_segments)) {
    feat <- ogr$Feature(lyr$GetLayerDefn())
    feat$SetField("name", paste0("pipe", i - 1L))
    feat$SetField("diameter", 0.5)
    line <- ogr$Geometry(ogr$wkbLineString)
    line$AddPoint(pipe_segments[[i]]$from[1], pipe_segments[[i]]$from[2])
    line$AddPoint(pipe_segments[[i]]$to[1], pipe_segments[[i]]$to[2])
    feat$SetGeometry(line)
    lyr$CreateFeature(feat)
  }
  ds$FlushCache()
  ds <- NULL

  list(wells = wells_path, pipes = pipes_path, dir = dir)
}
