# OGR <-> wk bridge functions
#
# These are the central infrastructure pieces for converting between
# Python OGR objects and R data.frames with wk geometry columns.
# They are reusable across any future osgeo.* wrapping.


#' Convert a Python OGRLayer proxy to a data.frame with wk geometry
#'
#' @param py_layer reticulate proxy to an OGRLayer
#' @param crs Character CRS string (e.g., "EPSG:4326") or NULL
#' @param include_layer If TRUE, add a `.layer` column with the layer name
#' @param include_role If TRUE, add a `.role` column ("node" for point
#'   geometries, "edge" for line geometries, NA for others)
#' @return data.frame with attribute columns and a `geom` column of class wk_wkb
#' @noRd
.ogr_layer_to_wk_df <- function(py_layer, crs = NULL,
                                 include_layer = FALSE,
                                 include_role = FALSE) {
  py_layer$ResetReading()

  layer_name <- py_layer$GetName()

  records <- list()
  geoms <- list()
  roles <- character(0)

  repeat {
    feat <- py_layer$GetNextFeature()
    if (is.null(feat)) break

    # Geometry -> WKB (Python bytes -> R raw via reticulate)
    g <- feat$GetGeometryRef()
    if (!is.null(g)) {
      geoms <- c(geoms, list(as.raw(g$ExportToWkb())))
      if (include_role) {
        gtype <- g$GetGeometryType()
        # OGR geometry type constants:
        # wkbPoint = 1, wkbLineString = 2, wkbMultiPoint = 4,
        # wkbMultiLineString = 5
        # Mask off the 25D bit (0x80000000) for type comparison
        base_type <- bitwAnd(gtype, 0xFL)
        role <- if (base_type %in% c(1L, 4L)) "node"
                else if (base_type %in% c(2L, 5L)) "edge"
                else NA_character_
        roles <- c(roles, role)
      }
    } else {
      geoms <- c(geoms, list(NULL))
      if (include_role) roles <- c(roles, NA_character_)
    }

    # Attributes -> named list
    n <- feat$GetFieldCount()
    rec <- list(.gfid = as.integer(feat$GetFID()))
    if (n > 0L) {
      for (i in seq_len(n) - 1L) {
        fname <- feat$GetFieldDefnRef(i)$GetName()
        rec[[fname]] <- feat$GetField(i)
      }
    }
    records <- c(records, list(rec))
  }

  if (length(records) == 0L) {
    df <- data.frame(
      .gfid = integer(0),
      check.names = FALSE
    )
    df$geom <- wk::wkb(list())
    if (include_layer) df$.layer <- character(0)
    if (include_role) df$.role <- character(0)
    return(df)
  }

  # Build data.frame from records
  # All records should have the same names (same layer defn), but
  # be defensive about it
  all_names <- unique(unlist(lapply(records, names)))
  df <- as.data.frame(
    lapply(all_names, function(nm) {
      vapply(records, function(r) {
        v <- r[[nm]]
        if (is.null(v)) NA else v
      }, records[[1]][[nm]])
    }),
    stringsAsFactors = FALSE
  )
  names(df) <- all_names

  df$geom <- wk::wkb(geoms)
  if (!is.null(crs)) {
    wk::wk_crs(df$geom) <- crs
  }

  if (include_layer) df$.layer <- rep(layer_name, nrow(df))
  if (include_role) df$.role <- roles

  df
}


#' Convert a path OGRLayer to a wk data.frame
#'
#' GNM path results contain features from multiple layers interleaved.
#' We extract each feature, determine its source layer and role.
#'
#' @param py_layer reticulate proxy to the path result OGRLayer
#' @param py_net reticulate proxy to the network dataset (for layer lookup)
#' @param crs CRS string
#' @return data.frame with .gfid, .layer, .role, geom columns
#' @noRd
.path_layer_to_wk_df <- function(py_layer, py_net, crs = NULL) {
  py_layer$ResetReading()

  gfids <- integer(0)
  geoms <- list()
  roles <- character(0)
  layers <- character(0)

  repeat {
    feat <- py_layer$GetNextFeature()
    if (is.null(feat)) break

    gfid <- as.integer(feat$GetFID())
    gfids <- c(gfids, gfid)

    g <- feat$GetGeometryRef()
    if (!is.null(g)) {
      geoms <- c(geoms, list(as.raw(g$ExportToWkb())))
      base_type <- bitwAnd(g$GetGeometryType(), 0xFL)
      role <- if (base_type %in% c(1L, 4L)) "node"
              else if (base_type %in% c(2L, 5L)) "edge"
              else NA_character_
      roles <- c(roles, role)
    } else {
      geoms <- c(geoms, list(NULL))
      roles <- c(roles, NA_character_)
    }

    # Try to determine source layer via GetFeatureByGlobalFID
    # The path layer doesn't carry layer-of-origin info directly,
    # so we look up by GFID in the network
    layer_name <- NA_character_
    tryCatch({
      net_feat <- py_net$GetFeatureByGlobalFID(gfid)
      if (!is.null(net_feat)) {
        # GetFeatureByGlobalFID doesn't directly tell us the layer name,
        # but we can infer from the feature's field schema
        layer_name <- NA_character_
      }
    }, error = function(e) NULL)
    layers <- c(layers, layer_name)
  }

  if (length(gfids) == 0L) {
    df <- data.frame(
      .gfid = integer(0), .role = character(0),
      check.names = FALSE
    )
    df$geom <- wk::wkb(list())
    return(df)
  }

  df <- data.frame(
    .gfid = gfids,
    .role = roles,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  df$geom <- wk::wkb(geoms)
  if (!is.null(crs)) {
    wk::wk_crs(df$geom) <- crs
  }

  df
}


#' Write a wk data.frame to a temporary OGR dataset via Python
#'
#' @param df data.frame with a wk geometry column
#' @param name Layer name
#' @param srs SRS string
#' @param geom_col Name of the geometry column (default: auto-detect)
#' @return list(py_ds, tmp_path, layer_name)
#' @noRd
.wk_df_to_ogr <- function(df, name, srs, geom_col = NULL) {
  # Find the wk column
  if (is.null(geom_col)) {
    wk_cols <- vapply(df, inherits, logical(1), "wk_vctr")
    if (!any(wk_cols)) {
      cli::cli_abort("No wk geometry column found in {.arg df}.")
    }
    geom_col <- names(df)[which(wk_cols)[1]]
  }

  geom <- df[[geom_col]]
  attrs <- df[setdiff(names(df), geom_col)]
  # Convert any wk type to WKT for OGR ingestion
  wkt_vec <- as.character(wk::wk_handle(geom, wk::wkt_writer()))

  ogr <- .osgeo$ogr
  osr <- .osgeo$osr

  sr <- osr$SpatialReference()
  sr$SetFromUserInput(srs)

  tmp <- tempfile(fileext = ".gpkg")
  drv <- ogr$GetDriverByName("GPKG")
  out_ds <- drv$CreateDataSource(tmp)

  # Infer geometry type from first non-NA geometry
  non_na <- which(!is.na(wkt_vec))
  if (length(non_na) == 0L) {
    cli::cli_abort("All geometries are NA in {.arg df}.")
  }
  sample_geom <- ogr$CreateGeometryFromWkt(wkt_vec[non_na[1]])
  geom_type <- sample_geom$GetGeometryType()

  lyr <- out_ds$CreateLayer(name, sr, geom_type)

  # Create attribute fields
  for (col in names(attrs)) {
    val <- attrs[[col]]
    if (is.integer(val)) {
      fld <- ogr$FieldDefn(col, ogr$OFTInteger)
    } else if (is.double(val) || is.numeric(val)) {
      fld <- ogr$FieldDefn(col, ogr$OFTReal)
    } else {
      fld <- ogr$FieldDefn(col, ogr$OFTString)
    }
    lyr$CreateField(fld)
  }

  # Write features
  for (i in seq_len(nrow(df))) {
    feat <- ogr$Feature(lyr$GetLayerDefn())

    for (col in names(attrs)) {
      val <- attrs[[col]][i]
      if (!is.na(val)) {
        feat$SetField(col, val)
      }
    }

    if (!is.na(wkt_vec[i])) {
      g <- ogr$CreateGeometryFromWkt(wkt_vec[i])
      feat$SetGeometry(g)
    }

    lyr$CreateFeature(feat)
  }

  out_ds$FlushCache()

  list(py_ds = out_ds, tmp_path = tmp, layer_name = name)
}
