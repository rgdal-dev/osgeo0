# osgeo0: R bindings for osgeo.gnm (and a template for osgeo.*)

## Package vision

`osgeo0` wraps `osgeo.gnm` via reticulate with an idiomatic R interface built on S7 classes and `wk` geometry. The name signals intent: this is the first module in what could become a broader R interface to the `osgeo` Python namespace, with GNM serving as the worked example for patterns that generalise to `osgeo.gdal`, `osgeo.ogr`, `osgeo.osr`, and `osgeo.gdal_array`.

GNM is a good first target because the API surface is small and self-contained, the object lifecycle issues are representative of the broader osgeo API, and it exercises both read and write paths including dataset creation, layer management, feature access, and analysis — all of which recur throughout osgeo.

### Dependencies

**Hard dependencies:**
- `reticulate` — Python interop
- `wk` — geometry vectors and data frames
- `S7` — class system
- `cli` — user-facing messages and errors

**Suggested (used if available):**
- `gdalraster` — for any GDAL-side I/O that osgeo.gnm doesn't handle directly (reading source shapefiles/geopackages into OGR layers, writing result layers to disk). This is a transitional dependency: as osgeo0 grows to wrap more of osgeo.ogr, gdalraster's role shrinks.

---

## Why wk

wk gives us exactly the right abstraction layer:

1. **A wk geometry vector** (`wk_wkb`, `wk_wkt`, `wk_xy`, etc.) is a first-class vector type that slots into any data.frame column. No special data.frame subclass needed — a `data.frame(gfid = integer(), cost = double(), geom = wk::wkt())` is a perfectly good return type.

2. **Interchange is the user's problem, not ours.** wk vectors convert to/from geos (`geos::as_geos_geometry`), terra (`terra::vect` via WKT/WKB), s2 (`s2::as_s2_geography`), and anything else that speaks WKT or WKB. We produce wk; the user routes it wherever they need.

3. **Construction from OGR is straightforward.** OGR features expose `ExportToWkb()` which returns Python `bytes`; reticulate converts these to R `raw` vectors via `as.raw()`; `wk::wkb()` accepts a list of raw vectors. The entire read path is binary — no text encoding, no floating-point round-trip.

4. **CRS handling.** `wk::wk_crs()` and `wk::wk_set_crs()` attach CRS as an attribute. We pull the SRS from the network/layer and attach it.

5. **Lightweight.** wk has minimal compiled code and no GDAL/PROJ/GEOS linkage of its own.

The critical architectural advantage: **no competing libgdal**. wk has no GDAL/PROJ/GEOS linkage, so there is no shared library conflict with reticulate's osgeo. We extract WKB bytes from the Python side and construct R geometry vectors from raw vectors — just bytes crossing the reticulate bridge. Packages that link their own libgdal cannot coexist safely with a second libgdal loaded via reticulate; wk sidesteps this entirely.

---

## The OGR ↔ wk bridge

This is the central piece of infrastructure that will be reused across any future osgeo.* wrapping. It deserves careful design.

### Python OGRLayer → R data.frame with wk column

```r
#' Convert a Python OGRLayer proxy to a data.frame with wk geometry
#'
#' @param py_layer reticulate proxy to an OGRLayer
#' @param crs Character CRS string (e.g., "EPSG:4326") or NULL
#' @return data.frame with attribute columns and a `geom` column of class wk_wkb
#' @noRd
.ogr_layer_to_wk_df <- function(py_layer, crs = NULL) {
  py_layer$ResetReading()
  
  records <- list()
  geoms <- list()
  
  repeat {
    feat <- py_layer$GetNextFeature()
    if (is.null(feat)) break
    
    # Geometry -> WKB (Python bytes -> R raw via reticulate)
    g <- feat$GetGeometryRef()
    if (!is.null(g)) {
      geoms <- c(geoms, list(as.raw(g$ExportToWkb())))
    } else {
      geoms <- c(geoms, list(NULL))
    }
    
    # Attributes -> named list
    n <- feat$GetFieldCount()
    rec <- list(.gfid = feat$GetFID())
    if (n > 0L) {
      for (i in seq_len(n) - 1L) {  # 0-indexed
        fname <- feat$GetFieldDefnRef(i)$GetName()
        rec[[fname]] <- feat$GetField(i)
      }
    }
    records <- c(records, list(rec))
  }
  
  if (length(records) == 0L) {
    return(data.frame(
      .gfid = integer(0),
      geom = wk::wkb(list()),
      check.names = FALSE
    ))
  }
  
  df <- do.call(rbind, lapply(records, as.data.frame,
                               stringsAsFactors = FALSE))
  df$geom <- wk::wkb(geoms)
  
  if (!is.null(crs)) {
    wk::wk_crs(df$geom) <- crs
  }
  
  df
}
```

Notes on this approach:

- We use `ExportToWkb()` rather than `ExportToWkt()`. OGR's `ExportToWkb()` returns Python `bytes`; reticulate's `as.raw.python.builtin.bytes` converts directly to an R raw vector; `wk::wkb()` accepts a list of raw vectors. The whole chain is binary — no text encoding/decoding, no floating-point round-trip through decimal representation. This is both more compact and more faithful than WKT.

- `.gfid` is the GNM Global Feature ID, which is what all GNM operations use. It's not the same as the OGR FID within a single layer. We include it in every result.

- The `do.call(rbind, ...)` is fine for small feature counts. For larger results (future osgeo.ogr work), we'd pre-allocate or use `vctrs::vec_rbind`.

### R data.frame with wk column → Python OGR dataset

For `gnm_import()`, we need to go the other direction: take a user's data.frame with a wk geometry column and get it into the network as an OGR layer.

```r
#' Write a wk data.frame to a temporary OGR dataset, return the Python dataset
#'
#' @param df data.frame with a wk geometry column
#' @param geom_col Name of the geometry column (default: auto-detect)
#' @param name Layer name
#' @param srs SRS string
#' @return list(py_ds = Python GDALDataset proxy, tmp_path = character,
#'              layer_name = character)
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
  wkt_vec <- wk::wk_handle(geom, wk::wkt_writer())
  
  # Construct via Python OGR API directly (no gdalraster dependency)
  ogr <- .osgeo$ogr
  osr <- .osgeo$osr
  
  sr <- osr$SpatialReference()
  sr$SetFromUserInput(srs)
  
  tmp <- tempfile(fileext = ".gpkg")
  drv <- ogr$GetDriverByName("GPKG")
  out_ds <- drv$CreateDataSource(tmp)
  
  # Infer geometry type from first non-NA geometry
  first_wkt <- wkt_vec[!is.na(wkt_vec)][1]
  sample_geom <- ogr$CreateGeometryFromWkt(first_wkt)
  geom_type <- sample_geom$GetGeometryType()
  
  lyr <- out_ds$CreateLayer(name, sr, geom_type)
  
  # Create attribute fields
  for (col in names(attrs)) {
    val <- attrs[[col]]
    if (is.integer(val)) {
      fld <- ogr$FieldDefn(col, ogr$OFTInteger)
    } else if (is.double(val)) {
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
      feat$SetField(col, attrs[[col]][i])
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
```

This is entirely Python-native — no gdalraster needed for write. We construct features via osgeo.ogr directly over the reticulate bridge. For import into the network, we then call `net@.env$py_ds$CopyLayer()` from this dataset.

### Where gdalraster fits

gdalraster becomes useful for operations that osgeo.gnm doesn't cover but that we want before wrapping more of osgeo.*:

- **Reading source data from arbitrary formats** when the user doesn't have an in-memory data.frame. For example, `gnm_import_path(net, dsn = "roads.shp", name = "roads")` could use gdalraster's `ogr_ds_create` / `ogr_layer_*` to open and inspect the source. But even here, osgeo.ogr can open the source file directly in Python.

- **CRS operations** — `gdalraster::srs_*` functions for parsing/converting CRS strings. But `osgeo.osr` does the same.

- **Metadata queries** — driver capabilities, format info. Again, osgeo.gdal provides these.

The honest assessment: gdalraster's utility in osgeo0 is marginal for GNM. The Python osgeo bindings handle everything. The main case for gdalraster is if you want to avoid round-tripping through Python for operations that have an efficient C-level path from R — but at GNM's scale of data, the round-trip cost is invisible.

**Recommendation:** Make gdalraster `Suggests`, not `Imports`. Use it opportunistically (e.g., a helper that prefers gdalraster for CRS validation when available, falls back to osgeo.osr). As osgeo0 grows toward raster operations, re-evaluate — gdalraster provides battle-tested raster I/O that would be painful to reimplement over reticulate, but it also represents an alternative to wrapping osgeo.gdal rather than a complement.

---

## Revised class design

### gnm_network (S7)

```r
gnm_network <- new_class("gnm_network",
  properties = list(
    name = class_character,
    srs  = class_character,
    path = class_character,
    description = class_character,
    .env = class_environment
  ),
  constructor = function(path, name, srs = "EPSG:4326",
                         description = "", .env = NULL) {
    if (is.null(.env)) {
      .env <- new.env(parent = emptyenv())
      .env$py_ds <- NULL
      .env$closed <- TRUE
    }
    new_object(
      S7_object(),
      name = name, srs = srs, path = path,
      description = description, .env = .env
    )
  }
)
```

Properties are all character — simple, serialisable, printable. The mutable guts live in `.env`.

### gnm_path (S7)

```r
gnm_path <- new_class("gnm_path",
  properties = list(
    from = class_integer,
    to = class_integer,
    algorithm = class_character,
    features = class_data.frame,  # data.frame with geom (wk), .gfid, .layer, .role
    n_features = class_integer
  ),
  constructor = function(from, to, algorithm, features) {
    new_object(
      S7_object(),
      from = as.integer(from),
      to = as.integer(to),
      algorithm = algorithm,
      features = features,
      n_features = as.integer(nrow(features))
    )
  }
)
```

`features` is a plain `data.frame` with columns:
- `geom` — `wk_wkb` vector with CRS attached
- `.gfid` — integer, the GNM Global Feature ID
- `.layer` — character, which class layer this feature came from
- `.role` — character, `"node"` or `"edge"` (derived from geometry type or layer)
- Plus any attribute columns from the source features

No special data.frame subclass. The user can do whatever they want with it.

### gnm_components (S7)

```r
gnm_components <- new_class("gnm_components",
  properties = list(
    features = class_data.frame,  # .gfid, .component_id, .layer, geom
    n_components = class_integer
  )
)
```

Plain data.frame. The `.component_id` column groups features by connected component.

---

## Revised public API

### Lifecycle

```r
gnm_create(
  path,
  name,
  srs = "EPSG:4326",
  format = "ESRI Shapefile",
  description = ""
) -> gnm_network

gnm_open(path) -> gnm_network

gnm_close(net) -> NULL (invisibly)

with_gnm(path, fun)
```

### Import / layer access

```r
# Import from a wk data.frame
gnm_import(net, data, name) -> net (invisibly)

# Import from a file path (opens via osgeo.ogr, copies layer in)
gnm_import_path(net, dsn, layer = NULL, name = NULL) -> net (invisibly)

# List class layers (excludes system layers)
gnm_layers(net) -> character

# Extract a class layer as a data.frame with wk geom column
gnm_layer(net, name) -> data.frame

# Look up one feature by GFID
gnm_feature(net, gfid) -> 1-row data.frame with wk geom
```

### Topology

```r
gnm_connect_auto(net, layers = NULL, tolerance = 0.001,
                 cost = 1, inverse_cost = 1,
                 directed = FALSE) -> net (invisibly)

gnm_connect(net, src, tgt, connector = -1L,
            cost = 1, inverse_cost = 1,
            directed = FALSE) -> net (invisibly)

gnm_disconnect(net, src, tgt, connector = -1L) -> net (invisibly)

gnm_disconnect_all(net) -> net (invisibly)
```

### Rules

```r
gnm_add_rule(net, rule) -> net (invisibly)
gnm_rules(net) -> character
gnm_delete_rule(net, rule) -> net (invisibly)
gnm_delete_all_rules(net) -> net (invisibly)
```

### Analysis

```r
gnm_shortest_path(net, from, to) -> gnm_path
gnm_k_paths(net, from, to, k = 3L) -> gnm_path
gnm_components(net) -> gnm_components
```

### Blocking

```r
gnm_block(net, gfid) -> net (invisibly)
gnm_unblock(net, gfid) -> net (invisibly)
gnm_block_all(net) -> net (invisibly)
gnm_unblock_all(net) -> net (invisibly)
```

### Generics / wk integration

```r
method(print, gnm_network) <- ...
method(print, gnm_path) <- ...
method(print, gnm_components) <- ...

# Register gnm_path as wk-handleable
wk_handle.gnm_path <- function(handleable, handler, ...) {
  wk::wk_handle(handleable@features$geom, handler, ...)
}
```

The `wk_handle` method means gnm_path objects work directly with `wk_plot()`, `wk_bbox()`, `geos::as_geos_geometry()`, etc.

---

## Internal module structure

```
osgeo0/
+-- DESCRIPTION
+-- NAMESPACE
+-- R/
|   +-- aaa-modules.R       # .onLoad, delay-loaded Python modules
|   +-- aaa-classes.R        # S7 class definitions
|   +-- bridge-ogr-wk.R     # .ogr_layer_to_wk_df, .wk_df_to_ogr
|   +-- bridge-check.R       # .assert_open, .assert_gnm_available, .as_gfid
|   +-- gnm-lifecycle.R      # gnm_create, gnm_open, gnm_close, with_gnm
|   +-- gnm-import.R         # gnm_import, gnm_import_path, gnm_layer, gnm_layers
|   +-- gnm-topology.R       # gnm_connect_auto, gnm_connect, gnm_disconnect
|   +-- gnm-rules.R          # gnm_add_rule, gnm_rules, gnm_delete_rule
|   +-- gnm-analysis.R       # gnm_shortest_path, gnm_k_paths, gnm_components
|   +-- gnm-block.R          # gnm_block, gnm_unblock, etc.
|   +-- methods-print.R      # print methods for all S7 classes
|   +-- methods-wk.R         # wk_handle, wk_crs methods
|   +-- zzz.R                # package-level hooks
+-- inst/
|   +-- testdata/
|       +-- pipes.shp (+ .shx, .dbf, .prj)
|       +-- wells.shp (+ .shx, .dbf, .prj)
+-- tests/
|   +-- testthat/
|       +-- test-lifecycle.R
|       +-- test-import.R
|       +-- test-topology.R
|       +-- test-analysis.R
|       +-- helper-skip.R   # skip_if_no_gnm()
+-- vignettes/
|   +-- water-network.Rmd
+-- man/
```

### aaa-modules.R

```r
# Package-level Python module references (populated in .onLoad)
.osgeo <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .osgeo$gnm  <- reticulate::import("osgeo.gnm",  delay_load = TRUE)
  .osgeo$gdal <- reticulate::import("osgeo.gdal", delay_load = TRUE)
  .osgeo$ogr  <- reticulate::import("osgeo.ogr",  delay_load = TRUE)
  .osgeo$osr  <- reticulate::import("osgeo.osr",  delay_load = TRUE)
}
```

The `.osgeo` environment is the seed of the "broader osgeo wrapping" vision. As the package grows, new modules (`osgeo.gdal_array`, `osgeo.gdalconst`) slot in here. Any future `osgeo0` function that needs raw gdal access uses `.osgeo$gdal`.

### bridge-check.R

```r
.assert_open <- function(net) {
  if (net@.env$closed) {
    cli::cli_abort(
      "{.cls gnm_network} {.val {net@name}} is not open.",
      call = parent.frame()
    )
  }
}

.assert_gnm_available <- function() {
  gdal <- .osgeo$gdal
  if (is.null(gdal$GetDriverByName("GNMFile"))) {
    cli::cli_abort(c(
      "GNM support not available in the current GDAL Python bindings.",
      i = "Ensure {.pkg osgeo} is built with GNM enabled.",
      i = "{.code conda install -c conda-forge gdal} includes GNM."
    ))
  }
}

.as_gfid <- function(x) {
  x <- as.integer(x)
  if (anyNA(x)) cli::cli_abort("GFID must not be NA.")
  reticulate::r_to_py(x)
}

.direction <- function(directed = FALSE) {
  if (isTRUE(directed)) {
    .osgeo$gnm$GNM_EDGE_DIR_SRCTOTGT
  } else {
    .osgeo$gnm$GNM_EDGE_DIR_BOTH
  }
}
```

---

## Lifetime management

```
                gnm_create() or gnm_open()
                         |
                         v
              +-----------------------+
              |  OPEN                 |
              |  .env$py_ds is live   |
              |  .env$closed = FALSE  |
              +-----------+-----------+
                          |
             gnm_close()  |  or GC destructor
                          v
              +-----------------------+
              |  CLOSED               |
              |  .env$py_ds = NULL    |
              |  .env$closed = TRUE   |
              +-----------------------+
```

Every method that touches `.env$py_ds` calls `.assert_open()` first.

Destructor registration (called in `gnm_create` / `gnm_open` after successfully opening):

```r
.register_destructor <- function(net) {
  e <- net@.env
  reg.finalizer(e, function(env) {
    if (!env$closed && !is.null(env$py_ds)) {
      tryCatch(env$py_ds$FlushCache(), error = function(e) NULL)
      env$py_ds <- NULL
      env$closed <- TRUE
    }
  }, onexit = TRUE)
}
```

Interactive use: call `gnm_close()` explicitly. Forgotten close: GC finalizer cleans up. Scoped use: `with_gnm()`.

---

## Patterns for future osgeo.* wrapping

GNM is small, but the patterns it establishes apply directly to wrapping the rest of the osgeo namespace:

### Pattern 1: S7 + private environment for GDAL datasets

Any GDAL dataset (raster, vector, network, multidimensional) has the same lifecycle: open/create -> operate -> close. The `.env$py_ds` + `reg.finalizer` + `.assert_open()` pattern works for all of them. A future `ogr_dataset` or `gdal_dataset` S7 class would look structurally identical to `gnm_network`.

### Pattern 2: .ogr_layer_to_wk_df as the universal vector result extractor

Every osgeo operation that returns vector features (OGR layer reads, spatial queries, GDAL vector translate results) can funnel through the same OGR->wk bridge. The function is reusable as-is.

### Pattern 3: .wk_df_to_ogr as the universal vector input writer

Any operation that takes vector input (import, translate, warp with cutline, vector-to-raster burn) can accept a wk data.frame and construct OGR features via the same path.

### Pattern 4: Constants as named character -> integer lookup

Rather than exposing raw integer constants:

```r
.gnm_algorithms <- c(
  dijkstra = 1L,
  kpaths = 2L,
  components = 3L
)
```

This generalises to GDAL's many enum-like constants (resampling algorithms, data types, open flags, etc.).

### Pattern 5: .osgeo environment as module registry

Adding a new osgeo module to osgeo0 means one line in `.onLoad` plus the wrapper functions. No new package infrastructure needed. The `.osgeo` environment can be documented as a semi-public interface for advanced users who want to drop down to raw reticulate calls:

```r
# Advanced: direct osgeo access
osgeo0:::.osgeo$gdal$VersionInfo("")
```

### Pattern 6: gdalraster as transitional, not foundational

The migration path as osgeo0 grows:

1. **Now (GNM only):** gdalraster is Suggests, barely used. The osgeo.ogr/osr Python modules handle everything.
2. **Medium term (osgeo.ogr wrapping):** gdalraster provides fallback for users without a Python environment. Some functions could dispatch to gdalraster when available and reticulate when not.
3. **Long term:** osgeo0 and gdalraster serve different dependency philosophies. osgeo0 targets users who already have a Python geospatial environment; gdalraster targets users who want pure R.

---

## Implementation phases

### Phase 1: Skeleton + bridge + lifecycle

- Package skeleton with DESCRIPTION, NAMESPACE, S7 + wk + reticulate + cli deps
- `.osgeo` module loading in `.onLoad`
- `.ogr_layer_to_wk_df()` and `.wk_df_to_ogr()`
- `gnm_network` S7 class with `.env` pattern and destructor
- `gnm_create()`, `gnm_open()`, `gnm_close()`, `with_gnm()`
- `print.gnm_network`
- Tests: create/open/close round-trip
- Ship pipes.shp + wells.shp in inst/testdata/

### Phase 2: Import + auto-connect + query

- `gnm_import()` (from wk data.frame)
- `gnm_import_path()` (from file, using osgeo.ogr)
- `gnm_layers()`, `gnm_layer()`
- `gnm_connect_auto()`
- `gnm_shortest_path()` -> `gnm_path` with `.gfid`, `.layer`, `.role`, `geom` columns
- `wk_handle.gnm_path`
- Tests: full water network workflow end-to-end

### Phase 3: Full topology + analysis

- Manual connect/disconnect/reconnect
- Rules
- Blocking
- K-shortest-paths
- Connected components
- `gnm_feature()` lookup by GFID

### Phase 4: Documentation + vignette

- `water-network.Rmd` vignette walking through the full API
- Developer vignette documenting the reusable patterns for osgeo.* wrapping
- pkgdown site

---

## Testing strategy

```r
# helper-skip.R
skip_if_no_gnm <- function() {
  skip_if_not_installed("reticulate")
  skip_if(!reticulate::py_available(initialize = TRUE))
  tryCatch({
    gnm <- reticulate::import("osgeo.gnm")
    gdal <- reticulate::import("osgeo.gdal")
    drv <- gdal$GetDriverByName("GNMFile")
    if (is.null(drv)) skip("GNM driver not available")
  }, error = function(e) {
    skip(paste("osgeo.gnm not available:", e$message))
  })
}
```

Tests create networks in `withr::local_tempdir()` and clean up automatically. The pipes/wells data is copied from `inst/testdata/` into the temp dir for each test.

CI: GitHub Actions with `conda-forge` environment providing `gdal` + `python-gdal`. The `setup-miniconda` action handles this.

---

## Open questions

1. **WKB vs WKT across the bridge.** Resolved: WKB for the read path (OGR → R). `ExportToWkb()` → reticulate `as.raw()` → `wk::wkb()` is binary end-to-end with no floating-point text round-trip. For the write path (R → OGR), WKT via `wk::wkt_writer()` → `CreateGeometryFromWkt()` remains simpler since `CreateGeometryFromWkb()` requires explicit Python bytes construction; worth revisiting if write performance matters at scale.

2. **Geometry column name.** Current design uses `geom`. wk is agnostic. Keeping `geom` — short and unambiguous.

3. **GFID column name.** `.gfid` with leading dot signals "infrastructure, not user data." Unusual for data.frame columns but clearly marks it as a system identifier. The `.layer` and `.role` columns follow the same convention.

4. **The `.role` column in path results.** GNM paths alternate between node and edge features. Deriving `.role` requires knowing which layer each feature came from and whether it's point (node) or line (edge) geometry. This is available from the OGR feature during extraction — we check geometry type and classify. Worth the small overhead for the usability gain.

5. **Multiple GDAL instances.** If the user has gdalraster (which links the system libgdal) and also loads osgeo.gdal via reticulate (a different libgdal from the Python env), two GDAL instances coexist. This works — they just can't share vsimem or open the same file with update access simultaneously. The WKB-based wk bridge sidesteps any shared-state issues entirely. Worth documenting prominently.
