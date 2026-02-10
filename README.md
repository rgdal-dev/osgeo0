# osgeo0

R interface to the GDAL Geographic Network Model (GNM) via Python's `osgeo.gnm` and [reticulate](https://rstudio.github.io/reticulate/).

> **Status**: alpha — API is exploratory and will change.

## What is this?

osgeo0 wraps `osgeo.gnm` as the first module in a broader R interface to the Python `osgeo.*` bindings. GNM was chosen as the starting point because it has a small, well-defined API surface that exercises the full lifecycle patterns (create, populate, analyse, close) needed for all future `osgeo.*` wrapping.

The package uses:

- **[wk](https://paleolimbot.github.io/wk/)** for geometry interchange (WKB bytes across the Python–R bridge)
- **[S7](https://rconsortium.github.io/S7/)** for the class system
- **[reticulate](https://rstudio.github.io/reticulate/)** to call Python's `osgeo` modules

## Installation

```r
# install.packages("pak")
pak::pak("mdsumner/osgeo0")
```

You also need a Python environment with GDAL's Python bindings:

```bash
conda install -c conda-forge gdal python
```

## Quick start

```r
reticulate::use_python("/usr/bin/python3 ")
library(osgeo0)

# Create a network
net <- gnm_create(tempdir(), "water_network", srs = "EPSG:4326")

# Import layers from shapefiles
gnm_import_path(net, "wells.shp", name = "wells")
gnm_import_path(net, "pipes.shp", name = "pipes")

# Build topology automatically
gnm_connect_auto(net, tolerance = 0.000001)

# Find shortest path
wells <- gnm_layer(net, "wells")
path <- gnm_shortest_path(net, wells$.gfid[1], wells$.gfid[4])
print(path)

# Path result is a data.frame with wk geometry — works with wk_plot, geos, etc.
wk::wk_plot(path)

# Block a feature (simulate a broken pipe)
gnm_block(net, wells$.gfid[2])

# Clean up
gnm_close(net)
```

## Design

See [`inst/design/osgeo0-design-v2.md`](inst/design/osgeo0-design-v2.md) for the full design document.

Key patterns that will carry forward to `osgeo.ogr`, `osgeo.gdal`, etc.:

- S7 value class + private environment for mutable Python dataset references
- OGR ↔ wk bridge via WKB bytes (no competing `libgdal` linkage)
- `reg.finalizer()` for automatic cleanup on GC
- Eager materialisation of results (no dangling Python references in returned objects)

## License

MIT
