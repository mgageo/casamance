# <!-- coding: utf-8 -->
#
# la partie couches : vecteur et raster
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
# https://github.com/riatelab/maptiles?tab=readme-ov-file
# https://docs.stadiamaps.com/map-styles/stamen-terrain/
#
ogcDir <<- sprintf("%s/ogc", varDir)
#
# source("geo/scripts/casamance.R"); couches_jour()
couches_jour <- function(ogc = FALSE, cartes = FALSE, test = 2) {
  carp()
  library(tidyverse)
  library(utf8)
  library(janitor)
  couches_ogc()
}
#
# la grille est en UTM zone 28N EPSG:31028
#
# https://github.com/riatelab/maptiles
couches_ogc <- function(test=2) {
  carp()
  library(tidyverse)
  library(sf)
  library(maptiles)
  ogcDir <<- sprintf("%s/ogc", varDir)
  dir.create(ogcDir, showWarnings = FALSE)
  fonds.df <- couches_fonds()
  dsn <- sprintf("%s/mailles.json", varDir)
  nc0 <- st_read(dsn) %>%
    st_transform("EPSG:3857")
  nc <- nc0 %>%
    st_buffer(10000)
  nc_osm <- get_tiles(nc, crop = TRUE)
# display map
  plot_tiles(nc_osm)
  plot(st_geometry(nc0), col = NA, add = TRUE)
}
#
couches_ogc <- function() {
  carp()
  library(tidyverse)
  library(sf)
  library(terra)
  library(maptiles)
  dir.create(ogcDir, showWarnings = FALSE)
  dsn <- sprintf("%s/casamance.json", varDir)
  dsn <- sprintf("%s/mailles.json", varDir)
  nc0 <- st_read(dsn) %>%
    st_transform("EPSG:31028")
#    st_transform("EPSG:3857")
  nc <- nc0 %>%
    st_buffer(5000)
# define the tile server parameters
  osmpos <- create_provider(
    name = "CARTO.NO",
#    url = "https://tiles.stadiamaps.com/tiles/stamen_terrain/{z}/{x}/{y}{r}.png",
    url = "https://tiles.stadiamaps.com/tiles/stamen_terrain_background/{z}/{x}/{y}{r}.png",
    citation = "© Stadia Maps © Stamen Design © OpenMapTiles © OpenStreetMap contributors"
  )
# define the tile server parameters
osmpos2 <- create_provider(
  name = "CARTO.POSITRON",
  url = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
  sub = c("a", "b", "c", "d"),
  citation = "© OpenStreetMap contributors © CARTO "
)
# dowload tiles and compose raster (SpatRaster)
  nc_osmpos <<- get_tiles(
    x = nc,
    provider = osmpos,
    crop = TRUE,
    zoom = 7,
    cachedir = tempdir(),
    verbose = TRUE,
    forceDownload = TRUE,
    retina = TRUE
  )
  dsn <- sprintf("%s/osmpos.tif", ogcDir)
  writeRaster(nc_osmpos, dsn, overwrite = TRUE)
  plot_tiles(nc_osmpos)
  plot(st_geometry(nc0), col = NA, add = TRUE)
# display credits
  mtext(text = get_credit(osmpos), side = 1, line = -1, adj = .99)
}
#
# source("geo/scripts/casamance.R"); couches_ggplot()
couches_ggplot <- function(test=2) {
  carp()
  library(tidyverse)
  library(ggplot2)
  library(ggthemes)
  library(sf)
  library(raster)
  library(terra)
  library(tidyterra)
  dsn <- sprintf("%s/mailles.json", varDir)
  vector_data <<- terra::vect(dsn) %>%
    terra::project("EPSG:31028")
  dsn <- sprintf("%s/osmpos.tif", ogcDir)
  raster_layer <<- terra::rast(dsn) %>%
    glimpse()
# Créer la carte choroplèthe
  gg <- ggplot() +
  # Ajouter le fond raster
    geom_spatraster_rgb(
      data = raster_layer
    ) +
    geom_spatvector(
      data = vector_data,
      fill = NA
    )
  gg <- gg + ggplot_casamance()
  plot(gg)
  ratio <-  dim(raster_layer)[1] /  dim(raster_layer)[2]
  dsn <- "d:/casamance_couches_ggplot.jpg"
  ggsave(dsn, plot = gg, width = 5, height = 5 * ratio)
}
#
# source("geo/scripts/casamance.R"); couches_terra()
couches_terra <- function(test=2) {
  carp()
  library(tidyverse)
  library(terra)
  dsn <- sprintf("%s/mailles.json", varDir)
  nc0 <- terra::vect(dsn) %>%
    terra::project("EPSG:31028")
  carp("nc0 ext: %s", ext(nc0))
  nc1 <- nc0 %>%
    buffer(5000)
  carp("nc1 ext: %s", ext(nc1))
  dsn <- sprintf("%s/osmpos.tif", ogcDir)
  dsn <- sprintf("%s/osmpos_v1.tif", ogcDir)
  raster <- terra::rast(dsn) %>%
    crop(nc1)
  carp("raster ext: %s", ext(raster))
  plotRGB(raster, axes = FALSE, add = FALSE)
  plot(nc0, add = TRUE, col = "grey", alpha = .5)
  dsn <- sprintf("%s/osmpos_v2.tif", ogcDir)
  writeRaster(x = raster, dsn, overwrite = TRUE)
  carp("dsn: %s", dsn)
#  dsn <- "d:/casamance_couches_terra.png"
#  dev.copy(png, dsn, width=dim(raster_layer)[2], height=dim(raster_layer)[1])
#  graphics.off()
}