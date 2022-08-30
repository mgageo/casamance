#
# quelques fonctions pour la casamance
# auteur: Marc Gauthier
#
#
# la grille utm
# source("geo/scripts/casamance.R"); nc1 <- grille_centre()
grille_centre <- function() {
  library(sf)
  library(rio)
  library(tidyverse)
  carp()
  dsn <- sprintf("%s/CONF/casamance_mailles.csv", varDir)
  df <- rio::import(dsn) %>%
    glimpse()
  df1 <- df %>%
    mutate(wkt = sprintf("POLYGON((%s %s),(%s %s),(%s %s),(%s %s),(%s %s))",
      swX, swY, seX, seY, neX, neY, nwX, nwY, swX, swY)) %>%
    glimpse()
  nc1 <- st_as_sf(df1, geometry = st_as_sfc(df1$wkt, crs = st_crs(4326))) %>%
    glimpse()
  return(invisible(nc1))
}
