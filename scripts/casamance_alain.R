# <!-- coding: utf-8 -->
#
# quelques fonctions pour la casamance
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
#
# conversion des données pour Alain en wgs84
#
# lecture des données
# source("geo/scripts/casamance.R");alain_lire_rio()
alain_lire_rio <- function() {
  if (exists("rio.df")) {
    return(rio.df)
  }
  library(rio)
  dsn <- sprintf("%s/CONF/donnees20201118_v12.xlsx", varDir)
  carp("dsn: %s", dsn)
  rio.df <<- import(dsn) %>%
    glimpse()
  return(invisible(rio.df))
}
# source("geo/scripts/casamance.R");alain_wgs84()
alain_wgs84 <- function() {
  library(stringr)
  carp()
  df <- alain_lire_rio()
  dsn <- sprintf("%s/utm_json.csv", cfgDir)
  carp("dsn: %s", dsn)
  utm.df <- rio::import(dsn)
    dplyr::select(utm, lat = X, lon = Y) %>%
    glimpse()
  min_lat <- min(utm.df$lat)
  max_lat <- max(utm.df$lat)
  min_lon <- min(utm.df$lon)
  max_lon <- max(utm.df$lon)

  df1 <- df %>%
    left_join(utm.df, by = c("CarreUTM" = "utm")) %>%
    glimpse()
  df2 <- df1 %>%
    filter(is.na(lat)) %>%
    glimpse()
#  print(knitr::kable(df2, format = "pipe"))
  df3 <- df %>%
    filter(grepl("^\\d+", X)) %>%
    filter(grepl("^\\d+", Y)) %>%
    mutate(x = as.numeric(str_extract(X, "\\d+"))) %>%
    mutate(y = as.numeric(str_extract(Y, "\\d+"))) %>%
    glimpse()
# Casamance UTM 28P EPSG:32628
  nc1 <- st_as_sf(df3, coords = c("x", "y"), crs = 32628) %>%
    st_transform(4326) %>%
    glimpse()
}