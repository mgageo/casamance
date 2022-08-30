# <!-- coding: utf-8 -->
#
# la partie atlas : les cartes
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
#
# source("geo/scripts/casamance.R");atlas_jour(force = TRUE)
atlas_jour <- function(force = FALSE) {
  carp()
  df <- donnees_lire(force = force) %>%
    glimpse()
#  atlas_especes_cartes(df, force = force)
  atlas_stat(df)
  tex_pdflatex("atlas.tex")
}
#
#
# source("geo/scripts/casamance.R"); df <- atlas_especes_cartes(force = TRUE)
atlas_especes_cartes <- function(df, force = FALSE) {
  carp()
  df1 <- df %>%
    filter(grepl("^Nidif", probabilite_nidification))
  atlas_especes_cartes_nicheurs(df1)
}
#
# source("geo/scripts/casamance.R"); df <- atlas_stat()
atlas_stat <- function(df) {
  library(tidyverse)
  library(janitor)
  carp()
  df1 <- df %>%
    rename(maille = carre_utm, espece = nom_francais) %>%
    mutate(aaaa = strftime(date_obs, format="%Y")) %>%
    mutate(mm = strftime(date_obs, format="%m")) %>%
    mutate(indice = dplyr::recode(probabilite_nidification,
      "Hivernage" = "hiv",
      "Nidification probable" = "npro",
      "Nidification possible" = "npos",
      "Nidification certaine" = "ncer",
      ))
#
# les status de nidification
  df2 <- df %>%
    group_by(probabilite_nidification) %>%
    summarize(nb = n()) %>%
    adorn_totals()
  tex_df2kable(df2, suffixe = "statut")
#
# les status de nidification par espèce
  df2 <- df1 %>%
    tabyl(espece, indice) %>%
    adorn_totals(c("row", "col"))
  tex_df2kable(df2, suffixe = "statut_espece")
#
# les status de nidification par maille
  df2 <- df1 %>%
    tabyl(maille, indice) %>%
    adorn_totals(c("row", "col"))
  tex_df2kable(df2, suffixe = "statut_maille")
#
# les status de nidification par région
  df2 <- df1 %>%
    tabyl(region, indice) %>%
    adorn_totals(c("row", "col"))
  tex_df2kable(df2, suffixe = "statut_region")
#
# les status de nidification par année
  df2 <- df1 %>%
    tabyl(aaaa, indice) %>%
    adorn_totals(c("row", "col"))
  tex_df2kable(df2, suffixe = "statut_aaaa")
#
# les status de nidification par mois
  df2 <- df1 %>%
    tabyl(mm, indice) %>%
    adorn_totals(c("row", "col"))
  tex_df2kable(df2, suffixe = "statut_mm")
}
#
# source("geo/scripts/casamance.R"); df <- atlas_mailles_xlsx_json(force = TRUE)
atlas_mailles_xlsx_json <- function(force = TRUE) {
  carp()
  library(sf)
  dsn <- sprintf("%s/casamance_mailles.xlsx", varDir)
  df1 <- rio::import(dsn) %>%
    mutate(wkt = sprintf("POLYGON((%s %s,%s %s,%s %s,%s %s,%s %s))"
    , swY, swX, nwY, nwX, neY, neX, seY, seX, swY, swX)) %>%
    glimpse()
  nc1 <- st_as_sf(df1, geometry=st_as_sfc(df1$wkt), crs = 4326, remove = FALSE) %>%
    dplyr::select(maille = utm) %>%
    glimpse()
  dsn <- sprintf("%s/mailles.json", varDir)
  st_write(nc1, dsn, delete_dsn=TRUE, driver='GeoJSON')
}
#
atlas_especes_cartes_nicheurs <- function(df) {
  carp()
  library(sf)
  dsn <- sprintf("%s/casamance.json", varDir)
  casamance.sf <<- st_read(dsn) %>%
    glimpse()
  dsn <- sprintf("%s/mailles.json", varDir)
  mailles.sf <<- st_read(dsn) %>%
    glimpse()
  df1 <- df %>%
    rename(maille = carre_utm, espece = nom_francais) %>%
    mutate(indice = dplyr::recode(probabilite_nidification,
      "Nidification probable" = "PRO",
      "Nidification possible" = "POS",
      "Nidification certaine" = "CER",
      )) %>%
    dplyr::select(maille, espece, indice) %>%
    arrange(maille, espece, indice) %>%
    glimpse() %>%
    group_by(maille, espece) %>%
    summarise(indice = first(indice)) %>%
    ungroup() %>%
    glimpse()
  atlasDir <- sprintf("%s/atlas_nicheurs", texDir)
  dir.create(atlasDir, showWarnings = FALSE, recursive = TRUE)
  especes.df <- df1 %>%
    group_by(espece) %>%
    summarise(nb = n()) %>%
    mutate(camel = camel5(espece)) %>%
    mutate(especeFic = sprintf("atlas_nicheurs/%s.pdf", camel)) %>%
    mutate(atlasFic = sprintf("%s/%s", texDir, especeFic)) %>%
    glimpse()
  texFic <- sprintf("%s/%s", texDir, "atlas_especes_cartes_nicheurs.tex")
  TEX <- file(texFic, encoding = "UTF-8")
  tex <- "% <!-- coding: utf-8 -->"
  for ( i in 1:nrow(especes.df) ) {
    tex <- append(tex, sprintf("\\subsection{%s}", especes.df[i, "espece"]))
    tex <- append(tex, sprintf("\\includegraphics[width=\\malargeurgraphique]{%s}", especes.df[i, "especeFic"]))
#    atlas_espece_cartes_nicheurs(df1, especes.df[i,])
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp("texFic: %s", texFic)
}
#
# https://rquer.netlify.app/choropleth_maps
atlas_espece_cartes_nicheurs <- function(df, especes.df) {
  library(tidyverse)
  library(janitor)
  library(ggplot2)
  library(ggtext)
  library(viridis)
  library(sf)
  carp("espece: %s", especes.df[1, "espece"])
  df1 <- df %>%
    filter(espece == especes.df[[1, "espece"]])
  df2 <- df1 %>%
    group_by(indice) %>%
    summarize(nb = n()) %>%
    adorn_totals("row") %>%
    mutate(texte = sprintf("%s %s", indice, nb))
  nc1 <- mailles.sf %>%
    left_join(df1, by = c("maille")) %>%
    filter(! is.na(espece)) %>%
    mutate(couleur = dplyr::recode(indice,
      "POS" = "yellow",
      "PRO" = "orange",
      "CER" = "red",
      ))
  titre <- sprintf("%s <span style='font-size: 12pt;'>%s</span>", especes.df[1, "espece"], paste(df2$texte, sep = '', collapse = ', '))
  gg <- ggplot() +
    geom_sf(data = casamance.sf, fill = NA, color = "orange", size = 1, linetype = 1) +
    geom_sf(data = mailles.sf, fill = NA, color = "blue", size = 1, linetype = 1) +
    geom_sf(data = nc1, fill = nc1$couleur, size = 1, linetype = 1)
  gg <- gg +
    annotation_custom(ggplot_grid_text(position = "bottomright"))
  gg <- gg +
    labs(title = titre)
  gg <- gg + ggplot_geonature() +
    theme(
      plot.title = element_markdown(lineheight = 1, size = 14),
      legend.text = element_markdown(size = 14)
    )
  ggsave(especes.df[[1, "atlasFic"]], plot = gg, width = 7, height = 3.2)
#  stop("***")
}