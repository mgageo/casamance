# <!-- coding: utf-8 -->
#
# la partie atlas : les cartes
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
#
# source("geo/scripts/casamance.R");atlas_jour(force = FALSE)
atlas_jour <- function(force = FALSE) {
  library(tidyverse)
  library(janitor)
  carp()
  df <- donnees_lire(force = force)
  df1 <- df %>%
    clean_names() %>%
    rename(maille = carre_utm, espece = nom_francais) %>%
#    filter(espece %in% c("Martin-pêcheur huppé", "Tantale Ibis")) %>%
#    filter(espece %in% c("Aigle couronné")) %>%
    mutate(aaaa = strftime(date_obs, format="%Y")) %>%
    mutate(mm = strftime(date_obs, format="%m")) %>%
    replace_na(list(probabilite_nidification = "Simple présence")) %>%
    mutate(indice = dplyr::recode(probabilite_nidification,
      "Simple présence" = "sp",
      "Hivernage" = "hiv",
      "Nidification probable" = "npro",
      "Nidification possible" = "npos",
      "Nidification certaine" = "ncer",
      )) %>%
    mutate(periode = ifelse(aaaa < "2008", "histo", "actuel")) %>%
    mutate(periode = "toutes") %>%
#    filter(periode == "actuel") %>%
    glimpse()
  atlas_especes_cartes(df1, force = force)
  atlas_stat(df1)
  atlas_especes_mois(df1, force = force)
  tex_pdflatex("atlas.tex")
}
#
#
atlas_especes_cartes <- function(df, force = FALSE) {
  carp()
  df1 <- df %>%
    filter(! grepl("^Hivernage", probabilite_nidification))
  atlas_especes_cartes_nicheurs(df1)
}
#
atlas_especes_mois <- function(df1, force = FALSE) {
  carp()
#
# les status de nidification par mois
  list1 <- df1 %>%
    tabyl(indice, mm, espece) %>%
    adorn_totals(c("row", "col"))
  names1 <- names(list1)
  texFic <- sprintf("%s/%s", texDir, "atlas_especes_mois.tex")
  TEX <- file(texFic, encoding = "UTF-8")
  tex <- "% <!-- coding: utf-8 -->"
  for (i in 1:length(list1)) {
    carp("espece: %s", names1[[i]])
    tex <- append(tex, sprintf("\\subsection{%s}", names1[[i]]))
    df3 <- list1[[i]]
    t <- tex_df2kable(df3, suffixe = "espece")
    tex <- append(tex, t)
#    glimpse(df3); stop("*****")
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp("texFic: %s", texFic)
}
#
# source("geo/scripts/casamance.R"); df <- atlas_stat()
atlas_stat <- function(df1) {
  library(tidyverse)
  library(janitor)
  carp()
#
# les status de nidification
  df2 <- df1 %>%
    group_by(probabilite_nidification) %>%
    summarize(nb = n()) %>%
    adorn_totals()
#  glimpse(df2); stop("****")
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
  nc1 <- st_as_sf(df1, geometry = st_as_sfc(df1$wkt), crs = 4326, remove = FALSE) %>%
    dplyr::select(maille = utm) %>%
    glimpse()
  dsn <- sprintf("%s/mailles.json", varDir)
  st_write(nc1, dsn, delete_dsn = TRUE, driver = "GeoJSON")
}
#
atlas_especes_cartes_nicheurs <- function(df) {
  carp()
  library(sf)
  dsn <- sprintf("%s/casamance.json", varDir)
  casamance.sf <<- st_read(dsn) %>%
    st_transform("EPSG:31028") %>%
    glimpse()
  dsn <- sprintf("%s/mailles.json", varDir)
  mailles.sf <<- st_read(dsn) %>%
    st_transform("EPSG:31028") %>%
    glimpse()
  dsn <- sprintf("%s/osmpos_v2.tif", ogcDir)
  raster_layer <<- terra::rast(dsn)
  df0 <- df %>%
    mutate(indice = dplyr::recode(probabilite_nidification,
      "Simple présence" = "SP",
      "Hivernage" = "HIV",
      "Nidification probable" = "PRO",
      "Nidification possible" = "POS",
      "Nidification certaine" = "CER",
      )) %>%
    filter(indice %in% c("CER", "PRO", "POS", "SP")) %>%
    glimpse()
  df1 <- df0 %>%
    dplyr::select(maille, espece, periode, indice) %>%
    arrange(maille, espece, periode, indice) %>%
    glimpse() %>%
    group_by(maille, espece, periode) %>%
    summarise(indice = first(indice)) %>%
    ungroup() %>%
    glimpse()
  atlasDir <- sprintf("%s/atlas_nicheurs", texDir)
  dir.create(atlasDir, showWarnings = FALSE, recursive = TRUE)
  especes.df <- df0 %>%
    group_by(espece, nom_scientifique, nom_anglais) %>%
    summarise(nb = n()) %>%
    mutate(camel = camel5(espece)) %>%
    mutate(especeFic = sprintf("atlas_nicheurs/%s.png", camel)) %>%
    mutate(atlasFic = sprintf("%s/%s", texDir, especeFic)) %>%
    glimpse()

#  stop("######")
  texFic <- sprintf("%s/%s", texDir, "atlas_especes_cartes_nicheurs.tex")
  TEX <- file(texFic, encoding = "UTF-8")
  tex <- "% <!-- coding: utf-8 -->"
#
# le template tex
  dsn <- sprintf("%s/atlas_espece_tpl.tex", texDir)
  template <- readLines(dsn, encoding="UTF-8")
  for (i in 1:nrow(especes.df)) {
    carp("i: %s/%s", i, nrow(especes.df))
#    stop("*$$$$$$$$$$$$$")
#    tex <- append(tex, sprintf("\\subsection{%s}", especes.df[i, "espece"]))
#    tex <- append(tex, sprintf("\\includegraphics[width=\\malargeurgraphique]{%s}", especes.df[i, "especeFic"]))
    df2 <- atlas_espece_cartes_nicheurs(df1, especes.df[i,]) %>%
      dplyr::select(-texte) %>%
      pivot_wider(names_from = indice, values_from = nb)
#    glimpse(df2); stop("ùùùùùùùùù")
    tpl <- template
    tpl <- tex_df2tpl(especes.df, i, tpl)
    tpl <- tex_df2tpl(df2, 1, tpl)
    tex <- append(tex, tpl)
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
    filter(espece == especes.df[[1, "espece"]]) %>%
    glimpse()
#  stop("******")
  df2 <- df1 %>%
    group_by(indice) %>%
    summarize(nb = n()) %>%
    adorn_totals("row") %>%
    mutate(texte = sprintf("%s %s", indice, nb))
  nc1 <- mailles.sf %>%
    left_join(df1, by = c("maille")) %>%
    filter(periode == "actuel") %>%
    filter(! is.na(espece)) %>%
    mutate(couleur = dplyr::recode(indice,
      "SP" = "grey",
      "POS" = "yellow",
      "PRO" = "orange",
      "CER" = "red",
      ))
  nc2 <- mailles.sf %>%
    left_join(df1, by = c("maille")) %>%
    filter(periode == "histo") %>%
    filter(! is.na(espece)) %>%
    mutate(couleur = dplyr::recode(indice,
      "SP" = "grey",
      "POS" = "yellow",
      "PRO" = "orange",
      "CER" = "red",
      )) %>%
    st_centroid()
#  stop("****")
  titre <- sprintf("%s <span style='font-size: 12pt;'>%s</span>", especes.df[1, "espece"], paste(df2$texte, sep = "", collapse = ", "))
  gg <- ggplot() +
    geom_sf(data = casamance.sf, fill = NA, color = "orange", size = 1, linetype = 1) +
    geom_sf(data = mailles.sf, fill = NA, color = "blue", size = .5, linetype = 1) +
    geom_sf(data = nc1, fill = nc1$couleur, color = "blue", size = .5, linetype = 1) +
    geom_sf(data = nc2, fill = NA, color = nc2$couleur, size = 2.5, linetype = 1)
  gg <- gg +
    annotation_custom(ggplot_grid_texte(position = "bottomright"))
#  gg <- gg +
#    labs(title = titre)
  gg <- gg + ggplot_casamance() +
    theme(
      plot.title = element_markdown(lineheight = 1, size = 14),
      legend.text = element_markdown(size = 14)
    )
  ggsave(especes.df[[1, "atlasFic"]], plot = gg, width = 7, height = 3.2)
  return(invisible(df2))
#  stop("***")
}
#

atlas_espece_cartes_nicheurs <- function(df, especes.df) {
  library(tidyverse)
  carp("espece: %s", especes.df[1, "espece"])
  df1 <- df %>%
    filter(espece == especes.df[[1, "espece"]]) %>%
    glimpse()
#  stop("******")
  df2 <- df1 %>%
    group_by(indice) %>%
    summarize(nb = n()) %>%
    adorn_totals("row") %>%
    mutate(texte = sprintf("%s %s", indice, nb))
  nc1 <<- mailles.sf %>%
    left_join(df1, by = c("maille")) %>%
    filter(! is.na(espece)) %>%
    mutate(couleur = dplyr::recode(indice,
      "SP" = "grey",
      "POS" = "yellow",
      "PRO" = "orange",
      "CER" = "red",
      )) %>%
    dplyr::select(couleur) %>%
    glimpse()
  plotRGB(raster_layer, axes = FALSE, add = FALSE)
  plot(casamance.sf, add = TRUE, border = "orange", col = NA)
  plot(mailles.sf, add = TRUE, border = "blue", col = NA, lwd = .2)
  plot(st_geometry(nc1), add = TRUE, border = "blue", col = nc1$couleur, lwd = .2)
  mtext(text = "OpenStreetMap", side = 1, line = -1, adj = .99)
  stop("*****")
  dsn <- especes.df[[1, "atlasFic"]]
  dev.copy(png, dsn, width=dim(raster_layer)[2], height=dim(raster_layer)[1])
#  dev.copy(png, dsn, width=dim(raster_layer)[2], height=dim(raster_layer)[1])
  graphics.off()
  return(invisible(df2))
}
#
# version mapsf
atlas_espece_cartes_nicheurs <- function(df, especes.df) {
  library(tidyverse)
  library(mapsf)
  carp("espece: %s", especes.df[1, "espece"])
  df1 <- df %>%
    filter(espece == especes.df[[1, "espece"]]) %>%
    glimpse()
#  stop("******")
  df2 <- df1 %>%
    group_by(indice) %>%
    summarize(nb = n()) %>%
    adorn_totals("row") %>%
    mutate(texte = sprintf("%s %s", indice, nb))
  nc1 <<- mailles.sf %>%
    left_join(df1, by = c("maille")) %>%
    filter(! is.na(espece)) %>%
    mutate(couleur = dplyr::recode(indice,
      "SP" = "grey",
      "POS" = "yellow",
      "PRO" = "orange",
      "CER" = "red",
      )) %>%
    dplyr::select(couleur) %>%
    glimpse()
  dsn <- especes.df[[1, "atlasFic"]]
  mf_theme(mar = c(0, 0, 0, 0))
  mf_export(
    x = raster_layer,
    filename = dsn,
    width = 2048,
    expandBB = c(0, 0, 0, 0),
  )
  mf_raster(raster_layer, axes = FALSE, add = TRUE)
  mf_map(x = mailles.sf, border = "blue", col = NA, lwd = .2, pch = 19, add = TRUE)
  mf_map( x = nc1, add = TRUE, border = "blue", col = nc1$couleur, lwd = .2)
  mf_credits(
    txt = "© Apalis © Stadia Maps © OpenStreetMap contributors",
    pos = "bottomright",
    cex = 2.0
  )
  dev.off()
#  stop("*****")
  return(invisible(df2))
}
