# <!-- coding: utf-8 -->
# atlas
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
atlasDir <- sprintf("%s/bvi35/CouchesAtlas", Drive)
#
##
# setwd("d:/web"); source("geo/scripts/atlas.R");nc <- atlas_test(force = TRUE)
atlas_test <- function(force = FALSE) {
  library(tidyverse)
  library(sf)
  carp()
  df <- read.table(text="zone;grille;departements
#d35;utm;35
#d35;l93;35
#bzh;l93;22,29,35,56
#bzha;l93;22,29,35,44,56
#bzh;utm;22,29,35,56
#bzha;utm;22,29,35,44,56
bzh;ntf;22,29,35,56
bzha;ntf;22,29,35,44,56
", header=TRUE, sep=";", blank.lines.skip = TRUE, stringsAsFactors = FALSE, quote = "")
  for(i in 1:nrow(df)) {
    carp("***** i: %s", i)
    atlas_zone <<- df[[i, "zone"]]
    atlas_grille <<- df[[i, "grille"]]
    atlas_departements <<- str_split(df[[i, "departements"]], ",")[[1]]
    atlas_zone_grille_voisines()
  }
}
atlas_zone_grille_voisines <- function(force = FALSE) {
  library(tidyverse)
  library(sf)
  carp()
  ign.sf <- atlas_zone_ign()
  dsn <- sprintf("%s/grille_%s.geojson", atlasDir, atlas_grille)
  nc <- st_read(dsn) %>%
    dplyr::select(-any_of(c("tessellate", "extrude", "visibility", "surface", "voisines", "voisines_nb"))) %>%
    glimpse()
  nc1 <- atlas_mailles_surface(nc, ign.sf)
  df1 <- atlas_grille_voisines(nc1)
  nc2 <- nc1 %>%
    left_join(df1, by = c("Maille")) %>%
    glimpse()
  dsn <- sprintf("%s/%s_%s_voisines.geojson", atlasDir, atlas_zone, atlas_grille)
  st_write(nc2, dsn, append = FALSE, delete_dsn = TRUE)
  carp("dsn: %s", dsn)
  return(invisible(nc1))
}
# les mailles voisines
atlas_grille_voisines <- function(nc, force = TRUE) {
  carp("détermination des voisines")
  for (i in 1:nrow(nc)) {
    voisines <- st_intersects(x = nc, y = nc[i, ], sparse = F)
    voisines <- nc[voisines, ]$Maille
    voisines <- voisines[! voisines %in% c(nc[i, "Maille"])]
    nc[i, "voisines_nb"] <-  length(voisines)
    nc[i, "voisines"] <- paste(voisines, collapse = "|")
  }
  df <- nc %>%
    st_drop_geometry() %>%
    dplyr::select(Maille, voisines, voisines_nb) %>%
    replace_na(list(voisines_nb = 0)) %>%
    glimpse()
  return(invisible(df))
}
#
# détermination de la zone
atlas_zone_ign <- function(les_departements = c("22", "29", "35", "56"), force = FALSE, debug = TRUE) {
  library(tidyverse)
  library(sf)
  library(rmapshaper)
  dsn <- sprintf("%s/%s_zone.geojson", atlasDir, atlas_zone)
  carp("dsn: %s", dsn)
  if (file.exists(dsn)) {
    nc2 <- st_read(dsn)
    return(invisible(nc2))
  }
  carp("ign des départements")
  ign.sf <- ign_adminexpress_lire_sf("DEPARTEMENT") %>%
    filter(INSEE_DEP %in% les_departements) %>%
    st_transform(4326) %>%
    glimpse()
  if (debug) {
    plot(st_geometry(ign.sf))
  }
  carp("un seul objet")
# https://mtes-mct.github.io/parcours_r_module_analyse_spatiale/les-op%C3%A9rations-g%C3%A9om%C3%A9triques.html
  nc1 <- ign.sf %>%
    mutate(insee = "tous") %>%
    group_by(insee) %>%
    summarise(do_union = T) %>%
    glimpse()
  carp("on simplifie avant: %s", object.size(nc1))
  nc2 <- ms_simplify(nc1, keep = 0.05, keep_shapes = TRUE)
  carp("on simplifie après: %s", object.size(nc2))
  if (debug) {
    plot(st_geometry(nc2), border = "blue", add = TRUE)
  }
  dsn <- sprintf("%s/%s_zone.geojson", atlasDir, atlas_zone)
  st_write(nc2, dsn, append = FALSE, delete_dsn = TRUE)
  carp("dsn: %s", dsn)
  return(invisible(nc2))
}
#
# calcul de la surface d'une maille
atlas_mailles_surface <- function(nc, nc2, force = FALSE, debug = TRUE) {
  library(tidyverse)
  library(sf)
  library(rmapshaper)
  carp()
  if (debug) {
    plot(st_geometry(nc))
  }
  if (debug) {
    plot(st_geometry(nc2), border = "blue", add = TRUE)
  }
  carp("intersection avec les départements, c'est lent ...")
  nc3 <- st_intersection(nc2, nc) %>%
    mutate(surface = as.numeric(round(st_area(.), 0))) %>%
    arrange(surface) %>%
    glimpse()
  carp("les mailles hors départements")
  nc4 <- nc %>%
    filter(Maille %notin% nc3$Maille) %>%
    glimpse()
  if (debug) {
    plot(st_geometry(nc4), border = "red", add = TRUE)
  }
  carp("les mailles des départements")
  nc5 <- nc %>%
    filter(Maille %in% nc3$Maille) %>%
    glimpse()
  if (debug) {
    plot(st_geometry(nc5), border = "yellow", add = TRUE)
  }
  df3 <- nc3 %>%
    st_drop_geometry() %>%
    dplyr::select(Maille, surface)
  nc6 <- nc5 %>%
    left_join(df3, by = c("Maille")) %>%
    glimpse()
  return(invisible(nc6))
}
#
## comment faire une grille pour une zone d'un ou plusieurs départements
#
# intersection de la grille avec la zone
# suppression des mailles dont la surface est majoritairement hors de cette zone
#
# setwd("d:/web"); source("geo/scripts/atlas.R");nc <- atlas_grilles_zones_test(force = TRUE)
atlas_grilles_zones_test <- function(force = FALSE, debug = TRUE) {
  library(tidyverse)
  library(sf)
  carp()
  df <- read.table(text="zone;grille;departements
bzh;utm;22,29,35,44,56
bzh;l93;22,29,35,44,56
bzh;ntf;22,29,35,44,56
bzha;utm;22,29,35,56
bzha;l93;22,29,35,56
bzha;ntf;22,29,35,56
d22;utm;22
d22;l93;22
d22;ntf;22
d29;utm;29
d29;l93;29
d29;ntf;29
d35;utm;35
d35;l93;35
d35;ntf;35
d44;utm;44
d44;l93;44
d44;ntf;44
d56;utm;56
d56;l93;56
d56;ntf;56
", header=TRUE, sep=";", blank.lines.skip = TRUE, stringsAsFactors = FALSE, quote = "")
  texFic <- sprintf("%s/atlas_grilles_tex.tex", texDir)
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
  for(i in 1:nrow(df)) {
    carp("***** i: %s", i)
    atlas_zone <<- df[[i, "zone"]]
    atlas_grille <<- df[[i, "grille"]]
    atlas_departements <<- str_split(df[[i, "departements"]], ",")[[1]]
    atlas_grille_zone()
    dsn <- sprintf("grille_%s_%s.pdf", atlas_zone, atlas_grille)
    t <- sprintf("\\includegraphics[width=.4\\textwidth,height=.4\\textwidth,keepaspectratio]{%s}\\\\", dsn)
    tex <- append(tex, t)
  }
  write(tex, file = TEX, append = FALSE)
  carp("texFic: %s", texFic)
  tex_pdflatex("atlas_grilles.tex")
}
# setwd("d:/web"); source("geo/scripts/atlas.R"); atlas_grilles_zones_tex(force = TRUE)
atlas_grilles_zones_tex <- function(force = FALSE, debug = TRUE) {
  tex_pdflatex("atlas_grilles.tex")
}
# setwd("d:/web"); source("geo/scripts/atlas.R");nc <- atlas_grille_zone(force = TRUE)
atlas_grille_zone <- function(force = FALSE, debug = TRUE) {
  library(tidyverse)
  library(sf)
  carp()
  par(mar = c(0,0,0,0), oma = c(0,0,0,0))
  dsn <- sprintf("%s/grille_%s.geojson", atlasDir, atlas_grille)
  nc <- st_read(dsn) %>%
    dplyr::select(-any_of(c("tessellate", "extrude", "visibility", "surface", "voisines", "voisines_nb"))) %>%
    st_transform(2154) %>%
    glimpse()
  nc1 <- atlas_departements_voisins()
  zone.sf <- nc1 %>%
    filter(INSEE_DEP %in% atlas_departements) %>%
    glimpse() %>%
    mutate(insee = "zone") %>%
    group_by(insee) %>%
    summarise(do_union = T) %>%
    glimpse()
  if (debug) {
    plot(st_geometry(zone.sf), col = "grey")
  }
  hors.sf <- nc1 %>%
    filter(INSEE_DEP %notin% atlas_departements) %>%
    mutate(insee = "hors") %>%
    group_by(insee) %>%
    summarise(do_union = T) %>%
    glimpse()
  if (debug) {
    plot(st_geometry(hors.sf), add = TRUE, col = "red")
  }
  if (debug) {
    plot(st_geometry(nc), add = TRUE, border = "black")
  }
  carp("les deux zones")
  nc2 <- rbind(zone.sf, hors.sf)
  carp("l'intersection")
  nc3 <- st_intersection(nc, nc2) %>%
    mutate(surface = as.numeric(round(st_area(.), 0))) %>%
    arrange(Maille, surface) %>%
    glimpse()
  nc4 <- nc3 %>%
    filter(insee == "zone")
  nc5 <- nc %>%
    filter(Maille %in% nc4$Maille)
  if (debug) {
    plot(st_geometry(nc5), add = TRUE, border = "blue", lwd = 2)
  }
  carp("les mailles frontalières")
  nc6 <- nc3 %>%
    st_drop_geometry() %>%
    filter(Maille %in% nc4$Maille) %>%
    arrange(Maille, surface) %>%
    group_by(Maille) %>%
    summarise(across(everything(), last)) %>%
    filter(insee == "hors") %>%
    glimpse()
  nc7 <- nc %>%
    filter(Maille %in% nc6$Maille)
  if (debug) {
    plot(st_geometry(nc7), add = TRUE, border = "orange", lwd = 2)
  }
  nc8 <- nc5 %>%
    filter(Maille %notin% nc6$Maille)
  if (debug) {
    plot(st_geometry(nc8), add = TRUE, border = "green", lwd = 3)
  }
  titre <- sprintf("%s %s nb mailles: %s", atlas_zone, atlas_grille, nrow(nc8))
  texte <- sprintf("@ Marc Gauthier CC-BY-NC-ND")
  geo_legende(titre, pos = "topleft")
  geo_legende(texte, pos = "bottomright")
  dsn <- sprintf("%s/grille_%s_%s.pdf", texDir, atlas_zone, atlas_grille)
  dev2pdf(dsn)
  dsn <- sprintf("%s/grille_%s_%s.geojson", atlasDir, atlas_zone, atlas_grille)
  st_write(st_transform(nc8, 4326), dsn, append = FALSE, delete_dsn = TRUE)
  carp("dsn: %s", dsn)
}
# détermination des départements voisins de la Bretagne
# setwd("d:/web"); source("geo/scripts/atlas.R");nc <- atlas_departements_voisins(force = TRUE)
atlas_departements_voisins <- function(les_departements = c("22", "29", "35", "44", "56"), force = FALSE, debug = TRUE) {
  library(tidyverse)
  library(sf)
  library(rmapshaper)
  dsn <- sprintf("%s/departements_voisins.geojson", atlasDir)
  if (file.exists(dsn)) {
    nc2 <- st_read(dsn)
    return(invisible(nc2))
  }
  carp("l'ensemble des départements")
  nc1 <- ign_adminexpress_lire_sf("DEPARTEMENT") %>%
    st_transform(2154) %>%
    glimpse()
  if (debug) {
    plot(st_geometry(nc1))
  }
  carp("on simplifie avant: %s", object.size(nc1))
  nc2 <- ms_simplify(nc1, keep = 0.05, keep_shapes = TRUE)
  carp("on simplifie après: %s", object.size(nc2))
  if (debug) {
    plot(st_geometry(nc2), border = "blue", add = TRUE)
  }
  carp("que les départements de la zone")
  nc3 <- nc2 %>%
    filter(INSEE_DEP %in% les_departements) %>%
    glimpse()
  if (debug) {
    plot(st_geometry(nc3), border = "black", add = TRUE)
  }
  carp("l'intersection")
  nc4 <- st_intersection(st_buffer(nc3, 100), nc2) %>%
    glimpse()
  df4 <- nc4 %>%
    st_drop_geometry() %>%
    group_by(dept = INSEE_DEP.1) %>%
    summarize(nb = n())
  nc5 <- nc2 %>%
    filter(INSEE_DEP %in% df4$dept) %>%
    glimpse()
  if (debug) {
    plot(st_geometry(nc5), border = "red", add = TRUE)
  }
  st_write(nc5, dsn, append = FALSE, delete_dsn = TRUE)
  carp("dsn: %s", dsn)
}
#
## le nom des espèces
#
atlas_espece_recode <- function(df) {
  library(tidyverse)
  library(utf8)
  df <- df %>%
#    mutate(camel = camel5(espece)) %>%
    mutate(espece = dplyr::recode(espece,
      "Echasse blanche" = "Échasse blanche",
      "Epervier d'Europe" = "Épervier d'Europe",
      "Erismature rousse" = "Érismature rousse",
      "Etourneau sansonnet" = "Étourneau sansonnet",
      "Roitelet triple-bandeau" = "Roitelet à triple bandeau",
      "Bergeronnette flavéole (M.f.flavissima)" = "Bergeronnette flavéole",
      "Pigeon biset (origine naturelle)" = "Pigeon biset",
      "Bergeronnette printanière sp." = "Bergeronnette printanière",
      "Bergeronnette printanière type" = "Bergeronnette printanière",
      "Héron garde-bœufs" = "Héron garde-boeufs",
    ))
  df$espece <- as_utf8(df$espece, normalize = TRUE)
  return(invisible(df))
}
#
## les calculs d'indice
#
# setwd("d:/web"); source("geo/scripts/atlas.R");atlas_far_test()
atlas_far_test <- function() {
  df <- tribble(
    ~"catégorie FAR", ~rgb, ~pa, ~valeur, ~UICN,
    "très robuste", "00AF50", 10, ">=5", "Préoccupation mineure (LC)",
    "robuste", "93CF55", 4, "<5", "Préoccupation mineure (LC)",
    "à surveiller", "EEFFFA", 1.5, "<2", "Espèce quasi menacée (NT)",
    "sensible", "00AF50", 0.8, "<1", "Espèce vulnérable (VU)",
    "fragile", "E46D09", 0.5, "<0.6", "Espèce en danger (EN)",
    "très fragile", "FFFD97", 0.2, "<0.25", "En danger critique d'extinction (CR)",
    "éteint", "732EA2", 0, "=0", "Disparue au niveau régional (RE)"
    ) %>%
    glimpse()
  df1 <- atlas_far(df)
  tex <- atlas_far_tex(df1)
  print(tex)
}
atlas_far <- function(df) {
  df <- df %>%
    mutate(FAR = ifelse(pa > 5, "très robuste", "robuste")) %>%
    mutate(FAR = ifelse(pa < 2, "à surveiller", FAR)) %>%
    mutate(FAR = ifelse(pa < 1, "sensible", FAR)) %>%
    mutate(FAR = ifelse(pa < .6, "fragile", FAR)) %>%
    mutate(FAR = ifelse(pa < .25, "très fragile", FAR)) %>%
    mutate(FAR = ifelse(pa == 0, "éteint", FAR)) %>%
    mutate(couleur = dplyr::recode(FAR,
      "très robuste" = "00AF50",
      "robuste" = "93CF55",
      "à surveiller" = "EEFFFA",
      "sensible" = "FFFE00",
      "fragile" = "E46D09",
      "très fragile" = "FB0103",
      "éteint" = "732EA2",
      )) %>%
    glimpse()
  return(invisible(df))
}
atlas_far_tex <- function(df) {
  df$pa <- cell_spec(round(df$pa, 2), color = df$couleur, format = "html")
  misc_print(df)
  tex <- knitr::kable(df, "latex"
    , longtable = T
    , booktabs = TRUE
    , linesep = c(rep('', 9),'\\addlinespace')
    , digits = 1
    , format.args = list(decimal.mark = ',')
    ) %>%
    kable_styling(
      position = "left",
      latex_options = c("repeat_header"),
      repeat_header_text = "\\textit{(suite)}"
    )
# \cellcolor{yellow}
  tex <- tex %>%
    str_replace_all(' <span style="     color: ', '\\\\cellcolor[HTML]{') %>%
    str_replace_all(' !important;" >', '}') %>%
    str_replace_all('</span> ', '') %>%
    str_replace_all('</span>', '')
  return(invisible(tex))
}