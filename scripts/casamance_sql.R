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
# lecture des données, export sql en format csv
# source("geo/scripts/casamance.R");sql_lire()
sql_lire <- function() {
  library(rio)
  library(tidyverse)
  dsn <- sprintf("%s/CONF/casamance_donnees_20190108.csv", varDir)
  dsn <- sprintf("%s/CONF/casamance_donnees_20190430.csv", varDir)
  dsn <- sprintf("%s/CONF/casamance_donnees2017.csv", varDir)
  carp("dsn: %s", dsn)
  df <- import(dsn, encoding = "UTF-8") %>%
#    mutate(id=row_number()) %>%
    filter(grepl("2019", DateObs)) %>%
#    glimpse() %>%
    dplyr::select(-id) %>%
    arrange(NomFrancais, DateObs, Region, Ville, Lieudit) %>%
    distinct(NomFrancais, DateObs, Region, Ville, Lieudit, CarreUTM, Contact, Age, Effectif, ProbabiliteNidification, DontMale, DontFemelle, Commentaire, Observateur, Milieu)
  return(invisible(df))
}

# source("geo/scripts/casamance.R");sql_table_mailles_ecrire()
sql_table_mailles_ecrire <- function(table = "casamance_mailles") {
  library(rio)
  library(tidyverse)
  library(rio)
  library(dbplyr)
  carp()
  mysql_connect(user = "casamance")
  dsn <- sprintf("%s/utm_json.csv", cfgDir)
  df <- rio::import(dsn) %>%
    dplyr::select(! matches("^V\\d+")) %>%
    glimpse()
  stop("****")
  sql <- "DROP TABLE IF EXISTS %s;"
  sql <- sprintf(sql, table)
  db_SendStatement(db_con, sql) %>%
    glimpse()
  copy_to(dest = db_con, df = df, name = table, overwrite = TRUE)
  return(invisible(df))
}
#
# récupération des tables sur le serveur en format xlsx
# source("geo/scripts/casamance.R");sql_tables_get()
sql_tables_get <- function() {
  carp()
  sql_table_get("casamance_mailles")
  sql_table_get("casamance_especes")
  sql_table_get("casamance_donnees2017")
}
sql_table_get <- function(table) {
  library(httr)
  url <- sprintf("https://oiseaux-casamance.com/outils/casamance_sauve.php?action=xlsx&export=%s", table)
  carp(url)
  dsn <- sprintf("%s/%s.xlsx", varDir, table)
  GET(url, write_disk(dsn, overwrite = TRUE))
  carp("dsn: %s", dsn)
}
