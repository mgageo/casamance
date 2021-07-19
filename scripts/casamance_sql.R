#
# quelques fonctions pour la casamance
# auteur: Marc Gauthier
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