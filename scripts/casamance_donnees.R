#
# quelques fonction pour la casamance
# auteur: Marc Gauthier
#
#
# conversion des données
# source("geo/scripts/casamance.R");donnees_ods2xlsx()
donnees_ods2xlsx <- function() {
  if (exists("_rio.df")) {
    return(rio.df)
  }
  library(readODS)
  dsn <- sprintf("%s/CONF/donnees20200303_v1.ods", varDir)
  carp("dsn: %s", dsn)
  ods.df <- read_ods(dsn, sheet = 1, col_names = TRUE) %>%
    glimpse()

}
#
# lecture des données
# source("geo/scripts/casamance.R");donnees_lire_rio()
donnees_lire_rio <- function(force = FALSE) {
  if (exists("rio.df") & force == FALSE) {
    return(rio.df)
  }
  library(rio)
  dsn <- sprintf("%s/CONF/donnees2018s2_v8.xlsx", varDir)
  dsn <- sprintf("%s/CONF/base_30_12_2018.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees2018s2_v16.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees2019s1_v4.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20190702_v2.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20190710.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20190710_v3.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20191204_v4.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20200228_v4.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20200228_v6.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20200229_v1.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20200303_v1.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20201118_v4.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20201118_v8.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20201118_v10.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20201118_v12.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20210303_v2.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20210303_v4.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20210303_v6.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20210303_v8.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20210521_v2.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20210703_v2.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20210715_v2.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20211012_v2.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20211220_v2.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20220705_v2.xlsx", varDir)
  carp("dsn: %s", dsn)
  rio.df <<- import(dsn, col_names = TRUE) %>%
    glimpse()
  return(invisible(rio.df))
}
# source("geo/scripts/casamance.R");donnees_lire_ods()
donnees_lire_ods <- function() {
  if (exists("_ods.df")) {
    return(ods.df)
  }
  library(readODS)
  dsn <- sprintf("%s/CONF/nouvelle base_18_12_2018.ods", varDir)
  dsn <- sprintf("%s/CONF/base_24_12_2018.ods", varDir)
  dsn <- sprintf("%s/CONF/base globale 30_06_2019.ods", varDir)
  dsn <- sprintf("%s/CONF/nouvelle base Apalis_globale_juin2022.ods", varDir)
  carp("dsn: %s", dsn)
  ods.df <<- read_ods(dsn, col_names = TRUE) %>%
    glimpse()
  return(invisible(ods.df))
}
#
# cohérence données espèces
# source("geo/scripts/casamance.R");donnees_especes()
donnees_especes <- function() {
  carp()
  library(tidyverse)
  library(rio)
  df <- donnees_lire()
  especes.df <- especes_lire_xls()
  carp('nom francais')
  df %>%
    filter(! nom_francais %in% especes.df$nom_francais) %>%
    glimpse()
  carp('nom anglais')
  df %>%
    filter(! nom_anglais %in% especes.df$nom_anglais) %>%
    glimpse()
  carp('nom scientifique')
  df %>%
    filter(! nom_scientifique %in% especes.df$nom_scientifique) %>%
    glimpse()
  carp('especes')
  df1 <- df %>%
    mutate(francais = ifelse(nom_francais %in% especes.df$nom_francais, '', 'ko')) %>%
    mutate(scientifique = ifelse(nom_scientifique %in% especes.df$nom_scientifique, '', 'ko')) %>%
    mutate(anglais = ifelse(nom_anglais %in% especes.df$nom_anglais, '', 'ko')) %>%
    filter(anglais == 'ko') %>%
    left_join(especes.df, by = c('nom_scientifique' = 'nom_scientifique')) %>%
    dplyr::select(nom_scientifique, nom_francais.x, nom_francais.y, nom_anglais.x, nom_anglais.y) %>%
    arrange(nom_scientifique) %>%
    glimpse() %>%
    print(n = 50, na.print = "")
  dsn <- sprintf("%s/CONF/especes_mga.xlsx", varDir)
  export(df1, dsn)
  carp('dsn: %s', dsn)
}
#
# lecture des données
# source("geo/scripts/casamance.R");donnees_lire()
donnees_lire <- function(force = TRUE) {
  carp()
  library(tidyverse)
  library(janitor)
  system.time(df <- donnees_lire_rio(force))
  df <- df %>%
    clean_names() %>%
    dplyr::select(c(1:21))
  return(invisible(df))
}
#
# les noms d'espèces en double
# source("geo/scripts/casamance.R");donnees_dupes()
donnees_dupes <- function() {
  carp()
  library(tidyverse)
  library(janitor)
  library(knitr)
  library(kableExtra)
  df <- donnees_lire()
  df1 <- df %>%
    glimpse() %>%
    clean_names() %>%
    dplyr::select(c(1:21)) %>%
    remove_empty(c("rows", "cols")) %>%
    group_by(nom_francais, nom_anglais, nom_scientifique) %>%
    summarize(nb = n()) %>%
    print(n = 500) %>%
    ungroup() %>%
    glimpse()
  df2 <- df1 %>%
    get_dupes(nom_francais) %>%
    dplyr::select(nom_francais, nom_anglais, nom_scientifique, nb) %>%
    mutate(dup = "fr")
  df3 <- df1 %>%
    get_dupes(nom_anglais) %>%
    dplyr::select(nom_francais, nom_anglais, nom_scientifique, nb) %>%
    mutate(dup = "en")
  df4 <- df1 %>%
    get_dupes(nom_scientifique) %>%
    dplyr::select(nom_francais, nom_anglais, nom_scientifique, nb) %>%
    mutate(dup = "sc")
  carp("concatenation")
  df5 <- rbind(df2, df3, df4) %>%
    dplyr::select(nom_francais, nom_anglais, nom_scientifique, nb) %>%
    distinct() %>%
    glimpse() %>%
    arrange(nom_francais) %>%
    print(n = 100)
  print(kable(df5, format = "pipe"))
  return(invisible(df))
}
#
# lecture des données
donnees_lire_csv <- function() {
  library(readr)
  dsn <- sprintf("%s/casamance_donnees.csv", varDir)
  df <- read_tsv(dsn)
  return(invisible(df))
}

#
# recherche des données 2019
# source("geo/scripts/casamance.R");donnees_2019_lire()
donnees_base_lire <- function() {
  carp()
  library(tidyverse)
  library(janitor)
  library(rio)
  dsn <- sprintf("%s/CONF/donnees2018s2_v8.xlsx", varDir)
#  dsn <- sprintf("%s/CONF/base_30_12_2018.xlsx", varDir)
#  dsn <- sprintf("%s/CONF/donnees2018s2_v16.xlsx", varDir)
#  dsn <- sprintf("%s/CONF/donnees2019s1_v4.xlsx", varDir)
  dsn <- sprintf("%s/CONF/donnees20190702_v3.xlsx", varDir)
  carp("dsn: %s", dsn)
  df <- import(dsn, col_names = TRUE)
  df <- donnees_lire_rio() %>%
#    glimpse() %>%
    dplyr::select(c(1:21))
  return(invisible(df))
}
# source("geo/scripts/casamance.R");donnees_base_cmp()
donnees_base_cmp <- function() {
  carp()
  library(tidyverse)
  library(janitor)
  library(rio)
  carp('excel')
  xls.df <- donnees_lire_rio() %>%
    mutate(DateObs = format(DateObs, format = "%Y-%m-%d")) %>%
    replace(., is.na(.), "") %>%
    glimpse()
  carp('sql')
  sql.df <- sql_lire() %>%
    glimpse()
  df <- left_join(sql.df, xls.df) %>%
    filter(is.na(NomScientifique)) %>%
    dplyr::select(
      NomFrancais,
      DateObs,
      Region,
      Ville,
      Lieudit,
      CarreUTM,
      Contact,
      Age,
      Effectif,
      ProbabiliteNidification,
      DontMale,
      DontFemelle,
      Commentaire,
      Observateur,
      Milieu
    ) %>%
    glimpse()
  dsn <- sprintf("%s/CONF/delta_base.xlsx", varDir)
  export(df, dsn)
  carp("dsn: %s", dsn)
}
#
# comparaison des mailles
# source("geo/scripts/casamance.R");donnees_mailles()
donnees_mailles <- function() {
  carp()
  library(tidyverse)
  library(janitor)
  library(rio)
  mailles.df <- mysql_table_lire("casamance_mailles")
  donnees.df <- mysql_table_lire("casamance_donnees2017")
  donnees_mailles.df <- donnees.df %>%
    group_by(CarreUTM) %>%
    summarize(nb = n())
  df1 <- donnees_mailles.df %>%
    filter(CarreUTM %notin% mailles.df$utm) %>%
    arrange(-nb) %>%
    glimpse()

}