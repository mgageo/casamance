# <!-- coding: utf-8 -->
#
# les traitements sur les données en base postgresql
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
# la base postgresql/postgis est dans un conteneur docker, avec un espace disque partagé
# docker run -p 5432:5432 --name pg_visionature -e POSTGRES_PASSWORD=tgbtgb -v d:/docker/postgres-data/:/tmp -d postgis/postgis
#
# https://db.rstudio.com/best-practices/schema/
# https://www.r-bloggers.com/using-postgresql-in-r-a-quick-how-to/

# https://dzone.com/articles/using-jsonb-in-postgresql-how-to-effectively-store
# https://www.postgis.us/presentations/PGOpen2018_data_loading.pdf
# https://stackoverflow.com/questions/34688465/how-do-i-run-a-sql-file-of-inserts-through-docker-run
#
mysql_user_old <- FALSE
mysql_connect <- function(user = "casamance") {
  library(DBI)
  library(RMySQL)
  if (mysql_user_old != FALSE) {
    return(invisible(db_con))
  }
  carp()
  mga_mysql(user)
  lapply( dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect)
  db_con <<- dbConnect(MySQL(),
    dbname = db_name,
    host = db_host,
    port = db_port,
    user = db_user,
    password = db_password
  )
  mysql_user_old <- user
  return(invisible(db_con))
}
# source("geo/scripts/casamance.R");mysql_table_lire()
mysql_table_lire <- function(table = "casamance_mailles") {
  library(DBI)
  library(RMySQL)
  library(tidyverse)
  library(dbplyr)
  carp()
  mysql_connect(user = "casamance")
  carp("mysql_table_lire(): %s", table)
  df <- tbl(db_con, table, n = -1) %>%
    as.data.frame() %>%
    glimpse()
  return(invisible(df))
}
