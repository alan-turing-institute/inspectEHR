library(DBI)
library(Eunomia)

exploreCCHIC = function() {
  con <- dbConnect(drv=RSQLite::SQLite(), dbname="./data/CCHIC/public_release_20190403.sqlite3")
  
  ## list all tables
  tables <- dbListTables(con)
  
  ## exclude sqlite_sequence (contains table information)
  tables <- tables[tables != "sqlite_sequence"]
  
  events = tbl(con, 'events')
  print(events)
  
  res <- dbSendQuery(con, "SELECT code_name FROM events limit 10")
  dbFetch(res)
}

exploreEunomia = function() {
  connectionDetails <- getEunomiaConnectionDetails()
  connection <- connect(connectionDetails)
  tables = getTableNames(connection,databaseSchema = 'main')
  #print(tables)
  #statement = "SELECT COUNT(*) FROM Vocabulary limit 10;"
  statement = "SELECT * FROM Vocabulary where vocabulary_name LIKE '%snomed%';"
  #statement = "SELECT * FROM Vocabulary Limit 1"
  #statement = "SELECT * FROM Observation where Observation_source_concept_id=4316224;"
  person = querySql(connection, "SELECT * FROM Person where person_id = 16")
  observation_period = querySql(connection, "SELECT * FROM OBSERVATION_PERIOD where PERSON_ID=16")
  visit_occurence = querySql(connection, "SELECT * FROM VISIT_OCCURRENCE where PERSON_ID=16")
  #result = querySql(connection, statement)
  print(person)
  print(observation_period)
  print(visit_occurence)
  disconnect(connection)
}

exploreEunomia()