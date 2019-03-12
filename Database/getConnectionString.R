library(RODBC)


connectionString <- function( Server, Database, user = "ro", password = "read"){
	cs <- odbcDriverConnect( sprintf("driver={SQL Server};server=%s; database=%s;Uid=%s; Pwd=%s; trusted_connection=true", Server, Database, user, password))
	cs
}