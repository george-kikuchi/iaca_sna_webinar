
######
# configuration file
######

library(DBI)
library(odbc)
library(RODBC)

#### 1) database connection string

### sample MS access; comment these lines out with # or delete, 
### when using your own agency data
 con <- DBI::dbConnect(odbc::odbc(), 
                       .connection_string=paste0(
                           "Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                           DBQ=", file.path(getwd(), "sample.accdb")))

### MS SQL sample connection; 
### take out # to uncomment these lines and edit server and db information

# con <- DBI::dbConnect(odbc::odbc(), 
#                      Driver = "SQL Server", 
#                      Server = "xx.xxx.xxx.xxxx", 
#                      Database = "database name", 
#                      Trusted_Connection = "True")


#### 2) table name ("edit the name within the quotes to match your data)
#### this table (or view) needs to have incident_id, offender_id, and name columns
table <- "arrest"


#### 3) column names (edit the names within the quotes to match your data)

inct_id   <- "inct_id"
person_id <- "person_id"
name      <- "name"

