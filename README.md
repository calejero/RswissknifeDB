
<!-- badges: start -->

[![codecov](https://codecov.io/gh/calejero/RswissknifeDB/branch/master/graph/badge.svg?token=oaw2jeS4I3)](https://codecov.io/gh/calejero/RswissknifeDB)
[![CodeFactor](https://www.codefactor.io/repository/github/calejero/rswissknifedb/badge?s=0bd4ffc58a552d40d86144c31b9f3ce03b48320f)](https://www.codefactor.io/repository/github/calejero/rswissknifedb)
![GitHub language count](https://img.shields.io/github/languages/count/calejero/rswissknifedb?style=flat-square)
![GitHub All Releases](https://img.shields.io/github/downloads/calejero/rswissknifedb/total)
![GitHub commit activity](https://img.shields.io/github/commit-activity/w/calejero/rswissknifedb)
![GitHub tag (latest by date)](https://img.shields.io/github/v/tag/calejero/rswissknifedb?style=flat-square)

<!-- badges: end -->

# RswissknifeDB
This package allows you to work with some of common databases like MYSQL or PostgreSQL. Tested databases in this version:
  - MySQL

# Installation

  - Packages dependencies:
      - [RMariaDB](https://rmariadb.r-dbi.org)
      - [DBI](https://github.com/r-dbi/DBI)
      - [glue](https://github.com/tidyverse/glue) 
      - [rjson](https://github.com/cran/rjson) 
 
``` r
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("calejero/RswissknifeDB")
```

## Usage
``` r
library(RswissknifeDB)
BuildDBEnvirontment("mysql", pool.flag = FALSE)
Hello localhost - mysql 
Now, localhost the connection is available
Active conections availables:
con_localhost_master 
SelectDB("SELECT * FROM Persons", database = "test")
  PersonID     LastName FirstName     Address     City
1        2      Alberto     Gómez Dirección 2 Zaragoza
2        1 Jesús Armand  Calejero Dirección 1 Zaragoza
CloseConnectionDB()
```

### Read Functions


| Function | Params | Example |
| ------ | ------ | ------ |
| `SelectDB` | query, database | `SelectDB("SELECT * FROM Persons", "test")`
| `SelectByParamsDB` | query, params, database | `SelectByParamsDB("SELECT * FROM Persons WHERE LastName = '@param1'", 'Jesús Armand', database = "test")`

### Write Functions


| Function | Params | Example |
| ------ | ------ | ------ |
| `DeleteDB` | table, db.name | `DeleteDB("Persons", db.name = "test")`
| `DeleteByParamsDB` | table, colnames, params, db.name | `DeleteByParamsDB("Persons", "LastName", 'Jesús Armand', db.name = "test")`
| `DeleteByQueryDB` | query, db.name | `DeleteByQueryDB("DELETE * FROM Persons WHERE City = 'Zaragoza'", db.name = "test")`
| `TruncateTableDB` | table, db.name | `TruncateTableDB("Persons", db.name = "test")`
| `InsertByQueryDB` | query, db.name | `InsertByQueryDB("INSERT INTO Persons VALUES (2, 'Alberto', 'Gómez', 'Dirección 2', 'Zaragoza')", "test")`
| `InsertDB` | data, db.table, db.name | data.df = data.frame(PersonID = 4, LastName = 'Núñez', FirstName = 'Josep Lluis', Address = 'Dirección 4', City = 'Barcelona', stringsAsFactors = FALSE) `InsertDB(data.df, "Persons", "test")`
| `InsertByLoadDataDB` | data.df, db.table, db.name, cushion = 100000, is.pool = FALSE | `InsertByLoadDataDB(data.df, "Persons", "test")`
| `CreateTempTableDB` | tablename, db.name, fields.df | `CreateTempTableDB("Customers", db.name = "test", fields.df = campos.df)`
| `CreateTableByQueryDB` | tablename, db.name, query | `CreateTableByQueryDB("Customers", db.name = "test", query = new.table.c)`


### Important
##### V1.0.

1) Pool environment are available but not testing
2) First step is generate a file like `data/config_db.csv` with your database credentials.
