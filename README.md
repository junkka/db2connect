DB2 connect
-----

Connects to IBM DB2 databases, using the [RJDBC](https://www.rforge.net/RJDBC/) package. Integrates with RStudio database connection pane, see: [RStudio Connections Pane](https://db.rstudio.com/rstudio/connections/)


## Requirements

* Java
* DB2 JDBC Driver see: http://www-01.ibm.com/support/docview.wss?uid=swg21363866

## Usage

Setting up a connection.

```r
library(db2connect)
conn <- db2_connect(
  db_name  = "DATABASENAME",
  username = "username",
  server_address = "server-adress:50000",
  driver_path = "/path/to/db2jcc4.jar"
)
```


Get first 10 rows from table test_table in schema test_schema.

```r
db2_query(conn, "SELECT * FROM test_schema.test_table LIMIT 10")
```

As [R Markdown supports SQL chunks](https://bookdown.org/yihui/rmarkdown/language-engines.html), this also works

    ```{sql, connection = conn}
    SELECT * 
      FROM test_schema.test_table 
      LIMIT 10
    ```


List schemas in database.

```r
db2_list_objects(conn)
```

List tables in a specific schema.

```r
db2_list_objects(conn, schema = "test_schema")
```

List column and column types in a table.

```r
db2_list_columns(conn, schema = "test_schema", table = "test_table")
```

Close the connection.

```r
db2_close(conn)
```


For development of RStudio integration see: [Connections Contract](https://rstudio.github.io/rstudio-extensions/connections-contract.html)


## Customization

The `db2_connect` function can be further customized, accepting the following arguments:

* **db_name** the name of the database
* **username** name of user
* **password** user password
* **server_address** address to server, including port, in the format `<address>:<port>`
* **driver_path** path to your local copy of the DB2 JDBC driver `db2jcc4.jar`.

All these arguments can be set beforehand using `options`

    options("db2.username" = "username")
    options("db2.db_name" = "database")
    options("db2.password" = "secret-password")
    options("db2.server_address" = "serveradress:port")
    options("db2.driver_path" = "path/to/driver")


The password argument also accept quoted functions, thus, one can pass in passwords from a [keyring](https://db.rstudio.com/best-practices/managing-credentials/). For example:

```r
options("db2.password" = quote(keyring::key_get("service", "username")))
```




