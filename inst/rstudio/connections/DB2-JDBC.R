library(db2connect)
conn <- db2_connect(
  db_name = ${1:Database name=NULL},
  username = ${2:User=NULL}, 
  password = ${3:Password=NULL},
  server_address = ${4:Server address=NULL},
  driver_path = ${5:Driver path=NULL}
)
