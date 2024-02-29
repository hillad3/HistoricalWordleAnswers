rm(list = ls())
gc()

library(data.table)
library(dplyr)
library(DBI)

local_ans <- fread("data/wordle_answers.csv")

con <- dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("dbname"),
  host = Sys.getenv("host"),
  port = Sys.getenv("port"),
  user = Sys.getenv("userid"),
  password = Sys.getenv("pwd")
)
db_ans <- dbReadTable(con, "wordle") |> as.data.table()

new_ans <- local_ans[!(Word %in% db_ans$Word)]

if(dim(new_ans)[1]==0){
  # do nothing and close connection
  dbDisconnect(con)
} else {
  # create temp table and then insert into

  dbWriteTable(con, "wordle", new_ans, append=TRUE)
  dbDisconnect(con)

}
