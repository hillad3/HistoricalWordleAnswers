rm(list = ls())
gc()

library(data.table)
library(dplyr)
library(DBI)

local_ans <- fread("data/wordle_answers.csv")

five_letter_words <- fread("data/five_letter_scrabble_words.csv")
five_letter_words[,Word:=toupper(Word)]

local_ans <- local_ans[five_letter_words[,Index:=NULL], on = 'Word']

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

overwrite_table <- FALSE

if(dim(new_ans)[1]==0){
  # do nothing and close connection
  dbDisconnect(con)
} else if (overwrite_table) {
  # overwrite table

  dbWriteTable(con, "wordle", local_ans,  overwrite=TRUE)
  dbDisconnect(con)

} else {

  dbWriteTable(con, "wordle", new_ans, append=TRUE)
  dbDisconnect(con)
}
