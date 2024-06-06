rm(list = ls())
gc()

library(data.table)
library(dplyr)
library(DBI)

con <- dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("dbname"),
  host = Sys.getenv("host"),
  port = Sys.getenv("port"),
  user = Sys.getenv("userid"),
  password = Sys.getenv("pwd")
)

overwrite_flsw <- FALSE
if(overwrite_flsw){
  flsw <- fread("data/five_letter_scrabble_words.csv")
  flsw[,Word:=toupper(Word)]
  flsw[,Index:=NULL]
  dbWriteTable(con, "flsw", flsw, overwrite=TRUE)
}

wa <- fread("data/wordle_answers.csv")
overwrite_wa <- FALSE

if(overwrite_wa){
  dbWriteTable(con, "wa", wa, overwrite=TRUE)
} else {

  db_wa <- dbReadTable(con, "wa")
  new_wa <- anti_join(wa, db_wa, by = c("Word"))

  if(dim(new_wa)[1]>0){
    dbWriteTable(con, "wa", new_wa, append = TRUE)
  }

}

# recreate wordle table
flsw <- tbl(con, "flsw")
wa <- tbl(con, "wa")

wordle <- flsw |>
  full_join(wa, by = c("Word")) |>
  arrange(Word) |>
  select(Index, Date, Word)

dbWriteTable(con, "wordle", wordle |> collect(), overwrite=TRUE)

dbDisconnect(con)

