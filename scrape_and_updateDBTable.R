rm(list = ls())
gc()

library(data.table)
library(dplyr)
library(DBI)
library(rvest)
library(lubridate)


# options to overwrite entire scrabble or wordle tables, respectively
overwrite_scrabble_dict <- FALSE
overwrite_wordle_dict<- FALSE

# function used to update wordle table in database
update_db <- function(){
  # recreate wordle table
  flsw <- tbl(con, "flsw")
  wa <- tbl(con, "wa")


  # finally merge wordle answers with scrabble answers to get one full table
  wordle <- flsw |>
    full_join(wa, by = c("Word")) |>
    arrange(Word) |>
    select(Index, Date, Word)

  dbWriteTable(con, "wordle", wordle |> collect(), overwrite=TRUE)

  print("Wordle table in db updated")
}

# make AWS database connection
con <- dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("dbname"),
  host = Sys.getenv("host"),
  port = Sys.getenv("port"),
  user = Sys.getenv("userid"),
  password = Sys.getenv("pwd")
)

if(overwrite_scrabble_dict){
  flsw <- fread("data/five_letter_scrabble_words.csv")
  flsw[,Word:=toupper(Word)]
  flsw[,Index:=NULL]
  dbWriteTable(con, "flsw", flsw, overwrite=TRUE)
}

# make a connection to the existing wordle answer table and get max index and corresponding date
db_wordle_answers <- dbReadTable(con, "wa") |> as.data.table()
setorder(db_wordle_answers, -Index)
max_index <- max(db_wordle_answers$Index)
max_date <- db_wordle_answers[Index==max_index,.(Date)] |> unlist() |> mdy()


if( (ymd(today()) > max_date) | overwrite_wordle_dict ){

  # scrape the website and determine which words are new
  u <- read_html("https://wordfinder.yourdictionary.com/wordle/answers/")
  url_wordle_answers <- u |> html_elements("table") |> html_table()
  url_wordle_answers <- bind_rows(url_wordle_answers) |> as.data.table()
  names(url_wordle_answers) <- c("Date","Index","Word")

  # remove today's word since it would require javascript to manipulate the DOM
  url_wordle_answers <- url_wordle_answers[2:dim(url_wordle_answers)[1]]
  url_wordle_answers <- merge(url_wordle_answers, db_wordle_answers[Index<1200], by = c("Index","Word"), all.x=TRUE)
  url_wordle_answers[,Date.y:=mdy(Date.y)]

  # I found this easier to do with a data.frame since data.table was converting dates to numbers
  url_wordle_answers <- url_wordle_answers |>
    as.data.frame() |>
    mutate(
      Date = if_else(is.na(Date.y),min(Date.y[!is.na(Date.y)])+Index,Date.y)
    ) |>
    as.data.table()

  url_wordle_answers[,Date.x:=NULL]
  url_wordle_answers[,Date.y:=NULL]
  # ensure wordle answers are in MM/DD/YYYY format to be compatible with shiny app
  url_wordle_answers[,`Date`:=format(`Date`, format = "%m/%d/%Y")]

  setcolorder(url_wordle_answers, neworder = c("Index", "Date", "Word"))
  setorder(url_wordle_answers, -Index)

  new_wordle_answers <- url_wordle_answers[Index > max_index]

  if(overwrite_wordle_dict){
    print("Overwriting entire wordle table in the database...")
    dbWriteTable(con, "wa", url_wordle_answers, overwrite=TRUE)
    update_db()

  } else if(dim(new_wordle_answers)[1]>0){
    print("Adding new wordle answers to table in the database...")
    dbWriteTable(con, "wa", new_wordle_answers, append = TRUE)
    update_db()

  } else {
    print("There are no new wordle answers to add to the table in the database.")
  }

} else {
  print("Wordle table is up-to-date.")
}

dbDisconnect(con)


