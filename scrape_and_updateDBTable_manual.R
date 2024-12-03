rm(list=ls())
gc()
library(data.table)
library(dplyr)
library(DBI)
library(rvest)
library(lubridate)
library(glue)
source("scrape_and_updateDBTable_helper.R")

clean_url_words <- function(df){
  df <- df[,date:=stringr::str_remove(date,"Today")]
  df <- df[,date:=stringr::str_remove_all(date,"\\s")]
  df <- df[,word:=stringr::str_remove(word,"Reveal")]
  df <- df[,word:=stringr::str_remove_all(word,"\\s")]
  return (df)
}

# function to update wordle table in database
update_db <- function(df){

  new_words <- df$word |> as_vector()

  # remove the old words from the website_word_list
  for (w in new_words){
    dbExecute(con, glue_sql('DELETE FROM website_word_list WHERE word = {w}',.con=con))
  }

  # add the new words that now have wordle index and dates associated with them
  dbWriteTable(con, "website_word_list", df, append=TRUE)

  print("Website word list table in db updated")
}

con <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("dbname"),
  host = Sys.getenv("host"),
  port = Sys.getenv("port"),
  user = Sys.getenv("userid"),
  password = Sys.getenv("pwd")
)

max_web_entry <- tbl(con, "wordle_answers") |>
  mutate(max_index = max(index)) |>
  filter(max_index == index) |>
  select(-max_index)

max_web_date <- max_web_entry |>
  mutate(date = as.character(date)) |>
  select(date) |>
  collect() |>
  unlist() |>
  unname() |>
  ymd()

max_web_index <- max_web_entry |>
  select(index) |>
  collect() |>
  unlist() |>
  unname()

# this aligns with the app, so that these should be similar dates and times
sys_date <- as.Date(lubridate::with_tz(Sys.time(), "US/Eastern"), tz = "US/Eastern")

# subtract 1 day since generally I don't want to include the current day's word and
# it is challenge to ensure that word isn't included with timezone differences
if( (sys_date-1 > max_web_date)){

  # scrape the website and reshape into a data.table
  url_wordle_answers <- read_html("https://wordfinder.yourdictionary.com/wordle/answers/") |>
    html_elements("table") |>
    html_table() |>
    bind_rows() |>
    as.data.table()

  # confirm that the column names match expected values and there are no NAs.
  # the tries to future proof if the HTML layout changes before a broken script sends
  # data to the database
  tryCatch(
    expr = if (all(names(url_wordle_answers) == c("Date", "Wordle #", "Answer"))){

      # apply standardized names
      names(url_wordle_answers) <- c("date","index","word")

      # clean up today's word since it has some extra text within the html
      url_wordle_answers <- clean_url_words(url_wordle_answers)

      max_url_index <- url_wordle_answers[index == max(index)][ ,index]

      if (all(lapply(url_wordle_answers$word, function(x) stringr::str_length(x) == 5) |> unlist())){

        # add the index to the first date ever. this assumes that there are no skipped days
        new_wordle_answers <- (copy(url_wordle_answers)
          [index > max_web_index]
          [, date:=ymd("2021-06-19")+index])

      } else {
        # this probably indicates an issue with cleaning the url_wordle_answers
        message("The column names matched expected values, but not all words have a length of 5 characters.")
      }

    } else {
      message("The column names do not match the expected values. No further action taken.")
    },
    error = function(e){
      message("An error occurred with assigning the column values. No further action taken.")
    }
  )

  if(dim(new_wordle_answers)[1]>0){
    print("Adding new wordle answers to table in the database...")
    dbWriteTable(con, "wordle_answers", new_wordle_answers, append = TRUE)
    update_db(new_wordle_answers)

  } else {
    print("There are no new wordle answers to add to the table in the database.")
  }

} else {
  print("The Wordle list is already up to date")
}

DBI::dbDisconnect(con)
rm(con)
