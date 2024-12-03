require(data.table)
require(dplyr)
require(DBI)
require(rvest)
require(lubridate)

con <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("dbname"),
  host = Sys.getenv("host"),
  port = Sys.getenv("port"),
  user = Sys.getenv("userid"),
  password = Sys.getenv("pwd")
)

# function used to update wordle table in database
update_db <- function(){
  # recreate wordle table
  five_letter_scrabble_words <- tbl(con, "five_letter_scrabble_words")
  wordle_answers <- tbl(con, "wordle_answers")

  # finally merge wordle answers with scrabble answers to get one full table
  website_word_list <- five_letter_scrabble_words |>
    full_join(wordle_answers, by = c("word")) |>
    arrange(word) |>
    select(index, date, word)

  dbWriteTable(con, "website_word_list", website_word_list |> collect(), overwrite=TRUE)

  print("Wordle table in db updated")
}

# make a connection to the existing wordle answer table and get max index and corresponding date
db_wordle_answers <- dbReadTable(con, "wordle_answers") |> as.data.table()
setorder(db_wordle_answers, -index)
max_index <- max(db_wordle_answers$index)
max_date <- db_wordle_answers[index==max_index][,date] |> unlist()


if( (ymd(today())-1 > max_date)){

  # scrape the website and determine which words are new
  u <- read_html("https://wordfinder.yourdictionary.com/wordle/answers/")
  url_wordle_answers <- u |> html_elements("table") |> html_table()
  url_wordle_answers <- bind_rows(url_wordle_answers) |> as.data.table()
  names(url_wordle_answers) <- c("date","index","word")

  # remove today's word since it would require javascript to manipulate the DOM
  url_wordle_answers <- url_wordle_answers[2:dim(url_wordle_answers)[1]]
  url_wordle_answers <- merge(url_wordle_answers, db_wordle_answers, by = c("index","word"), all.x=TRUE)
  # url_wordle_answers[,date.y:=mdy(date.y)]

  # I found this easier to do with a data.frame since data.table was converting dates to numbers
  url_wordle_answers <- url_wordle_answers |>
    as.data.frame() |>
    mutate(
      date = if_else(is.na(date.y),min(date.y[!is.na(date.y)])+index,date.y)
    ) |>
    as.data.table()

  url_wordle_answers[,date.x:=NULL]
  url_wordle_answers[,date.y:=NULL]
  # ensure wordle answers are in MM/DD/YYYY format to be compatible with shiny app
  # url_wordle_answers[,`date`:=format(`date`, format = "%m/%d/%Y")]

  setcolorder(url_wordle_answers, neworder = c("index", "date", "word"))
  setorder(url_wordle_answers, -index)

  new_wordle_answers <- url_wordle_answers[index > max_index]

  if(dim(new_wordle_answers)[1]>0){
    print("Adding new wordle answers to table in the database...")
    dbWriteTable(con, "wordle_answers", new_wordle_answers, append = TRUE)
    update_db()

  } else {
    print("There are no new wordle answers to add to the table in the database.")
  }

} else {
  print("The Wordle list is already up to date")
}

DBI::dbDisconnect(con)
rm(con)
