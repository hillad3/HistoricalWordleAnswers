library(data.table)
library(dplyr)
library(DBI)
library(rvest)
library(lubridate)
library(stringr)
library(glue)
library(purrr)

close_timer <- function(delay_ = 3) {
  for (i in delay_:1) {
    message(glue("Closing R in... {i}"))
    Sys.sleep(1)
  }
}

connect_db <- function() {
  dbConnect(
    drv = RPostgres::Postgres(),
    dbname   = Sys.getenv("supabaseDbName"),
    host     = Sys.getenv("supabaseHost"),
    port     = Sys.getenv("supabasePort"),
    user     = Sys.getenv("supabaseUser"),
    password = Sys.getenv("supabasePW")
  )
}

get_db_last_answer <- function(con) {
  tbl(con, "wordle_answers") |>
    slice_max(index, n = 1) |>
    collect() |>
    as.data.table()
}

scrape_html_table <- function() {

  df <- read_html("https://wordfinder.yourdictionary.com/wordle/answers/") |>
    html_elements("table") |>
    html_table() |>
    bind_rows() |>
    as.data.table()

  return(df)
}

safe_scrape_html_table <- possibly(
  scrape_html_table,
  otherwise = NULL,
  quiet = FALSE
)


clean_html_table <- function(df) {

  expected <- c("Date", "Wordle #", "Answer")
  if (!identical(names(df), expected)) {
    stop("Unexpected column names in scraped Wordle table.")
  }

  setnames(df, c("date", "index", "word"))

  df[, word := str_remove_all(word, "Reveal|\\s")]
  df[, date := ymd("2021-06-19") + index]

  if (any(str_length(df$word) != 5)) {
    stop("Invalid Wordle answer length detected.")
  }

  df[]
}

trim_html_table <- function(df, last_index, sys_date) {
  df <- df[index > last_index]

  # the source URL will potentially have answers that are one day ahead of Eastern timezones
  # so remove those to avoid complicated timezone updates
  df <- df[date <= sys_date]
}

update_db_wordle <- function() {

  con <- connect_db()
  on.exit(dbDisconnect(con), add = TRUE)

  sys_date <- as_date(with_tz(Sys.time(), "US/Eastern"))

  last_answer <- get_db_last_answer(con)
  last_index  <- last_answer$index
  last_date <- last_answer$date

  raw_html <- safe_scrape_html_table()

  if (is.null(raw_html)) {
    message("Scraping failed — website unreachable or malformed. No updates performed.")
    close_timer()
    return(invisible())
  }

  cleaned  <- clean_html_table(raw_html)
  new_rows <- trim_html_table(cleaned, last_index, sys_date)

  if (sys_date <= last_date) {
    message("Database should already be up to date. No scraping performed.")
    close_timer()
    return(invisible())
  }

  if (nrow(new_rows) == 0) {
    message("No new Wordle answers to add.")
    close_timer()
    return(invisible())
  }

  message("Adding new Wordle answers to database...")
  dbWriteTable(con, "wordle_answers", new_rows, append = TRUE, row.names = FALSE)

  close_timer()
  invisible()
}

# the website wordle list is a combinatino of the wordle_answers and five_letter_scrabble_words
# sql table. This combines the two, keeping all unique/repeated wordle answers
rebuild_website_word_list <- function() {

  con <- connect_db()
  on.exit(dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "DROP TABLE IF EXISTS website_word_list;")

  sql <- "
    CREATE TABLE website_word_list AS
    WITH wordle AS (
        SELECT
            word,
            date,
            index
        FROM wordle_answers
    ),
    scrabble AS (
        SELECT
            word
        FROM five_letter_scrabble_words
    ),
    combined AS (
        -- keep all Wordle rows (including repeats)
        SELECT word, date, index
        FROM wordle

        UNION ALL

        -- add Scrabble words with NULL date/index
        SELECT word, NULL::date AS date, NULL::integer AS index
        FROM scrabble
    ),
    counts AS (
        SELECT
            word,
            COUNT(*) FILTER (WHERE date IS NOT NULL) AS count
        FROM combined
        GROUP BY word
    )
    SELECT
        c.word,
        c.date,
        c.index,
        cnt.count
    FROM combined c
    JOIN counts cnt USING (word)
    ORDER BY word, date NULLS LAST;
  "

  DBI::dbExecute(con, sql)
}
