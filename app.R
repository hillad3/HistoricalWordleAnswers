rm(list = ls())
gc()

library(data.table)
library(DT)
library(shiny)
library(shinyWidgets)
library(bslib)
library(tidytext)
library(dplyr)
library(plotly)

source("moduleWordList.R")

tryCatch(
  expr = {
    con <- DBI::dbConnect(
      drv = RPostgres::Postgres(),
      dbname = Sys.getenv("dbname"),
      host = Sys.getenv("host"),
      port = Sys.getenv("port"),
      user = Sys.getenv("userid"),
      password = Sys.getenv("pwd")
    )

    ans <- DBI::dbReadTable(con, "wordle") |> as.data.table()

    DBI::dbDisconnect(con)
    rm(con)
  },
  error = function(e){
    ans <- fread("data/wordle_answers.csv")
  },
  warning = function(w){
    ans <- fread("data/wordle_answers.csv")
  }
)



sys_date <- as.Date(lubridate::with_tz(Sys.time(), "US/Eastern"), tz = "US/Eastern")

ans[,Date:=lubridate::mdy(Date)]
ans <- ans[!(Date %in% sys_date)] # if applicable, exclude today's word to prevent spoilers


days_since_last_update <- as.integer(as.POSIXct(sys_date) - as.POSIXct(max(ans$Date), tz = "EST"))
years <- unique(ans[,.(lubridate::year(Date))]) |> unlist() |> sort(decreasing=TRUE) |> as.character()
dups_present <- dim(ans[duplicated(Word)])[1]>0


five_letter_words <- fread("data/five_letter_scrabble_words.csv")
five_letter_words[,Word:=toupper(Word)]
five_letter_words[,`Past Wordle Answer`:=Word %in% ans$Word]

main_theme <- bs_theme(
  version = 5,
  bg = "#222222",
  fg = "#FFFFFF",
  base_font = "Helvetica"
)

ui <- page_fluid(
  theme = main_theme,
  tags$head(
    tags$title("Historical Wordle Answers")
  ),
  fluidRow( # essentially a navbar container
    column(
      10,
      br(),
      tags$h1(
        tags$span("Historical", style = "color:#3BC143"),
        tags$span("Wordle", style = "color:#EDC001"),
        tags$span("Answers", style = "color:#CCCCCC"),
        style = "text-align:center;"
      )
    ),
    column(
      2,
      navset_bar(
        nav_item(
          tags$a(
            shiny::icon("github"),
            "GitHub Repository",
            href = "https://github.com/hillad3/HistoricalWordleAnswers",
            target = "_blank"
          )
        )
      )
    )
  ),
  navset_underline(
    nav_panel(
      "Wordle Answers",
      modWordListUI("wordle",ans,sys_date,years,dups_present,days_since_last_update,TRUE,TRUE)
    ), # close nav_panel
    nav_panel(
      "5-Letter Scrabble Words",
      br(),
      column(
        8,
        tags$span("This Scrabble word list is provided from the {scrabble} package by Julie Laffy
                  available on "),
        tags$a(href="https://github.com/jlaffy/scrabble", "github", style = "color:#3BC143", .noWS="after"),
        tags$span(". For those familiar with both games, clearly the criteria for a Wordle answer is
                  not the same as a Scrabble word."),
        tags$span("Use your best judgement if you are looking for inspiration."),
        tags$span("(I make no judgements about how or if you use this list when playing Wordle.
                  Let's all enjoy games the way we prefer to play them.)"),
      ),
      br(),
      modWordListUI("scrabble",five_letter_words,years,dups_present,days_since_last_update,FALSE,FALSE)
    ),
    nav_panel(
      "About",
      column(8,
        br(),
        p("Hi! Thanks for visiting!"),
        p(
          tags$span("Occasionally, in the puzzle game "),
          tags$a(href="https://www.nytimes.com/games/wordle","Wordle", style = "color:#3BC143", .noWS="after"),
          tags$span(", I find myself wanting to know if a word was previously used or if a Wordle
                    answer has ever been repeated."),
          tags$span(" Ya know, fun stuff you think about while riding a rollercoaster.")
        ),
        p(),
        p("I'm not a fan of visiting ad-bloated websites to wade through multi-paragraph lead-ins
          (like this one?) only to have to scan an ever growing list of words. So, I decided to make
          a lightweight and data-focused website that can help me in this endeavour -- and now also you!"),
        p(
          tags$span("For the R enthusiasts, this website was built using the {shiny} package in R Studio,
                    along with Bootstrap 5 using {bslib}."),
          tags$span("Tables were created with the {data.table} package and rendered with the {DT} package."),
          tags$span("The interactive graphs were created in {plotly} using tokenization with {tidytext}."),
          tags$span("Its code is available on my github, "),
          tags$a(href="https://github.com/hillad3/HistoricalWordleAnswers", "here", style = "color:#3BC143", .noWS="after"),
          tags$span(".")
        ),
        p("I plan to update this list every couple of weeks, assuming the hubbub of life doesn't
          get the best of me. If I can figure out a way to create a feedback form, I'll add one and
          then you can nag me to update."),
        tags$span("Happy Wordle-ing!", style="font-weight:bold; font-size:110%"),
        tags$span(" (let's pretend that's a thing normal people say)", style="font-size:80%"),
        p("", style="margin-bottom:25px")
      )
    ) # close nav_panel About
  ) # close navset_panel
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  modWordListServer("wordle",ans,sys_date,TRUE,TRUE)
  modWordListServer("scrabble",five_letter_words,sys_date,FALSE,FALSE)

}

# Run the application
shinyApp(ui = ui, server = server)
