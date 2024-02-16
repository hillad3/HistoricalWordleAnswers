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

ans <- fread("data/wordle_answers.csv")
ans[,Date:=lubridate::mdy(Date)]
ans <- ans[!(Date %in% Sys.Date())] # if applicable, exclude today's word to prevent spoilers

days_since_last_update <- as.integer(as.POSIXct(Sys.Date()) - as.POSIXct(max(ans$Date)))
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
  tags$head(
    # tags$link(rel = "stylesheet", type = "text/css", href = "styles.freelancer.css"),
    tags$title("Historical Wordle Answers")
  ),
  theme = main_theme,
  br(),
  tags$h1(
    tags$span("Historical", style = "color:#3BC143"),
    tags$span("Wordle", style = "color:#EDC001"),
    tags$span("Answers", style = "color:#CCCCCC"),
    style = "text-align:center; font-size:"
  ),
  br(),
  navset_tab(
    nav_panel(
      "Wordle Answers",
      modWordListUI("wordle",ans,years,dups_present,days_since_last_update,TRUE,TRUE)
    ), # close nav_panel
    nav_panel(
      "5-Letter Scrabble Words",
      br(),
      column(
        8,
        tags$span("This Scrabble word list is provided from the {scrabble} package by Julie Laffy available on "),
        tags$a(href="https://github.com/jlaffy/scrabble", "github.", style = "color:#3BC143"),
        tags$span("For those familiar with both games, clearly the criteria for a Wordle answer is not the same as a Scrabble word."),
        tags$span("Use your best judgement if you are looking for inspiration."),
        tags$span("(I make no judgements about how or if you use this list when playing Wordle. Let's all enjoy games the way we prefer to play them.)"),
      ),
      br(),
      modWordListUI("scrabble",five_letter_words,years,dups_present,days_since_last_update,FALSE,FALSE)
    ),
    nav_panel(
      "About",
      column(8,asf
        br(),
        p("Hi! Thanks for visiting!"),
        p(
          tags$span("Occasionally, in the puzzle game "),
          tags$a(href="https://www.nytimes.com/games/wordle","Wordle,", style = "color:#3BC143"),
          tags$span(" I find myself wanting to know if a word was previously used or if a Wordle answer has ever been repeated."),
          tags$span(" Ya know, fun stuff you think about while riding a rollercoaster.")
        ),
        p(),
        p("I'm not a fan of visiting ad-bloated websites to wade through multi-paragraph lead-ins (like this one?) to only have to scan an ever growing list of words. So, I decided to make a lightweight and data-focused website that can do this for me -- and now also you!"),
        p(
          tags$span("For anyone curious, this website was built using the {shiny} package in R, along with Bootstrap 5 using {bslib}."),
          tags$span("Tables were created with the {data.table} package and rendered with the {DT} package."),
          tags$span("The interactive graphs were created in {plotly} using tokenization with {tidytext}."),
          tags$span("Its code is available on my github, "),
          tags$a(href="https://github.com/hillad3/HistoricalWordleAnswers", "here.", style = "color:#3BC143")
        ),
        p("I plan to update this list every couple of weeks, assuming the hubbub of life doesn't get the best of me. If I can figure out a way to create a feedback form, I'll add one and then you can nag me to update."),
        tags$span("Happy Wordle-ing!", style="font-weight:bold; font-size:110%"),
        tags$span(" (let's pretend that's a thing normal people say)", style="font-size:80%")
      )
    ) # close nav_panel About
  ) # close navset_panel
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  modWordListServer("wordle",ans,TRUE,TRUE)
  modWordListServer("scrabble",five_letter_words,FALSE,FALSE)

}

# Run the application
shinyApp(ui = ui, server = server)
