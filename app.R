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
library(stringr)
library(DBI)

source("moduleWordList.R")

con <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("dbname"),
  host = Sys.getenv("host"),
  port = Sys.getenv("port"),
  user = Sys.getenv("userid"),
  password = Sys.getenv("pwd")
)

sys_date <- as.Date(lubridate::with_tz(Sys.time(), "US/Eastern"), tz = "US/Eastern")

# preliminary values; may be refreshed if db is not up to date
ans <- DBI::dbReadTable(con, "wordle") |> as.data.table()
ans[,Date:=lubridate::mdy(Date)]
ans <- ans[!(Date %in% sys_date)] # if applicable, exclude today's word to prevent spoilers

DBI::dbDisconnect(con)
rm(con)

days_since_last_update <- as.integer(as.POSIXct(sys_date) - as.POSIXct(max(ans[!is.na(Date)]$Date), tz = "EST"))
years <- unique(ans[,.(lubridate::year(Date))]) |> unlist() |> sort(decreasing=TRUE) |> as.character()
dups_present <- dim(ans[duplicated(Word)])[1]>0


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
      modWordListUI("wordle",ans,sys_date,years,dups_present,days_since_last_update)
    ), # close nav_panel
    nav_panel(
      "More about using RegEx",
      column(
        width = 8,
        tags$p(),
        tags$p("The Advanced Filters accordion uses a Regular Expression ('RegEx') to pair down the word list. A RegEx is a character sequence that defines a search pattern. Although powerful, they can take a little getting used to, so below are some examples of the functionality and special characters to consider for the flavor of RegEx deployed in this website:"),
        tags$ul(
          tags$li("While Regex's are typically case sensitive, this website will convert everything to capitals before parsing."),
          tags$li("Any sequence of letters will be considered regardless of its position in a word. For example, 'EF' will match with both GRIEF and CLEFT."),
          tags$li("A wildcard can be indicated with a period '.'. For example, '...EF' will match with both GRIEF but not CLEFT because the RegEx requires at least three letters preceding the EF."),
          tags$li("The start boundary of a word can be indicated with a carrot '^'. For example, '^F' will match with FORCE but not GRIEF."),
          tags$li("The end boundary of a word can be indicated with a dollar sign '$'. For example, 'F$' will match with GRIEF but not FORCE."),
          tags$li("A logical OR relationship can be indicated with a pipe '|'. For example, '^F|F$' will match with both FORCE and GRIEF because the F can be at the start or end of the word."),
          tags$li("A 'one of' relationship can be indicated within square brackets '[]'. For example, '^[ABC]' will match any word starting with A, B or C."),
          tags$li("A 'one or more' relationship can be indicated with a '+'. For example, 'BE+' will match both MAYBE (with one E) and BEEFY (with two E's)."),
          tags$li("A 'zero or one' relationship can be indicated with a '?'. For example, 'AC?T' will make the C optional, so it will match both ACTOR and SPLAT."),
          tags$li("A 'zero or more' relationship can be indicated with a '*'. For example, 'UE*' will make the E optional (but could be multiples) and match ACUTE, ARGUE, FULLY and QUEEN."),
          tags$li("A quantifer can be indicated with curly brackets '{}'. For example, 'E{2}' will BEEFY and STEEL."),
          tags$li("A range of letters can be indicated with a dash inside square brackets. For example, '^[Q-S]' would match any word starting with a letter between Q and S, inclusive."),
          tags$li("A RegEx can be grouped using parentheses '()'. For example, '(ABO..)|(BOA)|(AO)' would return a list of 5-letter words that includes CACAO, BOAST, and AORTA.")
        ),
        tags$p(
          tags$span("There are even more powerful features of regex but they aren't really useful when dealing with a list of single words that are exactly 5 characters long. You can find out more about RegEx rules that I based this on "),
          tags$a(href="https://rstudio.github.io/cheatsheets/html/strings.html#match-characters", "here", style = "color:#3BC143", .noWS="after"),
          tags$span(".")
        ),
        tags$h3("Practical Applications"),
        tags$ul(
          tags$li("Since Wordle answers are only 5 letters, you will usually be able to search simply using letters and the wildcard period '.'. Repeating the wildcard character five times '.....' will match with any word, 'F....' will match any word starting with F, '....F' will find any word ending in F, '.FF..' will find any double F word in position 2 and 3, and 'FL..F' will only find FLUFF because there are no other words matching that pattern."),
          tags$li("If you know that the first two letters of an answer is ST and you know that the third position is not a K, then you could set the RegEx to '^ST[A-J|L-Z]'."),
          tags$li("If you know that the first two letters of an answer is ST and you know that the third position is not a K and you'd like to indicate that the K is in the fourth or fifth position, then you could set the RegEx to '(^ST[A-J|L-Z]K.)|(^ST[A-J|L-Z].K)'. Note: It is important to set up the two Regex in two sets of parentheses. If the RegEx was written '^ST[A-J|L-Z](.K)|(K.)' it would get parsed as '^ST[A-J|L-Z].K' or 'K.', and since position doesn't matter with this flavor of RegEx the 'K.' would return any word with a K.")
        ),
        tags$p()
      )
    ),
    nav_panel(
      "About",
      column(8,
        br(),
        p(
          tags$span("Occasionally, in the puzzle game "),
          tags$a(href="https://www.nytimes.com/games/wordle","Wordle", style = "color:#3BC143", .noWS="after"),
          tags$span(", I find myself wanting to know if a word was previously used or if a Wordle
                    answer has ever been repeated.")
        ),
        p(
          tags$span("I'm not a fan of visiting ad-bloated websites to mouse through an ever growing list of words "),
          tags$span(" (but when I do, I visit "),
          tags$a(href="https://wordfinder.yourdictionary.com/wordle/answers/", "this website", style = "color:#3BC143", .noWS="after"),
          tags$span(", which I am also using to scrape the Wordle answers on this website)."),
          tags$span("So, I decided to make a lightweight and data-focused website that can help me in this endeavour -- and now also you!")
        ),
        p(
          tags$span("This Scrabble word list is provided from the {scrabble} package by Julie Laffy
                    available on "),
          tags$a(href="https://github.com/jlaffy/scrabble", "github", style = "color:#3BC143", .noWS="after"),
          tags$span(". For those familiar with both games, clearly the criteria for a Wordle answer is
                    not the same as a Scrabble word."),
          tags$span("Use your best judgement if you are looking for inspiration."),
          tags$span("(I make no judgements about how or if you use this list when playing Wordle.
                    Let's all enjoy games the way we prefer to play them.)"),
        ),
        p(
          tags$span("This website was built using the {shiny} package in R Studio,
                    along with Bootstrap 5 using {bslib}."),
          tags$span("Data is scraped using {rvest}. (This scraping is triggered manually, so "),
          tags$span("feel free to text me or bug me on my Github if this list is out of date or something looks broken)."),
          tags$span("The Wordle answer list is served by a free-tier AWS PostgreSQL database, using the {DBI} package."),
          tags$span("Tables were created with the {data.table} package and rendered with the {DT} package."),
          tags$span("The interactive graph was created in {plotly} using tokenization with {tidytext}."),
          tags$span("Its code is available on my github, "),
          tags$a(href="https://github.com/hillad3/HistoricalWordleAnswers", "here", style = "color:#3BC143", .noWS="after"),
          tags$span(".")
        ),
        tags$span("Happy Wordle-ing!", style="font-weight:bold; font-size:110%"),
        p("", style="margin-bottom:25px")
      )
    ) # close nav_panel About
  ) # close navset_panel
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  modWordListServer("wordle",ans,sys_date)

}

# Run the application
shinyApp(ui = ui, server = server)
