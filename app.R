rm(list = ls())
gc()

library(data.table)
library(DT)
library(shiny)
library(shinyWidgets)
library(tidytext)
library(dplyr)
library(plotly)

ans <- fread("wordle_answers.csv")
ans[,Date:=lubridate::mdy(Date)]
days_since_last_update <- as.integer(as.POSIXct(Sys.Date()) - as.POSIXct(max(ans$Date)))
years <- unique(ans[,.(lubridate::year(Date))]) |> unlist() |> sort(decreasing=TRUE) |> as.character()
dups_present <- dim(ans[duplicated(Answer)])[1]>0


ui <- fluidPage(
  tags$head(
    # tags$link(rel = "stylesheet", type = "text/css", href = "styles.freelancer.css"),
    tags$title("Historical Wordle Answers")
  ),
  tags$h1("Historical Wordle Answers", style = "text-align:center"),
  br(),
  tabsetPanel(
    tabPanel(
      "Searchable Answers",
      sidebarLayout(
        sidebarPanel(
          p("Positional Filters", style = "font-weight:bold; font-size:110%; text-align:center"),
          selectInput(
            inputId = "first_letter",
            label = "1st Letter:",
            choices = c("", LETTERS),
            selected = "",
            multiple = FALSE
          ),
          selectInput(
            inputId = "second_letter",
            label = "2nd Letter:",
            choices = c("", LETTERS),
            selected = "",
            multiple = FALSE
          ),
          selectInput(
            inputId = "third_letter",
            label = "3rd Letter:",
            choices = c("", LETTERS),
            selected = "",
            multiple = FALSE
          ),
          selectInput(
            inputId = "fourth_letter",
            label = "4th Letter:",
            choices = c("", LETTERS),
            selected = "",
            multiple = FALSE
          ),
          selectInput(
            inputId = "fifth_letter",
            label = "5th Letter:",
            choices = c("", LETTERS),
            selected = "",
            multiple = FALSE
          ),
          br(),
          p("Date Filters", style = "font-weight:bold; font-size:110%; text-align:center"),
          dateRangeInput(
            inputId = "date_range",
            label = "Date Range",
            start = "2021-06-19",
            end = Sys.Date()
          ),
          selectInput(
            inputId = "year_filter",
            label = "Year",
            choices = c("", years),
            selected = "",
            multiple = FALSE
          ),
          br(),
          shinyWidgets::materialSwitch(
            inputId = "check_dups",
            label = "Check for Repeats",
            value = FALSE,
          ),
          if(!dups_present){tags$p(paste0("No repeats as of ",Sys.Date()), style = "font-size:90%")},
          br(),
          br(),
          actionButton(
            inputId = "reset_filter",
            label = "Reset Filters",
            style = "background:#f28482; color:white; font-weight:bold"
          ),
          width = 3,
          style = "font-color:white"
        ), # close sidebarPanel
        mainPanel(
          br(),
          tags$p(paste0("The answer list is updated periodically. Last update: ",max(ans$Date)," (",days_since_last_update,ifelse(days_since_last_update<=1," day ago)."," days ago)."))),
          br(),
          column(
            8,
            tags$h2("Answer List"),
            fluidRow(DTOutput("answer_table")),
            br(),
            tags$h2("Letter Frequency"),
            fluidRow(plotlyOutput("letter_freq"))
          )
        ) # close mainPanel
      ) # close sidebarLayout
    ), # close tabPanel for Historicals
    tabPanel(
      "About",
      column(5,
        br(),
        p("Hi! Thanks for visiting!"),
        tags$span("Occasionally, in the game "),
        tags$a(href="https://www.nytimes.com/games/wordle","Wordle,"),
        tags$span(" I find myself wanting to know if a word was previously used or if a Wordle answer has ever been repeated. Ya know, fun stuff you think of while riding a rollercoaster."),
        p(),
        p("I find it de-motivating to visit ad-bloated websites to wade through multi-paragraph lead-ins to only have to scan an ever growing list of words. So, I decided to make a lightweight and data-focused website instead that can do this for me -- and now also you!"),
        p("I plan to update this list every couple of weeks, assuming the hubbub of life doesn't get the best of me. If I can figure out a way to create a feedback form, I'll add one and then you can nag me to update."),
        tags$span("Happy Wordle-ing!", style="font-weight:bold; font-size:110%"),
        tags$span(" (let's pretend that's a thing normal people say)", style="font-size:80%")
      )
    ) # close tabPanel for About
  ) # close tabsetPanel
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  dt <- reactive({

    tmp <- "....."

    if(input$first_letter %in% LETTERS){
      tmp <- paste0(input$first_letter,substr(tmp,2,5))
    }

    if(input$second_letter %in% LETTERS){
      tmp <- paste0(substr(tmp,1,1),input$second_letter,substr(tmp,3,5))
    }

    if(input$third_letter %in% LETTERS){
      tmp <- paste0(substr(tmp,1,2),input$third_letter,substr(tmp,4,5))
    }

    if(input$fourth_letter %in% LETTERS){
      tmp <- paste0(substr(tmp,1,3),input$fourth_letter,substr(tmp,5,5))
    }

    if(input$fifth_letter %in% LETTERS){
      tmp <- paste0(substr(tmp,1,4),input$fifth_letter)
    }

    # logger::log_info(tmp)
    dt <- ans[stringr::str_detect(Answer, tmp)]

    dt <- dt[Date >= input$date_range[1] & Date <= input$date_range[2]]

    if(input$check_dups){
      dt <- dt[duplicated(Answer)]
    }

    dt

  })

  output$answer_table <- renderDT(

    if(dim(dt())[1]==0L){
      data.table(" " = "There are no historical answers with the filtered parameters!")
    } else {

      setnames(dt(), "Date", "Date (YYYY-MM-DD)")

      DT::datatable(
        dt()[,Index:=NULL],
        options = list(
          autoWidth=TRUE,
          pageLength=10,
          language = list(search = 'Full Text Search:')
        )
      )
    }

  )

  output$letter_freq <- renderPlotly({
    letter_counts <- dt() |>
      unnest_tokens(char,Answer,"characters") |>
      mutate(position = forcats::fct_inorder(rep(c("1st letter","2nd letter","3rd letter","4th letter","5th letter"), dim(dt())[1]))) |>
      group_by(char, position) |>
      reframe(n = n())

    if(dim(letter_counts)[1]>0){
      plot_ly(letter_counts, x = ~char, y = ~n, color = ~position, type = "bar") |>
        layout(barmode = "stack")
    }

  })


  observeEvent(
    input$year_filter,
    if(input$year_filter==""){
      updateDateRangeInput(
        inputId = "date_range",
        start = "2021-06-19",
        end = Sys.Date()
      )
    } else {
      updateDateRangeInput(
        inputId = "date_range",
        start = paste0(input$year_filter,"-01-01"),
        end = paste0(input$year_filter,"-12-31")
      )
    }
  )

  observeEvent(
    input$reset_filter,
    handlerExpr = {

      updateSelectInput(
        inputId = "first_letter",
        selected = ""
      )
      updateSelectInput(
        inputId = "second_letter",
        selected = ""
      )
      updateSelectInput(
        inputId = "third_letter",
        selected = ""
      )
      updateSelectInput(
        inputId = "fourth_letter",
        selected = ""
      )
      updateSelectInput(
        inputId = "fifth_letter",
        selected = ""
      )
      updateDateRangeInput(
        inputId = "date_range",
        start = "2021-06-19",
        end = Sys.Date()
      )
      updateSelectInput(
        inputId = "year_filter",
        selected = ""
      )
      updateMaterialSwitch(
        session = session,
        inputId = "check_dups",
        value = FALSE
      )
    }
  )


}

# Run the application
shinyApp(ui = ui, server = server)
