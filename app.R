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

ans <- fread("wordle_answers.csv")
ans[,Date:=lubridate::mdy(Date)]
days_since_last_update <- as.integer(as.POSIXct(Sys.Date()) - as.POSIXct(max(ans$Date)))
years <- unique(ans[,.(lubridate::year(Date))]) |> unlist() |> sort(decreasing=TRUE) |> as.character()
dups_present <- dim(ans[duplicated(Answer)])[1]>0

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
      "Searchable Answers",
      br(),
      tags$h1(
        tags$span("Word List Filters", style = "color:#3BC143"),
        tags$span(
          actionButton(
            inputId = "reset_filter",
            label = "Reset Filters",
            style = "background:#EDC001; color:#222222; font-weight:bold"
          ),
          style = "text-align:right"
        )
      ),
      accordion(
        open = FALSE,
        multiple = FALSE,
        accordion_panel(
          "By Letter Position",
          fluidRow(
            column(2,
               selectizeInput(
                 inputId = "first_letter",
                 label = "1st:",
                 choices = c("", LETTERS),
                 selected = "",
                 multiple = FALSE
               )
            ),
            column(2,
              selectizeInput(
                inputId = "second_letter",
                label = "2nd:",
                choices = c("", LETTERS),
                selected = "",
                multiple = FALSE
              )
            ),
            column(2,
              selectizeInput(
                inputId = "third_letter",
                label = "3rd:",
                choices = c("", LETTERS),
                selected = "",
                multiple = FALSE
              )
            ),
            column(2,
              selectizeInput(
                inputId = "fourth_letter",
                label = "4th:",
                choices = c("", LETTERS),
                selected = "",
                multiple = FALSE
              )
            ),
            column(2,
              selectizeInput(
                inputId = "fifth_letter",
                label = "5th:",
                choices = c("", LETTERS),
                selected = "",
                multiple = FALSE
              )
            )
          )
        ),
        accordion_panel(
          "By Date",
          dateRangeInput(
            inputId = "date_range",
            label = "Date Range",
            start = "2021-06-19",
            end = Sys.Date()
          ),
          selectInput(
            inputId = "year_filter",
            label = "Specific Year",
            choices = c("", years),
            selected = "",
            multiple = FALSE
          )
        ),
        accordion_panel(
          "Repeated Answer Words",
          shinyWidgets::materialSwitch(
            inputId = "check_dups",
            label = NULL,
            value = FALSE,
          ),
          if(!dups_present){tags$p(paste0("Note: No repeats identified as of ",Sys.Date()), style = "font-size:90%")},
        ),
        br()
      ),
      br(),
      tags$h1("Word List Analysis", style = "color:#3BC143"),
      tags$p(paste0("The answer list is updated periodically. Last update: ",max(ans$Date)," (",days_since_last_update,ifelse(days_since_last_update<=1," day ago)."," days ago)."))),
      accordion(
        open = TRUE,
        accordion_panel(
          "Searchable Word Table",
          column(6,
            DTOutput("answer_table")
          )
        ),
        accordion_panel(
          "Character Frequency",
          card(
            plotlyOutput("letter_freq")
          )
        ),
        br()
      )
    ), # close nav_panel
    nav_panel(
      "About",
      column(8,
        br(),
        p("Hi! Thanks for visiting!"),
        tags$span("Occasionally, in the puzzle game "),
        tags$a(href="https://www.nytimes.com/games/wordle","Wordle,", style = "color:#3BC143"),
        tags$span(" I find myself wanting to know if a word was previously used or if a Wordle answer has ever been repeated. Ya know, fun stuff you think about while riding a rollercoaster."),
        p(),
        p("I find it de-motivating to visit ad-bloated websites to wade through multi-paragraph lead-ins to only have to scan an ever growing list of words. So, I decided to make a lightweight and data-focused website instead that can do this for me -- and now also you!"),
        p("I plan to update this list every couple of weeks, assuming the hubbub of life doesn't get the best of me. If I can figure out a way to create a feedback form, I'll add one and then you can nag me to update."),
        tags$span("Happy Wordle-ing!", style="font-weight:bold; font-size:110%"),
        tags$span(" (let's pretend that's a thing normal people say)", style="font-size:80%")
      )
    ) # close nav_panel About
  ) # close navset_panel
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
        )) |>
        formatStyle(columns = names(dt())[1], color = "#EDC001") |>
        formatStyle(columns = names(dt())[2], color = "#3BC143")
    }

  )

  output$letter_freq <- renderPlotly({
    letter_counts <- dt() |>
      unnest_tokens(char,Answer,"characters") |>
      mutate(
        char = toupper(char),
        position = forcats::fct(
          rep(c("1st","2nd","3rd","4th","5th"), dim(dt())[1]),
          c("5th","4th","3rd","2nd","1st")
        )
      ) |>
      group_by(char, position) |>
      reframe(n = n())

    if(dim(letter_counts)[1]>0){
      plot_ly(letter_counts, x = ~char, y = ~n, color = ~position, type = "bar") |>
        layout(
          barmode = "stack",
          yaxis = list(title = "Frequency of Occurance", color = "#CCCCCC"),
          xaxis = list(title = "Letters", color = "#CCCCCC"),
          legend = list(title = list(text="<b>Letter Position</b>", font = list(color="#CCCCCC")),
                        font = list(color="#CCCCCC")),
          paper_bgcolor = "#363636",
          plot_bgcolor = "#363636"
        )
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

      updateSelectizeInput(
        inputId = "first_letter",
        selected = ""
      )
      updateSelectizeInput(
        inputId = "second_letter",
        selected = ""
      )
      updateSelectizeInput(
        inputId = "third_letter",
        selected = ""
      )
      updateSelectizeInput(
        inputId = "fourth_letter",
        selected = ""
      )
      updateSelectizeInput(
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
