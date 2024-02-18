
modWordListUI <- function(id,
                          dt_words,
                          sys_date_,
                          years_,
                          dups_present_,
                          days_since_last_update_,
                          use_date_filters = FALSE,
                          use_repeat_toggle = FALSE){
  tagList(
    br(),
    tags$h1(
      tags$span("Word List Filters", style = "color:#3BC143"),
      tags$span(
        actionButton(
          inputId = NS(id,"reset_filter"),
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
             inputId = NS(id,"first_letter"),
             label = "1st:",
             choices = c("", LETTERS),
             selected = "",
             multiple = FALSE
           )
          ),
          column(2,
           selectizeInput(
             inputId = NS(id,"second_letter"),
             label = "2nd:",
             choices = c("", LETTERS),
             selected = "",
             multiple = FALSE
           )
          ),
          column(2,
           selectizeInput(
             inputId = NS(id,"third_letter"),
             label = "3rd:",
             choices = c("", LETTERS),
             selected = "",
             multiple = FALSE
           )
          ),
          column(2,
           selectizeInput(
             inputId = NS(id,"fourth_letter"),
             label = "4th:",
             choices = c("", LETTERS),
             selected = "",
             multiple = FALSE
           )
          ),
          column(2,
           selectizeInput(
             inputId = NS(id,"fifth_letter"),
             label = "5th:",
             choices = c("", LETTERS),
             selected = "",
             multiple = FALSE
           )
          )
        )
      ),
      tagList(
        if(use_date_filters){
          accordion_panel(
            "By Date",
            dateRangeInput(
              inputId = NS(id,"date_range"),
              label = "Date Range",
              start = "2021-06-19",
              end = sys_date_
            ),
            selectInput(
              inputId = NS(id,"year_filter"),
              label = "Specific Year",
              choices = c("", years_),
              selected = "",
              multiple = FALSE
            )
          )
        } else {
         div()
        }
      ),
      tagList(
        if(use_repeat_toggle){
          accordion_panel(
            "Repeated Answer Words",
            shinyWidgets::materialSwitch(
              inputId = NS(id,"check_dups"),
              label = NULL,
              value = FALSE,
            ),
            if(!dups_present_){tags$p(paste0("Note: No repeats identified as of ",max(dt_words$Date)), style = "font-size:90%")},
          )
        } else {
          div()
        }
      )
    ),
    br(),
    br(),
    tags$h1("Word List Analysis", style = "color:#3BC143"),
    if("Date" %in% names(dt_words)){
      tags$p(paste0("The answer list is updated periodically. Last update: ",max(dt_words$Date)," (",days_since_last_update_,ifelse(days_since_last_update_<=1," day ago)."," days ago).")))
    },
    accordion(
      open = TRUE,
      accordion_panel(
        paste(stringr::str_to_title(id), " Word Table"),
        column(6,
          DTOutput(NS(id,"word_list_table"))
        )
      ),
      accordion_panel(
        "Character Frequency",
        card(
          plotlyOutput(NS(id,"letter_freq"))
        )
      ),
      br()
    )
  )
}

modWordListServer <- function(id, dt_words, sys_date_, use_date_filters = FALSE, use_repeat_toggle = FALSE){
  moduleServer(
    id,
    function(input, output, session){
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
        dt <- dt_words[stringr::str_detect(Word, tmp)]

        if(use_date_filters){
          dt <- dt[Date >= input$date_range[1] & Date <= input$date_range[2]]
        }

        if(use_repeat_toggle){
          if(input$check_dups){
            dt <- dt[duplicated(Word)]
          }
        }

        dt

      })

      output$word_list_table <- renderDT(

        if(dim(dt())[1]==0L){
          data.table(" " = "There are no historical answers with the filtered parameters!")
        } else {

          if("Date" %in% names(dt())){
            setnames(dt(), "Date", "Date (YYYY-MM-DD)")
          }

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
          unnest_tokens(char,Word,"characters") |>
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

      if(use_date_filters){
        observeEvent(
          input$year_filter,
          if(input$year_filter==""){
            updateDateRangeInput(
              inputId = "date_range",
              start = "2021-06-19",
              end = sys_date_
            )
          } else {
            updateDateRangeInput(
              inputId = "date_range",
              start = paste0(input$year_filter,"-01-01"),
              end = paste0(input$year_filter,"-12-31")
            )
          }
        )
      }

      observeEvent(
        input$reset_filter,
        handlerExpr = {

          updateSelectizeInput(
            session,
            inputId = "first_letter",
            selected = ""
          )
          updateSelectizeInput(
            session,
            inputId = "second_letter",
            selected = ""
          )
          updateSelectizeInput(
            session,
            inputId = "third_letter",
            selected = ""
          )
          updateSelectizeInput(
            session,
            inputId = "fourth_letter",
            selected = ""
          )
          updateSelectizeInput(
            session,
            inputId = "fifth_letter",
            selected = ""
          )
          if(use_date_filters){
            updateDateRangeInput(
              session,
              inputId = "date_range",
              start = "2021-06-19",
              end = sys_date_
            )
            updateSelectInput(
              session,
              inputId = "year_filter",
              selected = ""
            )
          }
          if(use_repeat_toggle){
            updateMaterialSwitch(
              session,
              inputId = "check_dups",
              value = FALSE
            )
          }
        }
      )

    }
  )
}