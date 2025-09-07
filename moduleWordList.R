
modWordListUI <- function(id,
                          dt_words_,
                          max_date_,
                          years_,
                          dups_present_,
                          days_since_last_update_){
  tagList(
    br(),
    tags$p(paste0(
      "Wordle Answer list refreshed as of ",
      max(dt_words_[!is.na(Date)]$Date)," (",
      days_since_last_update_,
      ifelse(days_since_last_update_<=1," day ago)."," days ago)."),
      " Today's word will not be displayed to prevent spoilers.")
    ),
    tags$div(
      tags$div(
        shinyWidgets::materialSwitch(
          inputId = NS(id,"include_scrabble_dict"),
          label = "Include Scrabble:",
          status = "success",
          width = "150px",
          value = FALSE
        ),
        style = "display: inline-block; font-size:9pt;"
      ),
      tags$div(
        shinyWidgets::materialSwitch(
          inputId = NS(id,"exclude_past_answers"),
          label = "Exclude Wordle:",
          status = "warning",
          width = "150px",
          value = FALSE
        ),
        style = "display: inline-block; font-size:9pt;"
      ),
      tags$div(
        actionButton(
          inputId = NS(id,"reset_filter"),
          label = "Reset Filters",
          style = "background:#EDC001; color:#222222; font-weight:bold"
        ),
        style = "display: inline-block;"
      ),
      br()
    ),
    br(),
    accordion(
      open = FALSE,
      multiple = FALSE,
      accordion_panel(
        "Expand for advanced filters",
        tags$div(
          tags$p("By RegEx", style = "display: inline-block; margin-right: 5px"),
          tags$div(
            actionButton(
              inputId = NS(id,"clear_regex"),
              label = "Clear",
              style = "background:#EDC001; color:#222222; font-weight:bold; padding:2px"
            ),
            style = "display: inline-block;"
          ),
          tags$div(
            textAreaInput(
              inputId = NS(id,"regex_str"),
              label = NULL,
              value=paste0("[",c(1:5),rep("ABCDEFGHIJKLMNOPQRSTUVWXYZ",5),"]", collapse = ""),
              height = "150px",
              width = "300px"
            ),
            style = "margin-bottom:5px;"
          )
        ),
        tags$div(
          tags$span(
            paste0(
              "Letters within each pair of square brackets are evaluated as possible ",
              "answers for a single letter position. Each leading number indicates the ",
              "letter position that the RegEx pertains to, but it is not evaluated or necessary. ",
              "The default regex shown above is a solution for all five-letter words possible. ",
              "Therefore, remove letters that you know have been eliminated from each position. "
            ),
            style = "font-size:9pt; margin-top:5px;"
          ),
          style = "margin-top:5px; margin-bottom:35px; width:250px; line-height:1"
        ),
        # div(style = "margin-bottom:35px"),
        dateRangeInput(
          inputId = NS(id,"date_range"),
          label = "By Date Range",
          start = "2021-06-19",
          end = max_date_
        ),
        selectInput(
          inputId = NS(id,"year_filter"),
          label = "By Specific Year",
          choices = c("", years_),
          selected = "",
          multiple = FALSE
        ),
        shinyWidgets::materialSwitch(
          inputId = NS(id,"check_dups"),
          label = "Check for Duplicates",
          value = FALSE,
          inline = TRUE,
          status = "success"
        ),
        if(!dups_present_){tags$p(paste0("No repeats identified as of ",max(dt_words_[!is.na(Date)]$Date)), style = "font-size:90%")}
      )
    ),
    br(),
    accordion(
      open = TRUE,
      accordion_panel(
        "Word Table",
        column(6,
          DTOutput(NS(id,"word_list_table"))
        )
      )
    ),
    br(),
    tags$h1("Letter Analysis", style = "color:#3BC143"),
    accordion(
      open = FALSE,
      accordion_panel(
        title = "Character Frequency",
        p("(Note: Results change only when using the Advanced Filters.)"),
        card(
          plotlyOutput(NS(id,"letter_freq"))
        )
      ),
      br()
    )
  )
}

modWordListServer <- function(id, dt_words_, max_date_){
  moduleServer(
    id,
    function(input, output, session){

      re <- reactive(
        if(is.null(input$regex_str) | input$regex_str==""){
          "."
        } else {
          str_to_upper(input$regex_str)
        }
      )

      dt <- reactive({

        # logger::log_info(re())

        dt <- tryCatch(
          expr = {dt_words_[str_detect(Word, re())]},
          error = function(e){dt_words_[Word=="12345"]}
        )

        if(!input$include_scrabble_dict){
          dt <- dt[(Date >= input$date_range[1] & Date <= input$date_range[2])]
        } else {
          dt <- dt[is.na(Date) | (Date >= input$date_range[1] & Date <= input$date_range[2])]
        }

        if(input$include_scrabble_dict){
          dt
        } else {
          dt <- dt[!is.na(Index)] # this needs to use Index since the Date for today's word is removed last in this reactive
          setorder(dt, -Index)
        }

        if(input$exclude_past_answers){
          dt <- dt[is.na(Date)]
        }

        if(input$check_dups){
          dt <- dt[duplicated(Word)]
        }

        # this has to come last to remove today's Wordle answer Date since
        # it converts dates to character strings that would otherwise break other logic for filtering
        dt[,Date:=ifelse(!is.na(Date) & is.na(Index),NA_character_,as.character(Date))]

        dt

      })

      output$word_list_table <- renderDT(

        if(dim(dt())[1]==0L){
          data.table(" " = "There are no historical answers with the filtered parameters!")
        } else {

          DT::datatable(
            dt()[,.(Word, Date, Index)],
            rownames = FALSE,
            class = 'compact',
            callback = JS("$(document).ready(function() {
                  $('table.dataTable').css('line-height', '0.7'); // Adjust line height
                });"),
            options = list(
              pagingType = "full",
              pageLength=10,
              language = list(search = 'Basic Search:'),
              dom = '<"top"f>t<"bottom"ilp>' # Customize layout
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


      observeEvent(
        input$year_filter,
        if(input$year_filter==""){
          updateDateRangeInput(
            inputId = "date_range",
            start = "2021-06-19",
            end = max_date_
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
        input$clear_regex,
        updateTextInput(
          session,
          inputId = "regex_str",
          value=""
        )
      )

      observeEvent(
        input$reset_filter,
        handlerExpr = {

          updateTextInput(
            session,
            inputId = "regex_str",
            value=paste0("[",c(1:5),rep("ABCDEFGHIJKLMNOPQRSTUVWXYZ",5),"]", collapse = "")
          )

          updateDateRangeInput(
            session,
            inputId = "date_range",
            start = "2021-06-19",
            end = max_date_
          )

          updateSelectInput(
            session,
            inputId = "year_filter",
            selected = ""
          )

          updateMaterialSwitch(
            session,
            inputId = "include_scrabble_dict",
            value = FALSE
          )

          updateMaterialSwitch(
            session,
            inputId = "exclude_past_answers",
            value = FALSE
          )

          updateMaterialSwitch(
            session,
            inputId = "check_dups",
            value = FALSE
          )
        }
      )

    }
  )
}