#' filter_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_name_filter_ui <- function(id){#}, measure_names){
  
  ns <- NS(id)
  
  tabPanel(
    id,
    wellPanel(
      # style = "padding: 10px",
      fluidRow(
        column(
          width = 6,
          textInput(
            ns("pasted_names"),
            label = NULL,
            width = "400px",
            placeholder = "Enter names here e.g. gene1, gene2"
          )
        ),
        column(
          width = 6,
          actionButton(ns("search_names"), "search")
        ),
        radioButtons(
          ns("name_delimiter"), 
          label = "separator", 
          choices = c("space/tab" = "space", "comma" = "comma"),
          inline = TRUE
        )
      ),  
      textOutput(ns("search_summary")),
      br(),
      fluidRow(
        column(
          width = 6,
          textInput(ns("set_name"), label = NULL, width = "300px", placeholder = "name of set")
        ),
        column(
          width = 6,
          actionButton(ns("add_names"), label = "Add")
        )
      ),
      textOutput(ns("add_set_msg")),
      br(),
      br(),
      actionButton(ns("browser"), "browser"),
      br(),
      checkboxInput(ns("show_dropdown"), "select from dropdown list")
    )
  )  
}


mod_name_filter_server <- function(id, measure_names, of_interest){
  
  moduleServer(id, function(input, output, session) {
      
    observeEvent(input$browser, browser())
    
    sets_of_interest <- reactiveVal(of_interest)
    
    entered_names <- eventReactive(input$search_names, {
      
      split_names(input$pasted_names, input$name_delimiter)
    })
    
    matched_names <- reactive({
      
      match_names(entered_names(), measure_names)
    })
    
    observeEvent(input$add_names, {
      
      shinyFeedback::feedbackWarning(
        "add_set_msg", 
        !isTruthy(matched_names()), 
        "Cannot add set with no matched names"
      )
      
      req(matched_names())
      
      if(nchar(input$set_name) >= 1){
       this_set_name <- input$set_name
      } else {
        this_set_name <- paste0("set_", length(of_interest)+1)
      }
     
      sets <- sets_of_interest()
      sets[[this_set_name]] = tibble::tibble(this_set_name = matched_names())
        
      sets_of_interest(sets)
      
      print("added set")
      set_msg(paste0("Added set ", input$set_name))
    })
    
    set_msg <- reactiveVal()
    
    output$add_set_msg <- renderText(set_msg())
    
    
    # this needs extracting and making robust to display useful messages
    search_msg <- reactive({
      
      req(entered_names())
      req(matched_names())
      
      paste0(
        length(entered_names()), 
        " names entered, ", 
        length(matched_names()),
        " of these were found in the dataset"
      )
      
    })
    
    output$search_summary <- renderText(search_msg())
    
    observeEvent(input$search_names, {
    
      #print("from the observer")
      
      n <- isTruthy(input$pasted_names)
      shinyFeedback::feedbackWarning("pasted_names", !n, "Please enter some names")
      
      delim_check <- (!isTruthy(matched_names()) & length(entered_names()) == 1)
      
      shinyFeedback::feedbackWarning(
        "pasted_names", 
        delim_check, 
        "No names matched the dataset, do you need to change the delimiter option below?"
      )
      
      next_check <- (!isTruthy(matched_names())) & length(entered_names()) > 1
      shinyFeedback::feedbackWarning(
        "pasted_names",
        next_check,
        "No names matched the dataset, make sure the separator option is set correctly
        below.
        Select from the set of names in the dataset by using the dropdown list below."
      )
    })
    
    reactive(sets_of_interest())
  })
}
    
    
#' Title
#'
#' @param name_string 
#' @param delimiter one of "space" or "comma"
#'
#' @return
#' @export
#'
#' @examples
split_names <- function(name_string, delimiter){
  
  delim <- switch(delimiter,
    space = " ",
    comma = ","
  )
  stringr::str_split(name_string, pattern = delim)[[1]]
  #stringr::str_trim(name_string)
}    
    
#' Title
#'
#' @param selected_names names to filter on
#' @param all_names  all the rownames from the main dataset
#'
#' @return vector of names that matched
#' @export
#'
#' @examples
match_names <- function(selected_names, all_names){
  
  #convert to upper or lower case
  selected_names[toupper(selected_names) %in% toupper(all_names)]
}    
    
    
    
    
    