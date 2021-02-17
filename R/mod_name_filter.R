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
      
      req(matched_names())
      
      # if(nchar(input$set_name) >= 1){
        this_set_name <- input$set_name
      # } else {
      #   this_set_name <- paste0("set_", length(of_interest)+1)
      # }
      #of_interest[[this_set_name]] = matched_names()
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
  # remove whitespace
}    
    
#' Title
#'
#' @param selected_names names to filter on
#' @param all_names  all the rownames from the main dataset
#'
#' @return
#' @export
#'
#' @examples
match_names <- function(selected_names, all_names){
  
  #convert to upper or lower case
  selected_names[selected_names %in% all_names]
  
}    
    
    
    
    
    