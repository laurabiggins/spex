library(shiny)
library(magrittr)
library(shinydashboard)

show_browser <- TRUE

# for accessing data from spex upload location
#data_location <- "/data/private/shiny_scripts/spex_upload/inst/extdata/" 
data_location <- "inst/extdata/"
folders <- basename(list.dirs(data_location))
available_datasets <- folders[folders != "extdata"]

sample_names <- "sample_name"
bab_light_blue <- "#00aeef"
bab_dark_blue <- "#1d305f"

box_height <- 500

appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

# UI ----
ui <- tagList(
  fluidPage(
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      HTML('<link rel="icon" type="image/jpg" href = "images/spex_logo_rotated.png"/>'),
      #tags$script(src = "script.js"),
      tags$script(
#         "var exploreText = document.getElementById('explore');
#         exploreText.style.backgroundColor = 'red';"
       "$(document).on('shiny:connected', function() {
         Shiny.setInputValue('pre_chosen_dataset', document.location.hash, {priority: 'event'});
       })"
      )
    ),
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(
        width = 150,
        h1("Spex", id = "main_title"),
        tags$img(src = "images/spex_logo_rotated.png", width = "50", height = "50", id = "main_logo"),
        sidebarMenu(id = "sidebar_menu",
          menuItem("INFO", tabName = "INFO"),
          menuItem("DATA", tabName = "DATA"),
          menuItem("PLOTS", tabName = "PLOTS")
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "INFO",
            fluidRow(
              tabBox(
                id = "description_etc",
                width = 12, 
                height = box_height,
                tabPanel(
                  title = "Select dataset",
                  fluidRow(
                    column(width = 6, 
                      wellPanel( 
                        br(),
                        selectInput(
                          inputId = "choose_dataset",
                          label = NULL,
                          choices = c("choose dataset", available_datasets)
                        ),
                        actionButton(inputId = "load_data", label = "load dataset"),
                        br(),
                        br(),
                        br(),
                        textOutput(outputId = "currently_loaded_ds")
                      )
                    )
                  )
                ),
                tabPanel(
                  title = "Description",
                  wellPanel(
                    textOutput(outputId = "dataset_name"),
                    br(),
                    textOutput(outputId = "dataset_info")
                  )
                ),
                tabPanel(
                    title = "Sets of interest",
                    uiOutput(outputId = "all_set_info")
                ),
                tabPanel(
                  title = "Dataset summary",
                  uiOutput(outputId = "all_dataset_summary")
                )
              )
            ),
            fluidRow(
              conditionalPanel(
                condition = "input.choose_dataset != 'choose dataset'",
                box(
                  width = 12,
                  title = "filter",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  uiOutput(outputId = "all_filters")
                )
              )
            ),  
            p("For more information about work carried out at the Babraham Institute
                 visit the", a(href= "https://www.babraham.ac.uk/", "website")),
            br(),br(),br(),br(),
          ),
          tabItem(tabName = "DATA",
            br(),
            wellPanel(
              DT::dataTableOutput("data_table"),
              downloadButton("download_csv", "download csv")
            )
          ),
          tabItem(tabName = "PLOTS",
            fluidRow(
              box(
                width = 4,
                class = "plotbox",
                title = "histogram",
                collapsible = TRUE,
                mod_histogramUI("hist")
              ),
              box(
                width = 4,
                class = "plotbox",
                title = "scatterplot",
                collapsible = TRUE,
                mod_scatterplot_ui("scatter")
              ),
              box(
                width = 4,
                title = "violinplot",
                collapsible = TRUE,
                mod_violinplot_ui("violinplot")
              )
            ),
            fluidRow(
              box(
                title = "heatmap",
                collapsible = TRUE,
                width = 12,
                mod_heatmap_ui("heatmap")
              )
            )
          )
        )
      )
    ),
  ## footers ----
    br(),
    fluidRow(
      column(
        width = 3,
        tags$img(src = "images/bioinformatics_logo_small_grey.png",
                 width = "200", height = "71")
      ),
      column(
        width = 6,
        offset = 3,
        br(),
        br(),
        p("Any problems please email laura.biggins@babraham.ac.uk",
          style = "font-size:12px", align = "right")
      )
    ),
    br(),
    # tags$script(
  #   "var exploreText = document.getElementById('explore');
  #         exploreText.style.backgroundColor = 'red';"
  # ),
    if(show_browser) actionButton("browser", "browser")
  ),
  tags$script(src = "script.js")
)


# server ----
server <- function(input, output, session ) {
  
## reactive values ----  
  data_loaded <- reactiveVal(FALSE)
  
  #chosen_dataset <- eventReactive(input$load_data, input$choose_dataset)

  chosen_dataset <- reactiveVal("choose dataset")
  
  rv <- reactiveValues(
    dataset = NULL, # the currently loaded dataset
    long_data_tib = NULL, # tidied long version of dataset
    measure_names = NULL, # all the row (gene) names
    measures_of_interest = NULL, # sets of ids of interest
    metadata = NULL # list of accompanying metadata
  )

  observeEvent(input$load_data, {
    chosen_dataset(input$choose_dataset)
  })
      
  observeEvent(input$pre_chosen_dataset, {
    req(input$pre_chosen_dataset)
    print("from pre_chosen_dataset")
    cleaned_choice <- substring(input$pre_chosen_dataset, 2) # remove hash
    if(cleaned_choice %in%  available_datasets){
      chosen_dataset(cleaned_choice) 
      updateSelectInput(
        inputId = "choose_dataset",
        choices = c("choose dataset", available_datasets),
        selected = cleaned_choice
      )
    }
  })
  
  ## load data ----     
  #observeEvent(input$load_data, {
   observeEvent(chosen_dataset(), { 
      
      if(chosen_dataset() != "choose dataset") {
        
        # need checks here that the locations exist
        data_folder <- paste0(data_location, chosen_dataset(), "/")
        
        #if(file.exists(paste0(data_folder, "dataset.feather"))){
        #  dataset <- feather::read_feather(paste0(data_folder, "dataset.feather"))
       # } else {
          dataset  <- readRDS(paste0(data_folder, "dataset.rds"))
       # }
                                          
        metadata_processed  <- readRDS(paste0(data_folder, "metadata.rds"))
        
        if(file.exists(paste0(data_folder, "of_interest.rds"))){
          of_interest  <- readRDS(paste0(data_folder, "of_interest.rds"))
        } else of_interest <- NULL
        
        if(file.exists(paste0(data_folder, "info.rds"))){
          info  <- readRDS(paste0(data_folder, "info.rds"))
        } else info <- list(summary_info = "populate with info file")
        
        
        rv$dataset <- dataset
        rv$metadata <- metadata_processed
        rv$measure_names <- rownames(dataset)
        rv$measures_of_interest <- of_interest
        rv$info <- info
        
        meta_factors <- metadata_processed$meta_all %>%
          dplyr::mutate_if(is.character, factor) %>%
          dplyr::mutate_if(is.double, factor) %>%
          dplyr::mutate_if(is.integer, factor)
        
        tib <- tibble::as_tibble(dataset, rownames = "row_attribute")
        
        long_data_tibble <- tib %>% 
          tidyr::pivot_longer(cols = -row_attribute, names_to = sample_names) %>%
          tidyr::drop_na() %>%
          dplyr::left_join(meta_factors)
        
        rv$long_data_tib <- long_data_tibble
        
        updateSelectInput(
          inputId = "selected_condition",
          label = "select condition",
          choices = names(rv$metadata$meta_summary)
        )
     }
  })

  output$currently_loaded_ds <- renderText({
    
    if(chosen_dataset() == "choose dataset") {
      "No dataset currently loaded"
    } else {
      paste0(chosen_dataset(), " is currently loaded.")
    }
  })
  
## info tab ----    
  output$dataset_name <- renderText({
    chosen_dataset()
  })
  
  output$dataset_info <- renderText({
    
    if(chosen_dataset() == "choose dataset") {
      "Choose a dataset"
    } else {
      #"populate this with an info file"
      rv$info$summary_info
    }
    
  })

## metadata tab ----
### dataset summary ----  
### 
  output$all_dataset_summary <- renderUI({
    if(!isTruthy(rv$dataset)) {
      welltags <- p("Load a dataset to see summary information.")
    } else {
      welltags <- tagList(
        textOutput("meta_info1"),
        textOutput("meta_info2"),
        h6("Number of categories in each condition:"),
        tableOutput("meta_info3"),
        checkboxInput("show_meta_summary", "show more information on conditions"),
        conditionalPanel(
          condition = "input.show_meta_summary == 1",
          fluidRow(
            column(
              width = 4,
              selectInput(
                "selected_condition",
                "select condition",
                choices = ""
              ),
            ),
            column(width = 8, tableOutput("meta_summary"))
          )
        ),
        checkboxInput("show_meta", "show all metadata"),
        conditionalPanel(
          condition = "input.show_meta == 1",
          DT::dataTableOutput("meta_table")
        )
      )
    }
    wellPanel(
      class = "info_panel",
      welltags
    )
  })
  
### 
#### info that's always present if data is loaded ----   
  output$meta_info1 <- renderText({
    req(rv$metadata)
    req(rv$dataset)
    paste0(
      "The dataset contains ", 
      nrow(rv$metadata$meta_summary[[sample_names]]), 
      " samples and ",
      nrow(rv$dataset),
      " measures."
    )
  })
  
  output$meta_info2 <- renderText({
    req(rv$metadata)
    met <- rv$metadata
    paste0(
      "Conditions are: ", 
      paste0(
        names(met$meta_summary)[!names(met$meta_summary) %in% met$sample_name], 
        collapse = ", "
      ),
      "."
    )
  })
  
  output$meta_info3 <- renderTable({
    req(rv$metadata)
    tibble::enframe(sapply(rv$metadata$meta_summary, nrow))
  }, colnames = FALSE)
  
#### info in conditional panels ----
  output$meta_table <- DT::renderDataTable({
    req(rv$metadata)
    dt_setup(rv$metadata$meta_all, n_rows = 20)
  })
  
  output$meta_summary <- renderTable({
    req(input$selected_condition)
    req(rv$metadata$meta_summary)
    rv$metadata$meta_summary[[input$selected_condition]]
  })
  
### Sets of interest ----  
###
  output$all_set_info <- renderUI({
    if(!isTruthy(rv$dataset)) {
      welltags <- p("These can be sets of genes/proteins etc. of special interest that can be highlighted on plots.")
    } else {
      if(!isTruthy(rv$measures_of_interest)){
        welltags <- p("No sets of interest available, add some using the filter options.")
      } else {
        welltags <- tagList(
          textOutput("set_info1"),
          h6("Number in each set:"),
          tableOutput("set_info2"),
          checkboxInput("show_sets", "show items in set"),
          conditionalPanel(
            condition = "input.show_sets == 1",
            fluidRow(
              column(
                width = 4,
                selectInput(
                  "selected_set",
                  "select set",
                  choices = ""
                )
              ),
              column(width = 8, tableOutput("set_summary"))
            )
          )
        )
      }
    }
    wellPanel(
      id = "sets_of_interest_panel", 
      class = "info_panel",
      welltags
    )
  })
  
  output$set_info1 <- renderText({
    req(rv$measures_of_interest)
    n_sets <- length(rv$measures_of_interest)
    if(n_sets == 1) text <- " set available"
    else text <- " sets available"
    paste0(n_sets, text, ". To add more, use the filter options.")
  })  
  
  output$set_info2 <- renderTable({
    req(rv$measures_of_interest)
    tibble::enframe(sapply(rv$measures_of_interest, nrow))
  }, colnames = FALSE)

#### table in conditional panel  ----   
  output$set_summary <- renderTable({
    req(input$selected_set)
    req(rv$measures_of_interest)
    rv$measures_of_interest[[input$selected_set]]
  })
  
  ### filter panel ----
  output$all_filters <- renderUI({
    if(!isTruthy(rv$dataset)) {
      wellPanel(p("Load dataset to see filter options.")) 
    } else {
      wellPanel(mod_name_filter_ui("name_filter"))
    }
  })
  
  ## data tab ----
  output$data_table <- DT::renderDataTable({
    req(rv$dataset)
    #dt_setup(rv$dataset, n_rows = 20, dom_opt = "ftlip", show_rownames = TRUE)
    
    DT::datatable(
      rv$dataset,
      options = list(
        dom = "ftlip", 
        scrollX = TRUE, 
        autoWidth = FALSE
      )
    ) %>% 
      DT::formatStyle(0, target = 'row', `font-size` = '90%', lineHeight = '80%') %>%
      DT::formatRound(columns = 1:ncol(rv$dataset), digits = 2)
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste(chosen_dataset(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(x = rv$dataset, file)
    }
  )

## plot tab ---- 
### histogram module ----    
  mod_histogramServer(
    "hist",
    data_to_plot = reactive(rv$long_data_tib),
    meta = reactive(rv$metadata),
    chosen_dataset
  )

### heatmap module ----       
  mod_heatmap_server(
    "heatmap",
    dataset = reactive(rv$dataset),
    metadata = reactive(rv$metadata),
    sample_name_col = sample_names,
    of_interest = reactive(rv$measures_of_interest),
    chosen_dataset = chosen_dataset
  )

### scatterplot module ----     
  mod_scatterplot_server(
    "scatter",
    data_to_plot = reactive(rv$long_data_tib),
    metadata = reactive(rv$metadata),
    sample_name_col = sample_names,
    sets_of_interest = reactive(rv$measures_of_interest),
    chosen_dataset = chosen_dataset
  )

### violinplot module ----       
  mod_violinplot_server(
    "violinplot", 
    long_data_tib = reactive(rv$long_data_tib),
    chosen_dataset = chosen_dataset, 
    metadata = reactive(rv$metadata),
    sample_name_col = sample_names
  )

## filter tab 
### filter module ----
  
  filter_results <- mod_name_filter_server(
    "name_filter", 
    reactive(rv$measure_names), 
    of_interest = reactive(rv$measures_of_interest),
    chosen_dataset = chosen_dataset
  )

### filter results - observeEvent    
  observeEvent(filter_results(), {
    
    rv$measures_of_interest <- filter_results()
    print("filter results updated")
    updateSelectInput(inputId = "selected_set", choices = names(rv$measures_of_interest))
  })
  
  observeEvent(input$browser, browser())
  
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")    
  shinyjs::show("app-content")

}


shinyApp(ui = ui, server = server)
