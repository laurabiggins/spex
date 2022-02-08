library(shiny)
library(magrittr)
library(shinydashboard)

show_browser <- TRUE

# TODO !! data summary and sets of interest do not update !!
# TODO the metadata should contain the sample name

# for accessing data from spex upload location
#data_location <- "/data/private/shiny_scripts/spex_upload/inst/extdata/" 
data_location <- "inst/extdata/"
folders <- basename(list.dirs(data_location))
available_datasets <- folders[folders != "extdata"]

data_summary_table <- readRDS("inst/extdata/summary_info.rds")

sample_names <- "sample_name"
bab_light_blue <- "#00aeef"
bab_dark_blue <- "#1d305f"

#box_height <- 900

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
       "$(document).on('shiny:connected', function() {
         Shiny.setInputValue('pre_chosen_dataset', document.location.hash, {priority: 'event'});
       })"
      ),
      tags$style(".inactiveLink {
                 cursor: not-allowed;
                 }
                 .inactiveLink:active {
                 pointer-events: none;
                 }"
                 )
    ),
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(
        width = 150,
        h1("Spex", id = "main_title"),
        sidebarMenu(id = "sidebar_menu",
          menuItem("BROWSE", tabName = "BROWSE"),
          menuItem("DATA", tabName = "DATA", icon = NULL,
            menuSubItem(text = "entire dataset", tabName = "all_data"),
            menuSubItem(text = "data summary", tabName = "data_summary"),
            menuSubItem(text = "sets of interest", tabName = "sets_of_interest"),
            menuSubItem(text = "filter", tabName = "filter")
          ),
          menuItem("PLOTS", tabName = "PLOTS")
        ),
        br(),
        br(),
        br(),
        tags$img(src = "images/spex_logo_grey.png", width = "70", height = "50", id = "main_logo")
      ),
      dashboardBody(
        uiOutput(outputId = "info_banner"),
        tabItems(
          ## browse tab ----
          tabItem(tabName = "BROWSE",
            wellPanel(class = "my_info_panel",
              DT::dataTableOutput("summary_table_all")
            ),
            wellPanel(id="selected_data_info", class = "my_info_panel",
              textOutput(outputId = "selected_dataset_title"),
              textOutput(outputId = "selected_dataset_citation"),
              br(),
              textOutput(outputId = "selected_dataset_summaryinfo")
            )        
          ),
          ## data tab ----
          tabItem(tabName = "all_data",
                  uiOutput(outputId = "entire_dataset")
          ),
          tabItem(tabName = "data_summary",
                  uiOutput(outputId = "all_dataset_summary")
          ),
          tabItem(tabName = "sets_of_interest",
                  uiOutput(outputId = "all_set_info")
          ),
          tabItem(tabName = "filter",
                  uiOutput(outputId = "filter")
          ),
          ## plot tab ----
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
                class = "plotbox",
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
    if(show_browser) actionButton("browser", "browser")
  )
)

# server ----
server <- function(input, output, session ) {
  
  shinyjs::addCssClass(selector = "a[data-value='PLOTS']", class = "inactiveLink")
  shinyjs::addCssClass(selector = "a[data-value='DATA']", class = "inactiveLink")
  shinyjs::hide(id="selected_data_info")
  
  currently_loaded_dataset <- reactive({
    if(chosen_dataset() == "choose dataset"){
      msg <- "No dataset is currently loaded"
    } else {
      msg <- paste0(chosen_dataset(), " is currently loaded")
    }
    msg
  })
  
  output$info_banner <- renderUI({
    if(input$sidebar_menu == "BROWSE"){
      if(is.null(input$summary_table_all_rows_selected)){
        bannertags <- tagList(
          p(id = "choose_ds_msg", "Choose dataset from table", class = "banner-text"),
        )
      } else if(chosen_dataset() != selected_dataset_name()) {
        bannertags <- tagList(
            actionButton(inputId = "load_dataset_btn", label = "load selected dataset", class = "button")
        )
      } else if(chosen_dataset() == selected_dataset_name()) {
        bannertags <- tagList(p(id="banner_filler", "", class = "banner-text")) # filler so screen doesn't jump around
      }
    } else bannertags <- NULL
 
    tagList(
      bannertags,
      p(id = "loaded_ds_msg", class = "banner-text", currently_loaded_dataset())
    )
  })
  
  
## reactive values ----  
 # data_loaded <- reactiveVal(FALSE)
  chosen_dataset <- reactiveVal("choose dataset")
  
  rv <- reactiveValues(
    dataset = NULL, # the currently loaded dataset
    long_data_tib = NULL, # tidied long version of dataset
    measure_names = NULL, # all the row (gene) names
    measures_of_interest = NULL, # sets of ids of interest
    metadata = NULL # list of accompanying metadata
  )
  
  # dataset name selected from the table - it doesn't have to be loaded to view 
  # summary text info
  selected_dataset_name <- reactive({
    if(is.null(input$summary_table_all_rows_selected)) return (NULL)
    row_number <- as.numeric(input$summary_table_all_rows_selected)
    data_summary_table$dataset_name[row_number] 
  })
  
  observeEvent(selected_dataset_name(), ignoreNULL = FALSE, {
    if(is.null(selected_dataset_name())){
      shinyjs::hide(id="selected_data_info")
    } else {
      shinyjs::show(id="selected_data_info")
    }
  })
  
  observeEvent(input$load_dataset_btn, {
    chosen_dataset(selected_dataset_name())
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
  observeEvent(chosen_dataset(), { 
    
    if(chosen_dataset() != "choose dataset") {
      
      # need checks here that the locations exist
      data_folder <- paste0(data_location, chosen_dataset(), "/")
      
      dataset  <- readRDS(paste0(data_folder, "dataset.rds"))
                                        
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
        inputId = "selected_set", 
        label = "Select set to see details",
        choices = names(rv$measures_of_interest)
      )
      
      shinyjs::removeCssClass(selector = "a[data-value='PLOTS']", class = "inactiveLink")
      shinyjs::removeCssClass(selector = "a[data-value='DATA']", class = "inactiveLink")
      
    } else {
      shinyjs::addCssClass(selector = "a[data-value='PLOTS']", class = "inactiveLink")
      shinyjs::addCssClass(selector = "a[data-value='DATA']", class = "inactiveLink")
   }
  })

  ## Browse tab ----
  output$summary_table_all <- DT::renderDataTable({
    
    DT::datatable(
      data_summary_table,
      rownames = FALSE,
      selection = "single",
      options = list(
        dom = "t",
        scrollX = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(
            targets = c(1,2),
            render = htmlwidgets::JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data.length > 50 ?",
              "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
              "}")
          )
        )
      )
    )    
  })
  
  output$selected_dataset_title <- renderText({
    req(input$summary_table_all_rows_selected)
    row_number <- as.numeric(input$summary_table_all_rows_selected)
    data_summary_table$dataset_name[row_number]  
  })
  
  output$selected_dataset_summaryinfo <- renderText({
    req(input$summary_table_all_rows_selected)
    row_number <- as.numeric(input$summary_table_all_rows_selected)
    data_summary_table$summary_info[row_number]  
  })
  
  output$selected_dataset_citation <- renderText({
    req(input$summary_table_all_rows_selected)
    row_number <- as.numeric(input$summary_table_all_rows_selected)
    data_summary_table$citation[row_number]  
  })
  
## Data tab ----
## 
### entire dataset ----
### 
  output$entire_dataset <- renderUI({
    if(!isTruthy(rv$dataset)) {
      welltags <- p("Load a dataset from the browse tab to see information here.")
    } else {
      welltags <- tagList(
        DT::dataTableOutput("data_table"),
        downloadButton("download_csv", "download csv")
      )
    }   
    wellPanel(class = "my_info_panel", welltags)
  })
    
  output$data_table <- DT::renderDataTable({
    req(rv$dataset)
    DT::datatable(
      rv$dataset,
      options = list(dom = "ftlip", scrollX = TRUE, autoWidth = FALSE, pageLength = 15)
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
 
### data summary ----  
  output$all_dataset_summary <- renderUI({
    if(!isTruthy(rv$dataset)) {
      welltags <- p("Load a dataset from the browse tab to see summary information.")
    } else {
      welltags <- tagList(
        fluidRow(
          column(width = 6, 
            p(class="section_header", "Data summary"),
            br(),
            textOutput("meta_info1"),
            br(),
            textOutput("meta_info2")
          ),
          column(width=6, 
            p(class="section_header", "Number of categories in each condition"),
            DT::dataTableOutput("meta_info3")
          )
        ),
        br(),
        p(class="section_header", "All metadata"),
        DT::dataTableOutput("meta_table")
      )
    }
    wellPanel(class = "my_info_panel", welltags)
  })
  
### 
#### Data summary text 1 ----   
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

#### Data summary text 2 - conditions ----    
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

#### Table of categories in each condition ----
  output$meta_info3 <- DT::renderDataTable({
    req(rv$metadata)
    table_data <- tibble::enframe(sapply(rv$metadata$meta_summary, nrow))
    DT::datatable(
      table_data,
      rownames = FALSE,
      options = list(
        dom = "t"
      )
    ) %>% 
      DT::formatStyle(0, target = 'row', `font-size` = '90%', lineHeight = '80%')
  })

#### All metadata table ----    
  output$meta_table <- DT::renderDataTable({
    req(rv$metadata)
    DT::datatable(
      rv$metadata$meta_all,
      rownames = FALSE,
      options = list(
        dom = "tip",
        pageLength = 12
      )
    ) %>% 
      DT::formatStyle(0, target = 'row', `font-size` = '90%', lineHeight = '80%')
  })
  
### Sets of interest ----  
###
  output$all_set_info <- renderUI({
    if(!isTruthy(rv$dataset)) {
      welltags <- tagList(
        br(),
        p("Load a dataset from the browse tab.")
      )
    } else {
      if(!isTruthy(rv$measures_of_interest)){
        welltags <- tagList(
          br(),
          p("No sets of interest have been loaded for this dataset, add some using the filter options (soon).")
        )
      } else {
        welltags <- tagList(
          textOutput("set_info1"),
          br(),
          fluidRow(
            column(
              width = 4,
              p(class="section_header", "Number in each set"),
              tableOutput("set_info2"),
              br(),
              selectInput(
                "selected_set",
                "select set to view in table",
                choices = names(rv$measures_of_interest)
              )
            ),
            column(width = 8, 
                   br(),
                   DT::dataTableOutput("set_summary")
                   )
          )
        )
      }
    }
    wellPanel(
      id = "sets_of_interest_panel", 
      class = "my_info_panel",
      p(class="section_header", "Sets of Interest"),
      p("These can be sets of genes/proteins etc. of special interest that can be highlighted on plots."),
      welltags
    )
  })
  
  output$set_info1 <- renderText({
    req(rv$measures_of_interest)
    n_sets <- length(rv$measures_of_interest)
    if(n_sets == 1) text <- " set available"
    else text <- " sets available"
    paste0(n_sets, text, " for this dataset. To add more, use the filter options.")
  })  
  
  output$set_info2 <- renderTable({
    req(rv$measures_of_interest)
    tibble::enframe(sapply(rv$measures_of_interest, nrow))
  }, colnames = FALSE)

#### set of interest table  ----   
  output$set_summary <- DT::renderDataTable({
    req(input$selected_set)
    req(rv$measures_of_interest)
    set_data <- rv$measures_of_interest[[input$selected_set]]
    
    DT::datatable(
      set_data,
      rownames = FALSE,
      options = list(
        dom = "tip",
        pageLength = 12
      )
    ) %>% 
      DT::formatStyle(0, target = 'row', `font-size` = '90%', lineHeight = '80%')
    
  })
  
  ### filter panel UI ----
  output$filter <- renderUI({
    if(!isTruthy(rv$dataset)) {
      wellPanel(class = "my_info_panel", p("Load dataset to see filter options.")) 
    } else {
      wellPanel(class = "my_info_panel", mod_name_filter_ui("name_filter"))
    }
  })

  ### filter module server ----
  
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
  
  
## plot tab ---- 
### histogram module ----    
  mod_histogramServer(
    "hist",
    data_to_plot = reactive(rv$long_data_tib),
    variables = reactive(names(rv$metadata$meta_summary))
  )

### heatmap module ----       
  mod_heatmap_server(
    "heatmap",
    dataset = reactive(rv$dataset),
    metadata = reactive(rv$metadata),
    sample_name_col = sample_names, # we should make sure that it's always called "sample_name"
    of_interest = reactive(rv$measures_of_interest),
    chosen_dataset = chosen_dataset
  )

### scatterplot module ----     
  mod_scatterplot_server(
    "scatter",
    data_to_plot = reactive(rv$long_data_tib),
    metadata = reactive(rv$metadata),
    sample_name_col = sample_names, # we should make sure that it's always called "sample_name"
    sets_of_interest = reactive(rv$measures_of_interest),
    chosen_dataset = chosen_dataset
  )

### violinplot module ----       
  mod_violinplot_server(
    "violinplot", 
    long_data_tib = reactive(rv$long_data_tib),
    metadata = reactive(rv$metadata),
    sample_name_col = sample_names # we should make sure that it's always called "sample_name"
  )

  observeEvent(input$browser, browser())
  
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")    
  shinyjs::show("app-content")

}


shinyApp(ui = ui, server = server)
