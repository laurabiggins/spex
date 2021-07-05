# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

library(shiny)
library(magrittr)

data_location <- "inst/extdata/"
folders <- basename(list.dirs(data_location))
available_datasets <- folders[folders != "extdata"]

data_folder <- paste0("inst/extdata/", "worm_femExpression", "/")
#data_folder <- paste0("inst/extdata/", "oximouse", "/")

dataset  <- readRDS(paste0(data_folder, "dataset.rds"))
meta1  <- readRDS(paste0(data_folder, "metadata.rds"))
of_interest  <- readRDS(paste0(data_folder, "of_interest.rds"))

sample_names <- "sample_name"
measure_names <- rownames(dataset)
bab_light_blue <- "#00aeef"
bab_dark_blue <- "#1d305f"

meta_factors <- meta1$meta_all %>%
 dplyr::mutate_if(is.character, factor) %>%
 dplyr::mutate_if(is.double, factor) %>%
 dplyr::mutate_if(is.integer, factor)

tib <- tibble::as_tibble(dataset, rownames = "row_attribute")
long_data_tib <- tidyr::pivot_longer(tib, cols = -row_attribute, names_to = sample_names) %>%
  tidyr::drop_na() %>%
  dplyr::left_join(meta_factors)


ui <- tagList(
  
  #bootstrapDep,

  fluidPage(
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    theme = bslib::bs_theme(
      bg = bab_dark_blue,
      fg = "white",
      primary = bab_light_blue,
      secondary = bab_light_blue
    ),
    titlePanel(
      tags$img(
        src = "images/BI_logo_grey.png", 
        style="margin-top: -10px; padding-right:10px; padding-bottom:10px", 
        width = "80", 
        height = "85",
        align = "right"
      ),
      windowTitle="spex"
    ),
    br(),
    #titlePanel("Dataset title"),
    tabsetPanel(
      id = "main_panels",
      tabPanel(
        "info",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput(
              inputId = "choose_dataset",
              label = NULL, #"Choose dataset",
              choices = c("choose dataset", available_datasets),
            ),
            br(),
            actionButton(inputId = "load_data", label = "load dataset"),
            p("Explore your chosen dataset by using the tabs above."),
            p("Sample names and experimental conditions are shown in the metadata section."),
            p("The data tab shows the whole dataset, which can be downloaded if required."),
            p("A range of plots can be viewed and downloaded to explore different aspects of the dataset.")
          ),
          mainPanel(
            br(),
            h1(textOutput(outputId = "dataset_name"), align = "center"),
            br(),
            h5(textOutput(outputId = "dataset_info")),
            br(),br(),br(),
            br(),br(),br(),br(),br(),br(),br(),
            h6("For more information about work carried out at the Babraham Institute
                 visit the", a(href= "https://www.babraham.ac.uk/", "website")),
            br(),br(),br(),br()
          )
        )  
      ),  
      tabPanel(
        "metadata",
        br(),
        fluidRow(
          column(
            width = 6,
            wellPanel(
              h3("Dataset summary", align = "center", style="margin: 10px;"),
              textOutput("meta_info1"),
              textOutput("meta_info2"),
              h6("Number of categories in each variable:"),
              tableOutput("meta_info3"),
              checkboxInput("show_meta_summary", "show more information on variables"),
              conditionalPanel(
                condition = "input.show_meta_summary == 1",
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      "selected_condition",
                      "select condition",
                      choices = names(meta1$meta_sum)
                    ),
                  ),
                  column(width = 6, tableOutput("meta_summary"))
                )
              ),
              checkboxInput("show_meta", "show all metadata"),
              conditionalPanel(
                condition = "input.show_meta == 1",
                DT::dataTableOutput("meta_table")
              ),
              actionButton("browser", "browser")
            )
          ),
          column(
            width = 6,
            wellPanel(
              h3("Sets of interest", align = "center", style="margin: 10px;"),
              textOutput("set_info1"),
              h6("Number in each set:"),
              tableOutput("set_info2"),
              checkboxInput("show_sets", "show items in set"),
              conditionalPanel(
                condition = "input.show_sets == 1",
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      "selected_set",
                      "select set",
                      choices = names(of_interest)
                    )
                  ),
                  column(width = 6, tableOutput("set_summary"))
                )
              )
            )
          )  
        ),
        br(),
        br()
      ),
      tabPanel(
        "data",
        br(),
        wellPanel(DT::dataTableOutput("data_table"))
      ),
      tabPanel(
        "plot",
        br(),
        navlistPanel(
          "plot type",
          tabPanel("histogram", mod_histogramUI("hist", meta1$meta_sum)),
          tabPanel("scatterplot", mod_scatterplot_ui("scatter", meta1$meta_sum, of_interest)),
          tabPanel("heatmap", mod_heatmap_ui("heatmap", meta1$meta_sum)),
          tabPanel("violinplot", mod_violinplot_ui("violinplot")),
          widths = c(3,9)
        )
      ),
      tabPanel(
        "filter",
        br(),
        mod_name_filter_ui("name_filter", measure_names)
      )
    ),
    br(),
    fluidRow(
      column(
        width = 3,
        tags$img(src = "images/bioinformatics_logo_small_grey.png", width = "200", height = "71")
      ),
      column(
        width = 6,
        offset = 3,
        br(),
        br(),
        p("Any problems please email laura.biggins@babraham.ac.uk", style = "font-size:12px", align = "right")
      )  
    ),
    br()
  )
)


server <- function(input, output, session ) {
  

  long_data_tib <- reactiveVal(long_data_tib)
  
  measure_names <- reactiveVal(measure_names)
  
  measures_of_interest <- reactiveVal(of_interest)
  
  metadata <- reactiveVal(meta1)
  

  data_loaded <- reactiveVal(FALSE)
  
  chosen_dataset <- eventReactive(input$load_data, input$choose_dataset)

    
  rv <- reactiveValues(
    dataset = NULL, # the currently loaded dataset
    long_data_tib = NULL, # tidied long version of dataset
    measure_names = NULL, # all the row (gene) names
    measures_of_interest = NULL, # sets of ids of interest, can be loaded upfront and added to by filtering
    metadata = NULL # list of accompanying metadata
  )
    
  observeEvent(input$load_data, {
    
    if(input$choose_dataset != "choose dataset") {
    
      # need checks here that the locations exist
      
      data_folder <- paste0(data_location, input$choose_dataset, "/")
      
      dataset  <- readRDS(paste0(data_folder, "dataset.rds"))
      metadata_processed  <- readRDS(paste0(data_folder, "metadata.rds"))
      of_interest  <- readRDS(paste0(data_folder, "of_interest.rds"))
      
      metadata(metadata_processed)
      measures_of_interest(of_interest)
      measure_names(rownames(dataset))
  
      rv$dataset <- dataset
      rv$metadata <- metadata_processed
      rv$measure_names <- rownames(dataset)
      rv$measures_of_interest <- of_interest
      
      meta_factors <- metadata_processed$meta_all %>%
        dplyr::mutate_if(is.character, factor) %>%
        dplyr::mutate_if(is.double, factor) %>%
        dplyr::mutate_if(is.integer, factor)
      
      tib <- tibble::as_tibble(dataset, rownames = "row_attribute")
      long_data_tibble <- tib %>% 
        tidyr::pivot_longer(cols = -row_attribute, names_to = sample_names) %>%
        tidyr::drop_na() %>%
        dplyr::left_join(meta_factors)
      
      long_data_tib(long_data_tibble)
      rv$long_data_tib <- long_data_tibble
      
      # updateSelectInput(
      #   inputId = "selected_set",
      #   label = "select set",
      #   choices = names(of_interest)
      # )
    }
  })
  
  output$dataset_name <- renderText({
    chosen_dataset()
  })
  
  output$dataset_info <- renderText({
    
    if(chosen_dataset() == "choose dataset") {
      "Choose a dataset from the dropdown on the left"
    } else {
      "populate this with an info file"
    }
    
  })
  
  
  # Data tab - the main dataset
  output$data_table <- DT::renderDataTable({
    
    req(rv$dataset)
    dt_setup(rv$dataset, n_rows = 20, dom_opt = "ftlip", show_rownames = TRUE)
  })
  
  output$meta_info1 <- renderText({
    paste0("The dataset contains ", nrow(metadata()$meta_summary[[sample_names]]), " samples.")
  })
  
  output$meta_info2 <- renderText({
    paste0(
      "Variables are: ", 
      paste0(
        names(metadata()$meta_summary)[!names(metadata()$meta_summary) %in% metadata()$sample_name], 
        collapse = ", "
      ),
      "."
    )
  })
  
  output$meta_info3 <- renderTable({
    tibble::enframe(sapply(metadata()$meta_summary, nrow))
  }, colnames = FALSE)
  
  output$set_info1 <- renderText({
    n_sets <- length(measures_of_interest())
    if(n_sets == 1) text <- " set available"
    else text <- " sets available"
    paste0(n_sets, text, ". To add more, use the filter tab.")
  })  
  
  output$set_info2 <- renderTable({
    tibble::enframe(sapply(measures_of_interest(), nrow))
  }, colnames = FALSE)
  
  output$set_summary <- renderTable({
    req(input$selected_set)
    req(measures_of_interest())
    measures_of_interest()[[input$selected_set]]
  })
  
  output$meta_table <- DT::renderDataTable(dt_setup(metadata()$meta_all, n_rows = 20))
  
  output$meta_summary <- renderTable({
    req(input$selected_condition)
    req(metadata()$meta_summary)
    metadata()$meta_summary[[input$selected_condition]]
  })
  
  
  mod_histogramServer(
    "hist",
    data_to_plot = long_data_tib,
    meta = metadata,
    chosen_dataset
  )
  
  #mod_heatmap_server("heatmap", data_values, metadata()$meta_summary, metadata()$meta_all, 
   #                  sample_name_col = sample_names, of_interest = of_interest)
  
  mod_scatterplot_server(
    "scatter",
    long_data_tib = long_data_tib,
    metadata = metadata,
    sample_name_col = sample_names,
    sets_of_interest = measures_of_interest,
    chosen_dataset = chosen_dataset
  )
  
  mod_violinplot_server(
    "violinplot", 
    #long_data_tib = long_data_tib,
    long_data_tib = rv$long_data_tib,
    chosen_dataset = chosen_dataset, 
    metadata = rv$metadata,
    sample_name_col = sample_names
  )
  
  filter_results <- mod_name_filter_server(
    "name_filter", 
    measure_names, 
    of_interest = measures_of_interest,
    chosen_dataset = chosen_dataset
  )
  
  observeEvent(filter_results(), {
    
    measures_of_interest(filter_results())
    print("filter results updated")
    updateSelectInput(inputId = "selected_set", choices = names(measures_of_interest()))
  })
  
  observeEvent(input$browser, browser())
}


shinyApp(ui = ui, server = server)
