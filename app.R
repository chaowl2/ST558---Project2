library(shiny)
library(tidyverse)
library(DT)
library(rlang)
library(scales)
library(shinycssloaders)
library(bslib)

# Helpers
num_vars_default <- c("Price", "Rooms", "Distance", "Landsize", "Propertycount")
cat_vars_default <- c("Type", "Regionname", "Method", "SellerG", "Postcode")

read_mh <- function(path = "MELBOURNE_HOUSE_PRICES_LESS.CSV") {
  req(file.exists(path))
  raw <- readr::read_csv(path, show_col_types = FALSE)
  raw %>% mutate(
    Date = suppressWarnings(lubridate::dmy(Date)),
    Type = as.factor(Type),
    Regionname = as.factor(Regionname),
    Suburb = as.factor(Suburb)
  )
}

# UI

ui <- page_sidebar(
  title = "Melbourne Housing Explorer",
  sidebar = sidebar(
    h3("Subset Filters"),
    helpText("Choose categories, pick numeric variables, then click Apply Filters."),
    
    
    # Categorical filters (two)
    selectizeInput("type", "Dwelling Type (Type)", choices = NULL, multiple = TRUE, options = list(placeholder = "All")),
    selectizeInput("region", "Region (Regionname)", choices = NULL, multiple = TRUE, options = list(placeholder = "All")),
    
    
    # Numeric variable choices (two), then dynamic sliders
    selectInput("num1", "Numeric var 1", choices = NULL),
    uiOutput("num1_ui"),
    selectInput("num2", "Numeric var 2", choices = NULL),
    uiOutput("num2_ui"),
    
    
    actionButton("apply", "Apply Filters", class = "btn-primary"),
    width = 350
  ),
  
  # Main content with tabs
  navset_tab(
    id = "tabs",
    
    
    nav_panel("About",
              div(class = "p-4",
                  h2("About this App"),
                  p("Explore the Melbourne Housing Market dataset. Use the sidebar to subset by dwelling type, region, and numeric ranges, then view tables and plots."),
                  tags$ul(
                    tags$li(strong("Data:"), " Kaggle – Melbourne Housing Market"),
                    tags$li(strong("Tabs:"), " Data Download (table + CSV download), Data Exploration (summaries + plots)")
                  ),
                  tags$img(src = "melbourne.jpg", width = "100%", style = "max-width:720px; border-radius:12px; margin-top:12px;"),
                  tags$hr(),
                  h3("Using the Sidebar"),
                  tags$ol(
                    tags$li("Choose Type and Region filters (or leave empty for all)."),
                    tags$li("Pick up to two numeric variables; sliders appear so you can set ranges."),
                    tags$li("Click ", strong("Apply Filters"), " to update the app.")
                  )
              )
    ),
    
    nav_panel("Data Download",
              div(class = "p-3",
                  h3("Filtered Data"),
                  withSpinner(DTOutput("tbl")),
                  br(),
                  downloadButton("dl", "Download CSV")
              )
    ),
    
    
    nav_panel("Data Exploration",
              div(class = "p-3",
                  navset_card_pill(
                    nav_panel("Categorical Summaries",
                              fluidRow(
                                column(4,
                                       selectInput("cat1", "Categorical (rows)", choices = NULL),
                                       selectInput("cat2", "Categorical (columns)", choices = c(None = "(none)"), selected = "(none)")
                                ),
                                column(8,
                                       h4("Counts / Proportions"),
                                       withSpinner(tableOutput("cat_table"))
                                )
                              )
                    ),
                    nav_panel("Numeric Summaries",
                              fluidRow(
                                column(4,
                                       selectInput("num_summary", "Numeric variable", choices = NULL),
                                       selectInput("group_var", "Group by (categorical)", choices = NULL)
                                ),
                                column(8,
                                       withSpinner(tableOutput("num_table"))
                                )
                              )
                    ),
                    nav_panel("Plots",
                              fluidRow(
                                column(4,
                                       selectInput("plot_type", "Plot type", choices = c(
                                         "Histogram" = "hist",
                                         "Boxplot" = "box",
                                         "Scatter" = "scatter",
                                         "Faceted Scatter" = "f_scatter",
                                         "Heatmap (median price by Region x Type)" = "heat"
                                       )),
                                       uiOutput("plot_ui")
                                ),
                                column(8,
                                       withSpinner(plotOutput("plot", height = 480))
                                )
                              )
                    )
                  )
              )
    )
  ),
  theme = bs_theme(version = 5, bootswatch = "flatly")
)

# Server
server <- function(input, output, session) {
  mh <- read_mh()
  
  
  # Initialize select choices
  updateSelectizeInput(session, "type", choices = sort(unique(na.omit(mh$Type))), server = TRUE)
  updateSelectizeInput(session, "region", choices = sort(unique(na.omit(mh$Regionname))), server = TRUE)
  
  
  num_vars <- intersect(num_vars_default, names(mh))
  cat_vars <- intersect(cat_vars_default, names(mh))
  
  
  updateSelectInput(session, "num1", choices = num_vars, selected = num_vars[1])
  updateSelectInput(session, "num2", choices = num_vars, selected = ifelse(length(num_vars) > 1, num_vars[2], num_vars[1]))
  
  
  updateSelectInput(session, "cat1", choices = cat_vars, selected = cat_vars[1])
  updateSelectInput(session, "cat2", choices = c("(none)", cat_vars), selected = "(none)")
  updateSelectInput(session, "num_summary", choices = num_vars, selected = num_vars[1])
  updateSelectInput(session, "group_var", choices = cat_vars, selected = cat_vars[1])
  
  
  # Dynamic sliders for numeric vars based on full data range
  make_slider <- function(var) {
    rng <- range(mh[[var]], na.rm = TRUE)
    sliderInput(paste0(var, "_rng"), paste0(var, " range"), min = floor(rng[1]), max = ceiling(rng[2]), value = rng, step = diff(rng)/200)
  }
  
  
  output$num1_ui <- renderUI({ req(input$num1 %in% names(mh)); make_slider(input$num1) })
  output$num2_ui <- renderUI({ req(input$num2 %in% names(mh)); make_slider(input$num2) })
  
  
  # ReactiveValues to hold the filtered data; only update on Apply
  rv <- reactiveValues(data = mh)
  
  
  observeEvent(input$apply, ignoreInit = TRUE, {
    d <- mh
    
    
    # categorical filters
    if (length(input$type)) d <- d %>% filter(is.na(Type) | Type %in% input$type)
    if (length(input$region)) d <- d %>% filter(is.na(Regionname) | Regionname %in% input$region)
    
    
    # numeric filters
    if (!is.null(input$num1) && !is.null(input[[paste0(input$num1, "_rng")]])) {
      rng <- input[[paste0(input$num1, "_rng")]]
      d <- d %>% filter(is.na(.data[[input$num1]]) | between(.data[[input$num1]], rng[1], rng[2]))
    }
    if (!is.null(input$num2) && !is.null(input[[paste0(input$num2, "_rng")]])) {
      rng <- input[[paste0(input$num2, "_rng")]]
      d <- d %>% filter(is.na(.data[[input$num2]]) | between(.data[[input$num2]], rng[1], rng[2]))
    }
    
    
    # Handle empty result safely
    if (nrow(d) == 0) {
      showNotification("No rows match the selected filters. Showing original data.", type = "error")
      rv$data <- mh
    } else {
      rv$data <- d
      showNotification(paste("Rows after filtering:", scales::comma(nrow(d))), type = "message")
    }
  })
# Data Download tab 
output$tbl <- renderDT({
  datatable(rv$data, options = list(scrollX = TRUE, pageLength = 10))
})


output$dl <- downloadHandler(
  filename = function() paste0("melbourne_filtered_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
  content = function(file) readr::write_csv(rv$data, file)
)

# Categorical summaries
output$cat_table <- renderTable({
  req(input$cat1 %in% names(rv$data))
  d <- rv$data
  
  
  if (isTruthy(input$cat2) && input$cat2 != "(none)") {
    validate(need(input$cat2 != input$cat1, "Choose two different categorical variables."))
    d %>% count(.data[[input$cat1]], .data[[input$cat2]], name = "n") %>%
      group_by(.data[[input$cat1]]) %>% mutate(pct_row = n/sum(n)) %>% ungroup()
  } else {
    d %>% count(.data[[input$cat1]], name = "n") %>% mutate(pct = n/sum(n))
  }
})


# Numeric summaries
output$num_table <- renderTable({
  req(input$num_summary %in% names(rv$data), input$group_var %in% names(rv$data))
  rv$data %>% group_by(.data[[input$group_var]]) %>%
    summarise(
      n = dplyr::n(),
      mean = mean(.data[[input$num_summary]], na.rm = TRUE),
      median = median(.data[[input$num_summary]], na.rm = TRUE),
      sd = sd(.data[[input$num_summary]], na.rm = TRUE),
      IQR = IQR(.data[[input$num_summary]], na.rm = TRUE),
      .groups = "drop"
    ) %>% arrange(desc(n))
})
# Plot UI 
output$plot_ui <- renderUI({
  d <- rv$data
  num_choices <- names(d)[map_lgl(d, is.numeric)]
  cat_choices <- names(d)[map_lgl(d, ~is.factor(.x) || is.character(.x))]
  
  
  switch(input$plot_type,
         hist = tagList(
           selectInput("p_num", "Numeric", choices = num_choices, selected = intersect(c("Price"), num_choices)[1]),
           selectInput("p_fill", "Fill (categorical)", choices = c(None = "(none)", cat_choices), selected = intersect(c("Type"), cat_choices)[1])
         ),
         box = tagList(
           selectInput("p_y", "Y (numeric)", choices = num_choices, selected = intersect(c("Price"), num_choices)[1]),
           selectInput("p_x", "X (categorical)", choices = cat_choices, selected = intersect(c("Type"), cat_choices)[1])
         ),
         scatter = tagList(
           selectInput("p_xn", "X (numeric)", choices = num_choices, selected = intersect(c("Distance","Rooms"), num_choices)[1]),
           selectInput("p_yn", "Y (numeric)", choices = num_choices, selected = intersect(c("Price"), num_choices)[1]),
           selectInput("p_col", "Color (categorical)", choices = c(None = "(none)", cat_choices), selected = intersect(c("Type"), cat_choices)[1])
         ),
         f_scatter = tagList(
           selectInput("p_xn2", "X (numeric)", choices = num_choices, selected = intersect(c("Rooms"), num_choices)[1]),
           selectInput("p_yn2", "Y (numeric)", choices = num_choices, selected = intersect(c("Price"), num_choices)[1]),
           selectInput("p_col2", "Color (categorical)", choices = c(None = "(none)", cat_choices), selected = intersect(c("Type"), cat_choices)[1]),
           selectInput("p_facet", "Facet (categorical)", choices = cat_choices, selected = intersect(c("Regionname"), cat_choices)[1])
         ),
         heat = tagList(
           helpText("Median Price by Region x Type"),
           p("Variables are fixed to Regionname (rows), Type (columns), median(Price).")
         )
  )
})

# Plot render
output$plot <- renderPlot({
  d <- rv$data
  req(nrow(d) > 0)
  
  
  switch(input$plot_type,
         hist = {
           req(input$p_num)
           g <- ggplot(d, aes(x = .data[[input$p_num]])) + geom_histogram(bins = 40)
           if (!is.null(input$p_fill) && input$p_fill != "(none)") g <- g + aes(fill = .data[[input$p_fill]])
           g + scale_x_continuous(labels = label_number_si()) + labs(x = input$p_num, y = "Count")
         },
         box = {
           req(input$p_x, input$p_y)
           ggplot(d, aes(x = .data[[input$p_x]], y = .data[[input$p_y]], fill = .data[[input$p_x]])) +
             geom_boxplot(outlier.alpha = 0.2) + scale_y_continuous(labels = label_number_si()) +
             labs(x = input$p_x, y = input$p_y)
         },
         scatter = {
           req(input$p_xn, input$p_yn)
           g <- ggplot(d, aes(x = .data[[input$p_xn]], y = .data[[input$p_yn]])) + geom_point(alpha = 0.5)
           if (!is.null(input$p_col) && input$p_col != "(none)") g <- g + aes(color = .data[[input$p_col]])
           g + scale_y_continuous(labels = label_number_si()) + labs(x = input$p_xn, y = input$p_yn)
         },
         f_scatter = {
           req(input$p_xn2, input$p_yn2, input$p_facet)
           g <- ggplot(d, aes(x = .data[[input$p_xn2]], y = .data[[input$p_yn2]])) + geom_point(alpha = 0.4)
           if (!is.null(input$p_col2) && input$p_col2 != "(none)") g <- g + aes(color = .data[[input$p_col2]])
           g + facet_wrap(as.formula(paste("~", input$p_facet))) + scale_y_continuous(labels = label_number_si()) +
             labs(x = input$p_xn2, y = input$p_yn2)
         },
         heat = {
           d %>% group_by(Regionname, Type) %>% summarise(med_price = median(Price, na.rm = TRUE), .groups = "drop") %>%
             ggplot(aes(x = Type, y = Regionname, fill = med_price)) + geom_tile() +
             scale_fill_continuous(labels = label_number_si()) +
             labs(x = "Type", y = "Region", fill = "Median Price")
         }
  )
})

}


shinyApp(ui, server)