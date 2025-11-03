library(shiny)
library(tidyverse)
library(DT)
library(rlang)
library(scales)
library(shinycssloaders)
library(bslib)

# Helpers
num_vars_default <- c("Price", "Rooms", "Distance", "Landsize", "YearBuilt", "Propertycount")
cat_vars_default <- c("Type", "Regionname", "Method", "SellerG")

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