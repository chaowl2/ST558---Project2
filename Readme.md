# Melbourne Housing Explorer (Shiny)


An interactive Shiny app to explore the **Melbourne Housing Market** dataset. Use the sidebar to subset by region and dwelling type, set numeric ranges (e.g., price), then explore the data via tables and plots. You can also download the (possibly subsetted) data.

**Live app:** https://chaowl.shinyapps.io/project2/

## Data Source
- Kaggle: *Melbourne Housing Market* (credit: Anthony Pino)

## Features
- Sidebar filters with **Apply Filters** button (two categorical + two numeric via dynamic sliders)
- Tabs: **About**, **Data Download**, **Data Exploration** (categorical summaries, numeric summaries, plots)
- At least six multivariate-ready plots including a **heatmap** and **faceting** support

## Getting Started
1. Download the dataset from Kaggle and save as `data/Melbourne_housing_FULL.csv`.
2. Open RStudio in this repo and run `renv::init()` (optional) or install packages below.
3. `shiny::runApp()` locally.
4. Deploy to shinyapps.io via the **Publish** button in RStudio.

## Packages
`shiny`, `tidyverse`, `DT`, `shinycssloaders`, `bslib`, `scales`, `rlang`