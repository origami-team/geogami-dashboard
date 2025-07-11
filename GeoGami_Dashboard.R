library(shiny)
library(shinythemes)
library(DT)
library(wordcloud2)
library(ggplot2)
library(dplyr)
library(leaflet)
library(bslib)
library(htmlwidgets)
iris$Species <- NULL

ui <- page_sidebar(
  title = div(
    style = "display: flex; align-items: center; gap: 20px;",
    tags$img(src = "https://geogami.ifgi.de/pictures/logo/icon.png", height = "60px"),
    tags$div(
      tags$h1("Welcome to the GeoGami dashboard!", style = "margin: 0; font-size: 24px;"),
      tags$a("app.geogami.ifgi.de", href = "https://app.geogami.ifgi.de/", style = "font-size: 14px; color: white;")
    )
  ),
  
  tags$head(
    tags$style(HTML('
    #selected_multiple_files-label + div div > .items {
      width: 80vw;
    }
    @media only screen and (min-width: 575px) {
      #selected_multiple_files-label + div div > .items {
        width: 20vw;
      }
    }
    @media only screen and (min-width: 750px) {
      #selected_multiple_files-label + div div > .items {
        width: 40vw;
      }
    }
    @media only screen and (min-width: 1100px) {
      #selected_multiple_files-label + div div > .items {
        width: 60vw;
      }
    }
    #selected_multiple_files-label + div div > .items {
      display: flex;
      flex-flow: row wrap;
      justify-content: flex-start;
      gap: 5px;
    }
    .item {
      word-wrap: anywhere;
    }
    pre {
      background-color: #F0F0F0 !important;
      color: #333 !important;
    }
   
    .bslib-page-sidebar .navbar {
      background: linear-gradient(90deg, rgb(7, 48, 59) 20%, rgb(12, 209, 232) 100%);
      min-height: 80px;
      padding-top: 10px;
      padding-bottom: 10px;
      padding-left: 20px;
    }

    .bslib-page-sidebar .navbar-brand {
      display: flex;
      align-items: center;
      gap: 15px;
    }

    .bslib-page-sidebar h1 {
      margin: 0;
      font-size: 25px;
      color: white; /* Make title text white */
    }

    .bslib-page-sidebar a {
      color: black; /* Optional: link color */
    }
    
    #inlineDiv {
      display: inline-block;
    }
  '))
  ),
  
  theme = bs_theme(),  # initially empty theme
  
  # Sidebar with collapsible toggle
  
  sidebar = sidebar(
    width = "400px",
    radioButtons("theme", "Choose Theme:",
                 choices = c("Light", "Dark"),
                 inline = TRUE,
                 selected = "Light"),
    
    #filter 1 - game selection
    div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 15px; border-radius: 8px;",
        selectizeInput(
          inputId = "selected_games",
          label = "Select your game:",
          choices = NULL,  # Leave it empty initially
        ),
    ),
    
    #filter 2 - JSON file selection
    div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 15px; border-radius: 8px;",
        selectizeInput(
          inputId = "selected_files",
          label = "Select the players:",
          choices = NULL,  # Leave it empty initially
          multiple = TRUE,
          options = list(
            placeholder = "Start typing to search...",
            plugins = list('remove_button'),
            onChange = I('
            function(value) {
              if (value.includes("ALL")) {
                // Select all files except "ALL"
                var allFiles = Object.keys(this.options).filter(k => k !== "ALL");
                this.setValue(allFiles);            
                this.removeOption("ALL");           
              }
            }
          ')
          )
        ),
        actionButton("reset", "Reset Selection", icon = icon("refresh"), style = "margin-top: 10px; margin-bottom: 15px;"),
    ),
    
    
    #filter 2 - ID - 2nd div
    div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 15px; border-radius: 8px;",
        numericInput("num_value", "Enter a task number:", value = 1, min = 1, max = 100)
    ),
  ),