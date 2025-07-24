# version 1.0.2
library(shiny)
library(shinythemes)
library(DT)
library(wordcloud2)
library(ggplot2)
library(dplyr)
library(leaflet)
library(bslib)
library(htmlwidgets)
library(httr)
library(jsonlite)

iris$Species <- NULL

# Function to fetch/load list of user games / games that user has access to their tracks
fetch_games_data_from_server <- function(apiUrl, token) {
  message("Fetched data, now processing...")
  # Adds a space between 'Bearer' and the token
  auth_header <- paste("Bearer", token)  

  # Make the request with Authorization header
  res <- GET(apiUrl, add_headers(Authorization = auth_header))

  # Handle the response
  if (status_code(res) == 200) {
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    if (length(data) == 0) {
      return(NULL)  # No content found
    }
    # return the content of the response
    message("Finished fetching data... m")
    return(data$content)
  } else {
    if (status_code(res) == 401) {
      warning("Unauthorized: Invalid or expired token.")
    } else {
      warning(paste("Failed to fetch games. Status code:", status_code(res)))
    }
    return(NULL)
  }
}

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
  
  # Main tabs
  tabsetPanel(
    tabPanel('All of your tasks', uiOutput("file_selector_ui"),
             div(id = "inlineDiv", style = "margin-bottom: 20px; border: 1px solid #ccc; padding: 10px; border-radius: 5px; background-color: #f9f9f9;",
                  h5(textOutput("player_name"), textOutput("overall_score"), style = "margin-bottom: 5px")), 
             DTOutput('iris_data'),
             div(style = "border: 0px solid #ccc; padding: 10px; margin-top: 15px; border-radius: 8px;",
                 downloadButton('save_data', 'Save to csv'))),
    tabPanel('Map', h3("Maps"), card(uiOutput("file_selector_ui3"), textOutput("mapLegend"), div(id="map", leafletOutput("map"),  style = "margin-top: 5px"),
             div(style = "border: 0px solid #ccc; padding: 10px; margin-top: 15px; border-radius: 8px;",
                 downloadButton('downloadMap','Save the map')), full_screen = TRUE)),
    tabPanel('Pictures', h3("Uploaded Photos"),
             card(uiOutput("file_selector_ui4"),
               uiOutput("photo_display"),
               full_screen = TRUE
             )
    ),
    tabPanel('All of your plays', uiOutput("file_selector_ui1"), textOutput("tabLegend"),
             conditionalPanel(condition = "output.tabLegend == 'Type task: Navigation to flag' || output.tabLegend == 'Type task: Navigation with arrow' || output.tabLegend == 'Type task: Navigation via text' || output.tabLegend == 'Type task: Navigation via photo'",
                              card(h4("Route length versus time"), tableOutput('cmp_table1'), downloadButton('save_table1', 'Save to csv'), style = "margin-top: 10px"),
                              ),
             conditionalPanel(condition = "output.tabLegend == 'Type task: Direction determination'",
                              card(h4("Answer and error for direction task"), tableOutput('cmp_table2'), downloadButton('save_table2', 'Save to csv'), style = "margin-top: 10px")
                              ),
             ),
    tabPanel('Statistics per task', h3("Statistics"),uiOutput("file_selector_ui2"), textOutput("graphLegend"),
             #if the task category is navigation
             conditionalPanel(condition = "output.graphLegend == 'Type task: Navigation to flag' || output.graphLegend == 'Type task: Navigation with arrow' || output.graphLegend == 'Type task: Navigation via text' || output.graphLegend == 'Type task: Navigation via photo'",
                              selectInput(
                                inputId = "graph_filter",
                                label = "Choose graphic to display:",
                                choices = c("Time VS Distance","Answer & Error"),
                                selected = c("Answer & Error")
                              ), conditionalPanel(
                                condition = "input.graph_filter == 'Answer & Error'",
                                card(card_header("Pie chart") , full_screen = TRUE, plotOutput('pie_chart'), 
                                     div(style = "border: 0px solid #ccc; padding: 10px; margin-top: 15px; border-radius: 8px;",
                                     downloadButton('save_picture','Save to png')))
                              ), conditionalPanel(
                                condition = "input.graph_filter == 'Time VS Distance'",
                                card(card_header("Time vs distance scatter plot"), full_screen = TRUE, plotOutput('time_chart'), 
                                     div(style = "border: 0px solid #ccc; padding: 10px; margin-top: 15px; border-radius: 8px;",
                                     downloadButton('save_time_chart','Save to png')))
                              )
                              ),
             #else, for the other tasks
             conditionalPanel(condition = "output.graphLegend == 'Type task: Direction determination' || output.graphLegend == 'Type task: Free' || output.graphLegend == 'Type task: Self location' || output.graphLegend == 'Type task: Object location'",
                              selectInput(
                                inputId = "graph_filter2",
                                label = "Choose graphic to display:",
                                choices = c("Answer & Error"),
                                selected = c("Answer & Error")
                              ), conditionalPanel(
                                condition = "input.graph_filter2 == 'Answer & Error'",
                                card(card_header("Time vs distance scatter plot"), full_screen = TRUE, plotOutput('pie_chart2'),
                                    div(style = "border: 0px solid #ccc; padding: 10px; margin-top: 15px; border-radius: 8px;",
                                    downloadButton('save_picture2','Save to png')))
                              ),
                              ),
            )
  )
)


server <- function(input, output, session) {

  # Store selected game track data reactively
  selected_game_tracks_rv <- reactiveVal()
  # Store access token reactively
  accessToken_rv <- reactiveVal()

  # Observe the URL query string for the token parameter
  observe({
    query <- parseQueryString(session$clientData$url_search)
    tokenParam <- query[["token"]]
    accessToken_rv(tokenParam)
  })

    # Theme options
  observe({
    if (input$theme == "Dark") {
      session$setCurrentTheme(bs_theme(bootswatch = "solar"))
    } else {
      session$setCurrentTheme(bs_theme(bootswatch = "flatly"))
    }
  })

    output$text <- renderText({
    paste("Current theme is:", input$theme)
  })
  
  observe({
    ## 1. Load list of user games / games that user has access to their tracks
    # Define the API URL and token
    apiUrl <- "https://api.geogami.ifgi.de/game/usergames"
    games_data <- fetch_games_data_from_server(apiUrl, accessToken_rv())
    games_name <- games_data$name
    if (is.null(games_data)) {
      games_name <- character(0)
      games_id <- character(0)
    } else {
      games_name <- games_data$name
      games_id <- games_data[["_id"]]
    }

    ### 2. Populate select input for games
    updateSelectizeInput(session, "selected_games",
                          choices = setNames(games_id, games_name),
                          server = TRUE)
  })
  
  ### 3. When a game is selected
  observeEvent(input$selected_games, {
    game_id <- input$selected_games

    # update the API URL with the selected game ID
    apiUrl <- paste0("https://api.geogami.ifgi.de/track/gametracks/", game_id)
    
    # Fetch game's tracks data from API
    # Note: The token is used for authentication, ensure it is valid
    games_tracks <- fetch_games_data_from_server(apiUrl, accessToken_rv())

    # Store in reactive value
    selected_game_tracks_rv(games_tracks)
  })
  
  ### 4. Update file selector when data changes
  observe({
    tracks_data <- selected_game_tracks_rv()
    
    if (!is.null(tracks_data)) {
      # Use meaningful labels for the UI: e.g., "Player Name - Date"
      choices <- setNames(
        tracks_data[["_id"]],
        paste0(tracks_data$players, " - ", tracks_data$createdAt)
      )
      
      updateSelectizeInput(session, "selected_files",
                          choices = choices,
                          server = TRUE)
    }
  })

  ### 5. Reset file selector when reset button clicked
  observeEvent(input$reset, {
    tracks_data <- selected_game_tracks_rv()
    
    if (!is.null(tracks_data)) {
      choices <- setNames(
        tracks_data[["_id"]],
        paste0(tracks_data$players, " - ", tracks_data$createdAt)
      )
      
      updateSelectizeInput(session, "selected_files",
                          choices = choices,
                          selected = NULL,
                          server = TRUE)
    }
  })

  # 6. Select single file to view
  output$file_selector_ui <- renderUI({
    req(input$selected_files)

    selectInput("selected_data_file",
                "Choose file to view data:",
                choices = input$selected_files,
                selected = input$selected_files[1])
  })
  
  ### 7. Reactive: load selected single file data
  loaded_json <- reactive({
    req(input$selected_data_file)
    selected <- input$selected_data_file

    lapply(selected, function(file) {
      track_id <- input$selected_data_file
      # Construct the API URL
      api_url <- paste0("https://api.geogami.ifgi.de/track/", track_id)
      # Fetch and return the JSON data from the server
      track_data <- fetch_games_data_from_server(api_url, accessToken_rv())
      return(track_data)
    })
  })

  ### 8. Reactive: load multiple json files for comparison
  load_multiple <- reactive({
    req(input$selected_multiple_files)

    # If multiple IDs are found, use only the last one
    sel <- input$selected_multiple_files[length(input$selected_multiple_files)]

    lapply(sel, function(file) {
      track_id <- sel
      # Construct the API URL
      api_url <- paste0("https://api.geogami.ifgi.de/track/", track_id)
      # Fetch and return the JSON data from the server
      track_data <- fetch_games_data_from_server(api_url, accessToken_rv())
      return(track_data)
    })
  })

  ### 9. UI: multiple file selector for comparison (tables, graphics, maps, photos)
  output$file_selector_ui1 <- renderUI({
    req(input$selected_files)

    selectInput("selected_multiple_files", 
                "Choose files to compare (Table):", 
                choices = input$selected_files,
                selected = input$selected_files,
                multiple = TRUE)
  })
  
  ##### Filters for comparing Graphics starts
  output$file_selector_ui2 <- renderUI({
    req(input$selected_files)
    
    selectInput("selected_multiple_files", 
                "Choose file to view data:", 
                choices = input$selected_files,
                selected = input$selected_files,
                multiple = TRUE, selectize = TRUE)
  })
  
  ##### Filter for maps
  output$file_selector_ui3 <- renderUI({
    req(input$selected_files)
    
    selectInput("selected_data_file",
                "Choose file to view data:",
                choices = input$selected_files,
                selected = input$selected_files[1])
  })
  
  ##### Filter for photos
  output$file_selector_ui4 <- renderUI({
    req(input$selected_files)
    
    selectInput("selected_data_file",
                "Choose file to view data:",
                choices = input$selected_files,
                selected = input$selected_files[1])
  })
  
  #####Big table code
  df_react <- reactiveVal()

observeEvent(req(input$selected_data_file, input$num_value), {
  req(input$num_value != 0 && input$num_value > 0)
  
  data <- loaded_json()  # load selected JSON
  
  if (is.null(data) || length(data) == 0) {
    showNotification("No data found for selected file.", type = "error")
    return()
  }

  # Defensive check: ensure expected structure exists
  if (!("events" %in% names(data[[1]])) || !("task" %in% names(data[[1]]$events))) {
    showNotification("Unexpected data format.", type = "error")
    return()
  }

  id <- data[[1]]$events$task[["_id"]]
  typ <- list()
  cons <- list()
  ans <- list()

  # Extract reusable fields
  csg <- data[[1]]$events$task$question$text
  ev <- data[[1]]$events$type
  pict_quest <- data[[1]]$events$task$question$photo
  ans_type <- data[[1]]$events$task$answer$type
  
  for (j in seq_len(length(id) - 1)) {
    if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
      correct_flag <- data[[1]]$events$answer$correct[j]
      
      ans_value <- NA  # Default

      if (ans_type[j] == "TEXT") {
        ans_text <- data[[1]]$events$answer$text[j]
        if (!is.na(correct_flag)) {
          ans_value <- paste(ifelse(correct_flag == "TRUE", "Correct", "Incorrect"), ans_text)
        }
      } else if (ans_type[j] == "MULTIPLE_CHOICE_TEXT") {
        choice_val <- data[[1]]$events$answer$selectedChoice$value[j]
        if (!is.na(correct_flag)) {
          ans_value <- paste(ifelse(correct_flag == "TRUE", "Correct", "Incorrect"), choice_val)
        }
      } else if (ans_type[j] == "NUMBER") {
        num_val <- data[[1]]$events$answer$numberInput[j]
        if (!is.na(correct_flag)) {
          ans_value <- paste(ifelse(correct_flag == "TRUE", "Correct", "Incorrect"), num_val)
        }
      } else {
        # Fallback for unknown types
        if (!is.null(correct_flag)) {
          ans_value <- ifelse(correct_flag == "TRUE", "Correct", "Incorrect")
        }
      }

      ans <- append(ans, ans_value)
      typ <- append(typ, data[[1]]$events$task$type[j])
      cons <- append(cons, csg[j])
    }
  }

    # print(cons)
    # print(typ)
    #print(cbind(data[[1]]$events$task$type, data[[1]]$events$correct,data[[1]]$events$answer$correct))
    #print(ans)
    
    #Distance to the correct answer
    dist1_m <- list()  #dist in m
    dist1_deg <- list()  #dist in degrees - by the player, we can compare both
    dist2_deg <-list()   #dist in degree - right answer
    for (j in 1:(length(id) - 1)) {
      if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
        dist1_m <- append(dist1_m, data[[1]]$events$answer$distance[j])
        if (length(data[[1]]$events$task$question$direction$bearing) != 0) { #Two different ways in the JSON for theme-direction
          if (length(data[[1]]$events$answer$clickDirection) != 0 && !is.na(data[[1]]$events$answer$clickDirection[j])) {
            dist1_deg <- append(dist1_deg, data[[1]]$events$answer$clickDirection[j]) #with the little arrow on the map
            dist2_deg <- append(dist2_deg, data[[1]]$events$compassHeading[j])
          }
          else {
            if (length(data[[1]]$events$answer$compassHeading) != 0) {
              dist1_deg <- append(dist1_deg, data[[1]]$events$answer$compassHeading[j]) #with orientation with tablet
            }
            else {
              dist1_deg <- append(dist1_deg, NA)
            }
            
            dist2_deg <- append(dist2_deg, data[[1]]$events$task$question$direction$bearing[j])
          }
        }
        else {
          dist1_deg <- append(dist1_deg, NA)
          dist2_deg <- append(dist2_deg, NA)
        }
      }
    }
    
    rds <- cbind(unlist(dist1_m),dist_deg = abs(unlist(dist2_deg)-unlist(dist1_deg)))
    #print(rds)
    
    ####sometimes we don't need to merge the column
    if (ncol(rds) == 2) {
      rds[is.na(rds)] <- 0
      dist <- c(rds[,1]+rds[,2])
      dist[dist == 0] <- NA
    }
    else {
      dist <- rds
    }
    
    if (length(dist) != 0) {
      dist <- round(dist,2)
    }
    
    #Add unities on the last column
    for (i in 1:length(typ)) {
      if (!is.na(typ[[i]]) && !is.na(dist[[i]]) && typ[[i]] == "theme-direction"){
        dist[[i]] <- paste(dist[[i]], "°")
      }
      if (!is.na(typ[[i]]) && !is.na(dist[[i]]) && (typ[[i]] == "nav-flag" || typ[[i]] == "theme-loc")) {
        dist[[i]] <- paste(dist[[i]], "m")
      }
    }
    #print(dist)
    
    #Computing time spent on a task   - #inspired from Tino's code
    tps <- data[[1]]$events$timestamp
    time1 <- as.POSIXct(tps[1], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
    tmp <- list()
    for (j in 1:(length(id) - 1)) {
      if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
        time2 <- as.POSIXct(tps[j], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
        tmp <- append(tmp, paste(floor(as.numeric(time2 - time1, units = "secs")),"s"))
        time1 <- as.POSIXct(tps[j+1], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
      }
    }
    
    print(tmp)
    #Computing tries number for each task
    tries = 0
    try <- list()
    for (j in 1:(length(id) - 1)) {
      if (ev[j] == "ON_OK_CLICKED") { #Answer when the player clicks on OK
        tries <- tries + 1
      }
      if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
        try <- append(try, tries)
        tries = 0
      }
    }
    
    #print(try)
    
    rg <- cbind(unlist(typ), unlist(cons), unlist(ans), unlist(tmp), unlist(try), unlist(dist))
    #print(rg)
    
    for (i in 1:(nrow(rg))) {
      if (((!is.na(rg[i,1]) && rg[i,1] == "nav-arrow") || (!is.na(rg[i,1]) && rg[i,1] == "nav-text")
           || (!is.na(rg[i,1]) && rg[i,1] == "nav-photo")) && rg[i,4] != "0 s") {
        rg[i,3] <- "Correct"
        rg[i,5] <- 1
      }
      if (is.na(rg[i,1])) {
        rg[i,1] <- "information"
        rg[i,3] <- NA
        rg[i,5] <- 0
      }
    }
    
    #Delete the last column if it's empty 
    counter_dist <- 0
    for (k in 1:length(dist)) {
      if (is.na(dist[[k]])) {
        counter_dist <- counter_dist + 1
      }
    }
    
    #Build the main table
    if (counter_dist != length(dist)) {
      mat = matrix(rg, ncol = 6, nrow = length(ans))
      
      df <- data.frame(
        Type = mat[,1],
        Assignment = mat[,2],
        Answer = mat[,3],
        Time = mat[,4],
        Tries = mat[,5],
        Error = mat[,6]
      )
      colnames(df)[6] <- "Error in °/m"
    }
    else {
      mat = matrix(rg, ncol = 6, nrow = length(ans)) #Big table without the last column
      
      df <- data.frame(
        Type = mat[,1],
        Assignment = mat[,2],
        Answer = mat[,3],
        Time = mat[,4],
        Tries = mat[,5]
      )
    }
    
    #Name of the player
    #print(data[[1]]$players[1])
    output$player_name <- renderText({
      paste("Player: ", data[[1]]$players[1], sep = "")
    })
    #Overall score
    if (sum(grepl(pattern = "Incorrect", df$Answer)) != 0) { #If incorrect values are in the table
      good <- sum(grepl(pattern = "Correct", df$Answer))
      total <- sum(grepl(pattern = "Incorrect", df$Answer)) + sum(grepl(pattern = "Correct", df$Answer))
    }
    else { #if all is correct
      good <- sum(grepl(pattern = "Correct", df$Answer))
      total <- good
    }
    
    output$overall_score <- renderText({
      paste("Overall score: ", good, "/", total, sep = "")
    })
    
    df_react(df)
    
    output$iris_data <- renderDT({
      df
    })
    
    
    long <- list()
    lati <- list()
    cou <- 1 #counter
    mr <- FALSE #Print an empty map or not
    coor <- data[[1]]$events$task$answer$position$geometry$coordinates #Position answer nav tasks
    
    lng_targ <- list()
    lat_targ <- list()
    lng_true <- list()
    lat_true <- list()
    targ <- data[[1]]$events$answer$clickPosition #Position answer theme localisation task 
    coor_true <- data[[1]]$events$position$coords
    
    #Position for free task
    dr_point_lat <- list()
    dr_point_lng <- list()
    drawing_point_lat <- data[[1]]$events$clickPosition$latitude
    drawing_point_lng <- data[[1]]$events$clickPosition$longitude
    #print(drawing_point_lat)
    
    type_task <- data[[1]]$events$task$type
    cat_task <- data[[1]]$events$task$category
    accuracy_radius <- data[[1]]$events$task$settings$accuracy #For theme-loc or navigation tasks
    accuracy_rad <- 0
    ev <- data[[1]]$events$type #Name of the event
    t <- ""
    
    
    #Print Polygons
    sel_polygon <- data[[1]]$events$task$question$geometry$feature
    lng_poly <- list()
    lat_poly <- list()
    lng_ans_obj <- list()
    lat_ans_obj <- list()
    
    #Recovering answers position for the map
    for (i in 1:(length(id)-1)) {
      if (!is.na(cat_task[i])) {
        if ((cat_task[i] == "nav") && (cou == input$num_value)) {
          long <- append(long, coor[[i]][1])
          lati <- append(lati, coor[[i]][2])
          accuracy_rad <- accuracy_radius[[i]]
          t <- type_task[i]
        }
        if ((cat_task[i] == "info") && (cou == input$num_value)) {
          mr <- TRUE #Showing an empty map
          t <- cat_task[i]
        }
      }
      if (!is.na(type_task[i])) { #target point for theme task
        if ((type_task[i] == "theme-loc") && (ev[i] == "ON_OK_CLICKED") && (cou == input$num_value)) {
          lng_targ <- append(lng_targ, targ[[i]][1])
          lat_targ <- append(lat_targ, targ[[i]][2])
          lng_true <- append(lng_true, coor_true$longitude[[i]])
          lat_true <- append(lat_true, coor_true$latitude[[i]])
          accuracy_rad <- accuracy_radius[[i]]
          t <- type_task[i]
        }
        if ((type_task[i] == "theme-loc") && (cou == input$num_value)) { #Always having the task type shown for theme-localisation
          t <- type_task[i]
        }
        if ((type_task[i] == "theme-direction" || (type_task[i] == "theme-object" && ans_type[[i]] == "PHOTO") || (type_task[i] == "theme-object" && length(data[[1]]$events$task$question$mode) != 0 && data[[1]]$events$task$question$mode[[i]] == "NO_FEATURE")) && cou == input$num_value) { #tasks that show nothing on the map
          mr <- TRUE
          t <- type_task[i]
        }
        if (type_task[i] == "theme-object" && cou == input$num_value && ans_type[[i]] == "MAP_POINT") { #tasks that show nothing on the map
          poly <- sel_polygon[[i]]$geometry$coordinates[[1]]
          for (n in 1:(length(poly)/2)) {
            lng_poly <- append(lng_poly, poly[n])
          }
          for (n in (length(poly)/2+1):length(poly)) {
            lat_poly <- append(lat_poly, poly[n])
          }
          t <- type_task[i]
          if (type_task[i] == "theme-object" && (cou == input$num_value)) {
            lng_ans_obj <- append(lng_ans_obj, targ[[i]][1])
            lat_ans_obj <- append(lng_ans_obj, targ[[i]][2])
          }
        }
        if ((type_task[i] == "free") && (cou == input$num_value) && length(drawing_point_lat) != 0 && !is.na(drawing_point_lat[[i]]) && ans_type[[i]] == "DRAW") {
          dr_point_lat <- append(dr_point_lat, drawing_point_lat[[i]])
          dr_point_lng <- append(dr_point_lng, drawing_point_lng[[i]])
          t <- type_task[i]
        }
        if ((type_task[i] == "free") && (cou == input$num_value) && (length(drawing_point_lat) == 0 || is.na(drawing_point_lat[[i]])) && ans_type[[i]] == "DRAW") { #correcting error to visualize draw
          mr <- TRUE
          t <- type_task[i]
        }
        if ((type_task[i] == "free") && (cou == input$num_value) && ans_type[[i]] != "DRAW") {
          mr <- TRUE
          t <- type_task[i]
        }
      }
      if (is.na(type_task[i]) && (i != 1) && (cou == input$num_value)) { #na task
        mr <- TRUE
      }
      if ((!is.na(id[i]) && (i != 1) && (id[i] != id[i + 1])) || i == (length(id) - 1)) {
        cou <- cou + 1
      }
    }
    
    #Print trajectory on the map
    traj_lng <- list()
    traj_lat <- list()
    accuracy <- list()
    task_number <- data[[1]]$waypoints$taskNo #Task number
    for (i in 1:length(task_number)) {
      if (task_number[i] == input$num_value) {
        traj_lng <- append(traj_lng, data[[1]]$waypoints$position$coords$longitude[i])
        traj_lat <- append(traj_lat, data[[1]]$waypoints$position$coords$latitude[i])
        if (length(data[[1]]$waypoints$position$coords$accuracy) != 0) {
          accuracy <- append(accuracy, data[[1]]$waypoints$position$coords$accuracy[i]) #accuracy on coordinates
        }
        else {
          accuracy <- append(accuracy, 1)
        }
      }
    }
    
    
    #Compute distance traveled
    deg_to_rad <- pi/180
    R <- 6.378e6
    if (length(traj_lng) > 1) {
      for (k in 1:(length(traj_lng)-1)) {
        if (!is.na(traj_lng[[k]][1])) {
          lat_1 <- traj_lat[[k]][1]
          lng_1 <- traj_lng[[k]][1]
          lat_2 <- traj_lat[[k+1]][1]
          lng_2 <- traj_lng[[k+1]][1]
          d <- R * acos(sin(lat_1*deg_to_rad)*sin(lat_2*deg_to_rad) + cos(lat_1*deg_to_rad)*cos(lat_2*deg_to_rad)*cos((lng_2 - lng_1)*deg_to_rad))
          if (accuracy[[k]] >= 20) {
            traj_lat[k] <- traj_lat[k+1] #Filter GPS coordinate errors
            traj_lng[k] <- traj_lng[k+1]
          }
        }
      }
    }
    
    # create icons (from Jakub's code)
    loc_marker <- makeIcon(
      iconUrl = "https://raw.githubusercontent.com/origami-team/origami/master/src/assets/icons/marker-editor.png",
      iconWidth = 20, iconHeight = 20,
      iconAnchorX = 10, iconAnchorY = 20,
    )
    loc_marker_green <- makeIcon(
      iconUrl = "https://raw.githubusercontent.com/origami-team/origami/master/src/assets/icons/marker-editor-solution.png",
      iconWidth = 20, iconHeight = 20,
      iconAnchorX = 10, iconAnchorY = 20,
    )
    
    #Print map
    if (mr == TRUE || (length(long) != 0 && length(traj_lat) == 0) || length(ans) <= input$num_value || (length(lng_targ) == 0 && length(lng_true) == 0 && t == "theme-loc")) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          setView(lng = 7, lat = 51, zoom = 6)
      }
    }
    if (length(long) != 0 && length(traj_lat) != 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          addMarkers(lng = unlist(long)[1], lat = unlist(lati)[1], icon = loc_marker_green) %>%
          addCircles(lng = unlist(long)[1], lat = unlist(lati)[1], radius = accuracy_rad, opacity = 0.5) %>%
          addPolylines(lng = unlist(traj_lng), lat = unlist(traj_lat), color = "red", weight = 2, opacity = 1, stroke = TRUE)
      }
    }
    if (length(dr_point_lng) != 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          addPolylines(lng = unlist(dr_point_lng), lat = unlist(dr_point_lat), color = "red", weight = 2, opacity = 1, stroke = TRUE)
      }
    }
    if (length(lng_targ) != 0 && length(lng_true) != 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>% #Displaying last answer of the player
          addMarkers(lng = unlist(lng_targ)[length(lng_targ)], lat = unlist(lat_targ)[length(lat_targ)], icon = loc_marker) %>%
          addMarkers(lng = unlist(lng_true)[length(lng_true)], lat = unlist(lat_true)[length(lat_true)], icon = loc_marker_green) %>%
          addCircles(lng = unlist(lng_true)[length(lng_true)], lat = unlist(lat_true)[length(lat_true)], radius = accuracy_rad)
      }
    }
    if (length(lng_targ) == 0 && length(lng_true) != 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>% #Displaying last answer of the player
          addMarkers(lng = unlist(lng_true)[length(lng_true)], lat = unlist(lat_true)[length(lat_true)], icon = loc_marker_green) %>%
          addCircles(lng = unlist(lng_true)[length(lng_true)], lat = unlist(lat_true)[length(lat_true)], radius = accuracy_rad)
      }
    }
    if (length(lng_poly) != 0 && length(lng_ans_obj) == 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          addPolygons(lng = unlist(lng_poly), lat = unlist(lat_poly), color = "blue", fillColor = "#90caf9", weight = 2, opacity = 1)
      }
    }
    if (length(lng_poly) != 0 && length(lng_ans_obj) != 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          addMarkers(lng = unlist(lng_ans_obj)[length(lng_ans_obj)], lat = unlist(lat_ans_obj)[length(lat_ans_obj)], icon = loc_marker) %>%
          addPolygons(lng = unlist(lng_poly), lat = unlist(lat_poly), color = "blue", fillColor = "#90caf9", weight = 2, opacity = 1)
      }
    }
    
    mr <- FALSE
    output$map <- renderLeaflet(map_shown)
    
    
    #Convert abbreviation for type task
    if (!is.na(t)) {
      if (t == "nav-flag") {
        t <- "Navigation to flag"
      }
      if (t == "nav-arrow") {
        t <- "Navigation with arrow"
      }
      if (t == "nav-photo") {
        t <- "Navigation via photo"
      }
      if (t == "nav-text") {
        t <- "Navigation via text"
      }
      if (t == "theme-loc") {
        t <- "Self location"
      }
      if (t == "theme-object") {
        t <- "Object location"
      }
      if (t == "theme-direction") {
        t <- "Direction determination"
      }
      if (t == "free") {
        t <- "Free"
      }
      if (t == "info") {
        t <- "Information"
      }
      if (t == "") {
        t <- "No task exists with this number"
      }
    }
    
    output$mapLegend <- renderText({paste("Type task:",t)})
    
    #Download map
    output$downloadMap <- downloadHandler(
      filename = function() {
        paste("map_", Sys.Date(), ".html", sep="")
      },
      content = function(file) {
        m <- saveWidget(map_shown, file = file, selfcontained = TRUE)
      }
    )
    
    
    #photo code starts---------------------
    cou <- 1 #counter
    pict <- list()
    ans_photo <- list()
    
    for (i in 1:(length(id)-1)) {
      if ((!is.na(id[i]) && (i != 1) && (id[i] != id[i + 1])) || i == (length(id) - 1)) {
        cou <- cou + 1
        pict <- append(pict, unlist(data[[1]]$events$task$question$photo[[i]]))
        ans_photo <- append(ans_photo, unlist(data[[1]]$events$answer$photo$changingThisBreaksApplicationSecurity[[i]]))
      }
    }
    
    if (length(pict) != 0) { #Photos in question
      if (input$num_value <= length(pict) && !is.na(pict[[input$num_value]]) && pict[[input$num_value]] != "") {
        # Render photo display with download buttons
        output$photo_display <- renderUI({
          
          photo_url <- pict[[input$num_value]]
          
          output[["download_image"]] <- downloadHandler(
            filename = function() {
              paste("image_", t, ".jpg", sep = "")
            },
            content = function(file) {
              download.file(photo_url, file, mode = "wb")
            }
          )
          
          # Create a flex container for images
          div(style = "display: flex; flex-wrap: wrap; gap: 20px;",  # Flexbox for alignment
              tagList(
                tags$div(style = "flex: 0 1 200px; display: inline-block; text-align: center;",  # Each image in a flexible box
                         tags$h4(paste("Question for", t)),
                         tags$img(src = photo_url, height = "500px", style = "margin: 10px; border: 1px solid #ccc;"),
                         downloadButton("download_image"), label = "Download", class = "btn btn-primary", style = "margin-top: 10px;")
              )
          )
        })
      }
    }
    
    if (length(ans_photo) != 0) { #Photos in answer
      if (input$num_value <= length(ans_photo) && !is.na(ans_photo[[input$num_value]]) && ans_photo[[input$num_value]] != "") {
        # Render photo display with download buttons
        output$photo_display <- renderUI({
          
          photo_ans_url <- ans_photo[[input$num_value]]
          
          output[["download_image_2"]] <- downloadHandler(
            filename = function() {
              paste("image_", t, ".jpg", sep = "")
            },
            content = function(file) {
              download.file(photo_ans_url, file, mode = "wb")
            }
          )
          
          # Create a flex container for images
          div(style = "display: flex; flex-wrap: wrap; gap: 20px;",  # Flexbox for alignment
              tagList(
                tags$div(style = "flex: 0 1 200px; display: inline-block; text-align: center;",  # Each image in a flexible box
                         tags$h4(paste("Answer for", t)),
                         tags$img(src = photo_ans_url, height = "500px", style = "margin: 10px; border: 1px solid #ccc;"),
                         downloadButton("download_image_2"), label = "Download", class = "btn btn-primary", style = "margin-top: 10px; background-color: #0CD1E8 ")
              )
          )
        })
      }
    }
    
    if (length(ans_photo) == 0 && length(pict) == 0) {
      output$photo_display <- renderUI({
        "No photos for this game"
      })
    }
    
    if (length(pict) != 0 && input$num_value > length(pict)) {
      output$photo_display <- renderUI({
        "No task exists with this number"
      })
    }
    
    if (length(pict) != 0 && length(ans_photo) != 0) {
      if (input$num_value <= length(ans_photo) && (is.na(ans_photo[[input$num_value]]) || ans_photo[[input$num_value]] == "") && (is.na(pict[[input$num_value]]) || pict[[input$num_value]] == "")) {
        output$photo_display <- renderUI({
          "No photos for this task"
        })
      }
    }
    if (length(pict) == 0 && length(ans_photo) != 0) {
      if (input$num_value <= length(ans_photo) && (is.na(ans_photo[[input$num_value]]) || ans_photo[[input$num_value]] == "")) {
        output$photo_display <- renderUI({
          "No photos for this task"
        })
      }
    }
    if (length(pict) != 0 && length(ans_photo) == 0) {
      if (input$num_value <= length(pict) && (is.na(pict[[input$num_value]]) || pict[[input$num_value]] == "")) {
        output$photo_display <- renderUI({
          "No photos for this task"
        })
      }
    }
    
    #photo code ends here-----------------------------------
    
  })
  
  #####End of big table
  
  #####multiple files
  observeEvent(req(input$selected_multiple_files, input$num_value), {
    cores <- data.frame(Name = c(), Correct = c(), Answer = c(), Error = c())
    ngts <- data.frame(Name = c(), Correct = c(), Time = c(), Distance = c())
    
    sum_cor <- list()
    sum_incor <- list()
    
    data2 <- load_multiple() #load multiple json
    data <- loaded_json() #load one json
    
    id <- data[[1]]$events$task[["_id"]]
    type_task <- data[[1]]$events$task$type
    cat_task <- data[[1]]$events$task$category
    t <- "" #type task
    cou <- 1 #counter
    
    
    #Recovering answers position for the map
    for (i in 1:(length(id)-1)) {
      if (!is.na(cat_task[i])) {
        if ((cat_task[i] == "nav") && (cou == input$num_value)) {
          t <- type_task[i]
        }
        if ((cat_task[i] == "theme") && (cou == input$num_value)) {
          t <- type_task[i]
        }
        if ((cat_task[i] == "info") && (cou == input$num_value)) {
          t <- cat_task[i]
        }
      }
      if ((!is.na(id[i]) && (i != 1) && (id[i] != id[i + 1])) || i == (length(id) - 1)) {
        cou <- cou + 1
      }
    }
    
    for (i in 1:length(data2)) {
      
      id <- data2[[i]]$events$task[["_id"]]
      
      ans2 <- list()
      typ2 <- list()
      dist1_m_new <- list()
      dist1_deg_new <- list()
      dist2_deg_new <- list()
      
      tps <- data2[[i]]$events$timestamp
      time1 <- as.POSIXct(tps[1], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
      tmp2 <- list()
      
      ev <- data2[[i]]$events$type #Name of the event
      tries = 0
      try2 <- list()
      
      for (j in 1:(length(id) - 1)) {
        if (ev[j] == "ON_OK_CLICKED") {
          tries <- tries + 1
        }
        if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
          if (length(data2[[i]]$events$answer$correct[j]) != 0) {
            ans2 <- append(ans2, data2[[i]]$events$answer$correct[j])
          }
          else {
            ans2 <- append(ans2, data2[[i]]$events$correct[j])
          }
          typ2 <- append(typ2, data2[[i]]$events$task$type[j])
          
          dist1_m_new <- append(dist1_m_new, data2[[i]]$events$answer$distance[j])
          if (length(data2[[i]]$events$task$question$direction$bearing) != 0) { #Two different ways in the JSON for theme-direction
            if (length(data2[[i]]$events$answer$clickDirection) != 0 && !is.na(data2[[i]]$events$answer$clickDirection[j])) {
              dist1_deg_new <- append(dist1_deg_new, data2[[i]]$events$answer$clickDirection[j])
              dist2_deg_new <- append(dist2_deg_new, data2[[i]]$events$compassHeading[j])
            }
            else {
              dist1_deg_new <- append(dist1_deg_new, data2[[i]]$events$answer$compassHeading[j])
              dist2_deg_new <- append(dist2_deg_new, data2[[i]]$events$task$question$direction$bearing[j])
            }
          }
          else {
            dist1_deg_new <- append(dist1_deg_new, NA)
            dist2_deg_new <- append(dist2_deg_new, NA)
          }
          
          time2 <- as.POSIXct(tps[j], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
          tmp2 <- append(tmp2, floor(as.numeric(time2 - time1, units = "secs")))
          time1 <- as.POSIXct(tps[j+1], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
          
          try2 <- append(try2, tries)
          tries = 0
        }
      }
      
      for (k in 1:length(ans2)) {
        if (!is.na(typ2[[k]][1]) && (typ2[[k]][1] == "nav-photo" || typ2[[k]][1] == "nav-arrow" || typ2[[k]][1] == "nav-text") && tmp2[[k]][1] != 0) {
          ans2[[k]][1] <- "Correct"
        }
        if (!is.na(ans2[[k]][1]) && ans2[[k]][1] == TRUE) {
          ans2[[k]][1] <- "Correct"
        }
        if (!is.na(ans2[[k]][1]) && ans2[[k]][1] == FALSE) {
          ans2[[k]][1] <- "Incorrect"
        }
      }
      
      traj_lng <- list()
      traj_lat <- list()
      accuracy <- list()
      task_number <- data2[[i]]$waypoints$taskNo #Task number
      for (k in 1:length(task_number)) {
        if ((task_number[k] == input$num_value)) {
          traj_lng <- append(traj_lng, data2[[i]]$waypoints$position$coords$longitude[k])
          traj_lat <- append(traj_lat, data2[[i]]$waypoints$position$coords$latitude[k])
          if (length(data2[[i]]$waypoints$position$coords$accuracy) != 0) {
            accuracy <- append(accuracy, data2[[i]]$waypoints$position$coords$accuracy[k]) #accuracy on coordinates
          }
          else {
            accuracy <- append(accuracy, 1)
          }
        }
      }
      
      
      #Compute distance traveled
      deg_to_rad <- pi/180
      R <- 6.378e6
      d_total <- 0 #counter
      if (length(traj_lng) > 1) {
        for (k in 1:(length(traj_lng)-1)) {
          if (!is.na(traj_lng[[k]][1])) {
            lat_1 <- traj_lat[[k]][1]
            lng_1 <- traj_lng[[k]][1]
            lat_2 <- traj_lat[[k+1]][1]
            lng_2 <- traj_lng[[k+1]][1]
            d <- R * acos(sin(lat_1*deg_to_rad)*sin(lat_2*deg_to_rad) + cos(lat_1*deg_to_rad)*cos(lat_2*deg_to_rad)*cos((lng_2 - lng_1)*deg_to_rad))
            if (d <= 10 && accuracy[[k]] <= 20) {
              d_total <- d_total + d
            }
          }
        }
      }
      #print(d_total)
      
      #TIME VS DISTANCE
      if (length(ans2) >= input$num_value && unlist(tmp2[[input$num_value]][1]) != 0) {
        if (length(dist1_m_new) != 0 && !is.na(unlist(dist1_m_new)[[input$num_value]][1])) { #Distance to the correct answer or not
          ngt <- cbind(Name = data2[[i]]$players[1], Correct = unlist(ans2[[input$num_value]][1]), Time = paste(unlist(tmp2[[input$num_value]][1]),"s"), Distance_travelled = paste(round(d_total), "m"), Distance_to_the_correct_answer = paste(round(unlist(dist1_m_new)[[input$num_value]][1],3),"m"))
          ngts <- rbind(ngts, ngt)
        }
        else {
          ngt <- cbind(Name = data2[[i]]$players[1], Correct = unlist(ans2[[input$num_value]][1]), Time = paste(unlist(tmp2[[input$num_value]][1]),"s"), Distance_travelled = paste(round(d_total),"m"), Distance_to_the_correct_answer = NA)
          ngts <- rbind(ngts, ngt)
        }
      }
      
      
      #CORRECT & ERRORS
      if (length(dist1_deg_new) >= input$num_value) {
        if (!is.na(unlist(ans2[[input$num_value]][1])) || !is.na(unlist(dist1_deg_new[[input$num_value]][1]))) { #Verify if the direction task is played or not
          core <- cbind(Name = data2[[i]]$players[1], Correct = unlist(ans2[[input$num_value]][1]), Answer = paste(round(unlist(dist1_deg_new[[input$num_value]][1]),3),"°"), Error = paste(round(unlist(abs(unlist(dist2_deg_new)-unlist(dist1_deg_new))[[input$num_value]][1]),3),"°"))
          cores <- rbind(cores, core)
        }
      }
      
      #Compute correct or incorrect tries
      if (length(ans2) >= input$num_value) { #outside index
        if (!is.na(ans2[[input$num_value]])) { #answer existence
          if (ans2[[input$num_value]] == "Correct") { #correct answer
            sum_incor <- append(sum_incor, 0) 
            sum_cor <- append(sum_cor, 1)
          }
          else { #incorrect answer
            sum_incor <- append(sum_incor, 1)
            sum_cor <- append(sum_cor, 0)
          }
        }
        else { #NA value: indeterminate answer
          sum_incor <- append(sum_incor, 0)
          sum_cor <- append(sum_cor, 0)
        }
      }
    }
    
    #Graphic on time
    if (length(ngts) != 0) {
      
      oral <- list()
      oral2 <- list()
      seconds <- strsplit(ngts[,3], " s")
      meters <- strsplit(ngts[,4], " m")
      for (k in 1:length(seconds)) {
        oral <- append(oral, as.numeric(seconds[[k]]))  #Convert in numeric format to show on graphic
        oral2 <- append(oral2, as.numeric(meters[[k]]))
      }
      
      df_player <- data.frame(time = unlist(oral), distance = unlist(oral2), players = ngts[,1])
      time_chart <- ggplot(df_player, aes(x = time, y = distance, fill = players)) +
        geom_point(size=4, shape=22) +
        labs(title = paste("Time chart of task:", t), x = "Time for this task in seconds", y = "Distance travelled in metres")
      
      for (i in 1:length(df_player$time)) {
        if (df_player$time[i] == 0) {
          sum_cor[i] <- 0
          sum_incor[i] <- 0
        }
      }
    }
    else {
      time_chart <- ggplot()
    }
    
    
    output$time_chart <- renderPlot({
      time_chart
    })
    
    #Download time chart
    output$save_time_chart <- downloadHandler(
      filename = function(){
        paste("time_chart_", Sys.Date(), ".png", sep="")
      },
      content = function(file){
        png(file)
        print(time_chart)
        dev.off()
      }
    )
    
    
    corr <- sum(unlist(sum_cor)) #Number of correct answer for one task - only the last answer per player
    incorr <- sum(unlist(sum_incor)) #Number of incorrect answer for one task - only the last answer per player
    rel <- c(corr,incorr)
    answer_vect <- c("correct","incorrect")
    df_pie <- data.frame(answers = answer_vect, value = rel)
    #print(df_pie)
    
    
    #pie_chart
    if (corr == 0 && incorr == 0) {
      pie_chart <- ggplot() +
        theme_void() +
        labs(title = "You didn't reply for this task")
    }
    else {
      pie_chart <- ggplot(df_pie, aes(x = "", y = value, fill = answers)) +
        geom_col() +
        coord_polar(theta = "y") +
        scale_fill_manual(values = c("#3C8D53","#BE2A3E")) +
        theme_void() +
        labs(title = paste("Pie chart of task:", t))
    }
    
    #Two outputs because two conditions in UI
    output$pie_chart <- renderPlot({
      pie_chart
    })
    
    output$pie_chart2 <- renderPlot({
      pie_chart
    })
    
    #Download pie chart
    output$save_picture <- downloadHandler(
      filename = function(){
        paste("pie_chart_", Sys.Date(), ".png", sep="")
      },
      content = function(file){
        png(file)
        print(pie_chart)
        dev.off()
      }
    )
    
    output$save_picture2 <- downloadHandler(
      filename = function(){
        paste("pie_chart_", Sys.Date(), ".png", sep="")
      },
      content = function(file){
        png(file)
        print(pie_chart)
        dev.off()
      }
    )
    
    #Delete the last column if it's empty 
    counter <- 0
    if (length(ngts) != 0) {
      for (k in 1:length(ngts[,5])) { 
        if (is.na(ngts[k,5])) {
          counter <- counter + 1
        }
      }
      if (counter == length(ngts[,5])) {
        ngts <- select(ngts, -Distance_to_the_correct_answer)
      }
    }
    
    
    #TIME VS DISTANCE Table
    output$cmp_table1 <- renderTable(
      ngts
    )
    
    #CORRECT & ERRORS Table
    output$cmp_table2 <- renderTable(
      cores
    )
    
    #Convert abbreviation for type task
    if (!is.na(t)) {
      if (t == "nav-flag") {
        t <- "Navigation to flag"
      }
      if (t == "nav-arrow") {
        t <- "Navigation with arrow"
      }
      if (t == "nav-photo") {
        t <- "Navigation via photo"
      }
      if (t == "nav-text") {
        t <- "Navigation via text"
      }
      if (t == "theme-loc") {
        t <- "Self location"
      }
      if (t == "theme-object") {
        t <- "Object location"
      }
      if (t == "theme-direction") {
        t <- "Direction determination"
      }
      if (t == "free") {
        t <- "Free"
      }
      if (t == "info") {
        t <- "Information"
      }
      if (t == "") {
        t <- "No task exists with this number"
      }
    }
    
    output$tabLegend <- renderText({paste("Type task:",t)})
    output$graphLegend <- renderText({paste("Type task:",t)})
    
    #Save analyse tables
    output$save_table1 <- downloadHandler(
      filename = function(){
        paste("time_dist_table_", Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        write.csv(ngts, file)
      }
    )
    
    output$save_table2 <- downloadHandler(
      filename = function(){
        paste("ans_err_table_", Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        write.csv(cores, file)
      }
    )
  })
  
  
  #Download big table
  output$save_data <- downloadHandler(
    filename = function(){
      paste("data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(df_react(), file)
    }
  )
  
}

shinyApp(ui, server)