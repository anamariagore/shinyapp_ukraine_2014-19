## Bonus Shiny Assignment - 2022 Programming in Psychological Science
#
# Record of Revisions
#
# Date            Programmer              Descriptions of Change
# ====         ================           ======================
# 28-Feb-22      Ana-Maria Gore               Original code

pacman::p_load(shiny, data.table, dplyr, gganimate, ggplot2, gifski)

# Define UI ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Russian attacks on Ukraine 2014-2019"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a dataset ----
      selectInput("year", "Choose the year:",
                  choices = c("2014 to 2019", "2014", "2015", "2016", "2017", "2018", "2019")),
      
      helpText("Please wait until the graph is loading. This might take a while."),
      actionButton("update", "Update View")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h4("Graph"),
      imageOutput("image"),
    )
    
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
  data <- fread("https://raw.githubusercontent.com/anamariagore/ukraine-2014-2022-/main/conflict_data_ukr.csv") %>% slice(2:n())
  data <- as.data.frame(data)
  data$date_start <- as.Date(data$date_start)
  data <-data[order(data$date_start),]
  data2014 <- data %>% filter(year == 2014)
  data2015 <- data %>% filter(year == 2015)
  data2016 <- data %>% filter(year == 2016)
  data2017 <- data %>% filter(year == 2017)
  data2018 <- data %>% filter(year == 2018)
  data2019 <- data %>% filter(year == 2019)
  
  yearInput <- eventReactive(input$update, {
    switch(input$year,
           "2014 to 2019" = data,
           "2014" = data2014,
           "2015" = data2015,
           "2016" = data2016,
           "2017" = data2017,
           "2018" = data2018,
           "2019" = data2019)
  }, ignoreNULL = FALSE)

  #Output ----
  output$image <- renderImage({
    
    dataset <- yearInput()
    dataset$cumulative <- cumsum(dataset$best)
    graph <- dataset %>%
      ggplot(aes(x=date_start, y=cumulative, color=year), renderer = gifski_renderer()) +
      geom_path() +
      ggtitle("Deaths resulting from the attacks") +
      labs(y = "deaths", x = "date") +
      transition_reveal(date_start)
    
    anim_save("outfile.gif", animate(graph)) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif", contentType = "image/gif")
  },
  deleteFile = TRUE
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)
