#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(shinyFeedback)
library(shinyWidgets)
library(bsicons)

library(httr2)
library(jsonlite)
library(glue)
library(purrr)
library(tibble)
library(stringr)
library(dplyr)


servers_url <- "https://api.nexushub.co/wow-classic/v1/servers"

servers_json <- httr2::request(servers_url) |> req_perform() |> resp_body_json()

regions <- names(servers_json)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(bootswatch = "yeti") |>
    bs_add_rules("#analyze { @extend .justify-content-center }"),
  useShinyFeedback(),
  tags$head(tags$style("
                       .jhr{
                       display: inline;
                       vertical-align: middle;
                       padding-left: 10px;
                       }")),
  # Application title
  titlePanel(HTML("<h1><img src='https://wow.zamimg.com/images/wow/icons/large/inv_misc_dust_01.jpg'/> Auction Board</h1>")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tags$h2("Your Server"),
      selectInput("region",
                  label = "Region",
                  choices = regions),
      selectInput("server",
                  label = "Server",
                  choices = NA),
      pickerInput("faction",
                  label = "Faction",
                  choices = c("Alliance","Horde"),
                  choicesOpt = list(content = c("<img src='https://nexushub.co/img/wow-classic/ui/alliance.svg' width=20px><div class='jhr'>Alliance</div></img>",
                                                "<img src='https://nexushub.co/img/wow-classic/ui/horde.svg' width=20px><div class='jhr'>Horde</div></img>") )),
      tags$h2("Search item"),
      textInput("search",
                label = "Search",
                placeholder = "Type here to search item"),
      pickerInput("select_item",
                  label="Select Item",
                  choices = NA),
      fluidRow(column(2),
               column(8,
                      actionButton("analyze","Analyze",icon = icon("bolt"),class="btn-primary",width = "100%")),
               column(2))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(6, 
               tags$h3("Item Details"),
               verbatimTextOutput("item")),
        column(6, 
               tags$h3("Price Details"),
               verbatimTextOutput("prices"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  server_choices <- reactive({
    unlist(servers_json[[input$region]])
  })
  
  observe({
    updateSelectInput(inputId = "server",
                      choices = server_choices())
  })
  
  search_result <- reactive({
    req(input$search)
    req(nchar(input$search) > 2)
    
    fixed_search_string <- str_replace_all(input$search," ","%20")
    
    search_url <- glue("https://api.nexushub.co/wow-classic/v1/search?query={fixed_search_string}&limit=20&threshold=1")
    
    search_json <- request(search_url) |> req_perform() |> resp_body_json()
    
    has_result <- length(search_json) > 0
    
    feedbackDanger("search",!has_result, "No result")
    
    req(has_result)
    
    tibble(id = unlist(map(search_json, ~.x[['itemId']])),
           name = unlist(map(search_json, ~.x[['name']])),
           img_url = unlist(map(search_json, ~.x[['imgUrl']]))) |> 
      distinct() |>
      rowwise() |>
      mutate(display = glue(as.character("<img src='{img_url}' width=30px><div class='jhr'>{name}</div></img>")))
    
  })
  
  observeEvent(search_result(),{
    
    req(search_result())
    
    updatePickerInput(session = getDefaultReactiveDomain(),
                      inputId = "select_item",
                      choices = search_result()$id,
                      choicesOpt = list(content = search_result()$display),
                      options = list(size=5,liveSearch=TRUE))
  })
  
  
  prices_json <- eventReactive(input$analyze,{
    req(input$select_item)
    req(input$server)
    req(input$faction)
    
    prices_url <- glue('https://api.nexushub.co/wow-classic/v1/items/{tolower(input$server)}-{tolower(input$faction)}/{input$select_item}/prices')
    request(prices_url) |> req_perform() |> resp_body_json()
    
  })
  
  item_json <- eventReactive(input$analyze,{
    req(input$select_item)
    req(input$server)
    req(input$faction)
    
    prices_url <- glue('https://api.nexushub.co/wow-classic/v1/items/{tolower(input$server)}-{tolower(input$faction)}/{input$select_item}')
    request(prices_url) |> req_perform() |> resp_body_json()
    
  })
  
  
  output$item <- renderText({
    req(item_json())
    paste(capture.output(str(item_json()), split=TRUE),"\n\r")
  })
  
  output$prices <- renderText({
    req(prices_json())
    paste(capture.output(str(prices_json()), split=TRUE),"\n\r")
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
