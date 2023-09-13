box::use(
  shiny[...],
  bslib[...],
  bsicons[...],
  glue[glue],
  htmltools[css]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  card(
    fill = TRUE,
    max_height = "250px",
    card_header(uiOutput(ns("card_header"))),
    layout_column_wrap(
      width = "49%",
      fill = FALSE,
      value_box(
        title = "Value",
        value = p("Average: ", textOutput(ns("price_avg"), inline = TRUE)),
        color_theme = "info",
        showcase = bs_icon("graph-up-arrow"),
        theme_color = "white",
        p("Maximum: ", textOutput(ns("price_max"), inline = TRUE)),
        p("Minimum: ", textOutput(ns("price_min"), inline = TRUE))
      ),
      value_box(
        title = "Quantity",
        value = p("Average: ", textOutput(ns("quantity_avg"),inline = TRUE)),
        showcase = bs_icon("boxes"),
        theme_color = "white",
        p("Maximum: ", textOutput(ns("quantity_max"), inline = TRUE)),
        p("Minimum: ", textOutput(ns("quantity_min"), inline = TRUE))
      )
    )
  )
}

#' @export
server <- function(id, selected_item, item_data) {
  moduleServer(id, function(input, output, session) {
    
    
    output$card_header <- renderUI({
      req(selected_item()$display)
      HTML(selected_item()$display)
    })
    
    stats <- reactive({
      list(price_avg = mean(item_data()$price_avg),
           price_min = min(item_data()$price_avg),
           price_max = max(item_data()$price_avg),
           quantity_avg = mean(item_data()$quantity), 
           quantity_min = min(item_data()$quantity),
           quantity_max = max(item_data()$quantity))
    })
    
    
    output$price_avg <- renderText({
      stats()$price_avg
    })
    
    output$price_min <- renderText({
      stats()$price_min
    })
    
    output$price_max <- renderText({
      stats()$price_max
    })
    
    output$quantity_avg <- renderText({
      stats()$quantity_avg
    })
    
    output$quantity_min <- renderText({
      stats()$quantity_min
    })
    
    output$quantity_max <- renderText({
      stats()$quantity_max
    })
    
    
  })
}

