box::use(
  shiny[...],
  bsicons
)

box::use(
  app/logic/api_interactions,
  app/logic/json_parsing
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  actionButton(inputId = ns("button_go"),
               label = "Analyze",
               icon = icon("bolt"),
               class = "btn-primary")
  
}

#' @export
server <- function(id, selected_server, selected_item, timerange) {
  moduleServer(id, function(input, output, session) {
    
    item_info <- reactiveVal(NULL)
    item_price <- reactiveVal(NULL)
    
    observeEvent(input$button_go, {
      req(selected_server())
      req(selected_item())

      server <- selected_server()$server
      faction <- selected_server()$faction
      item <- selected_item()$id
      item_info(api_interactions$get_item_info(server, faction, item))
      item_price(api_interactions$get_item_price(server, faction, item, timerange))
    })
    
    price_df <- reactive({
      req(item_price())
      req(item_price()$data)
      json_parsing$json_to_df_price(item_price())
    })
    
    return(
      reactive({
        # req(item_info())
        req(price_df())
        # list(item_info = item_info(),
        #      item_price = price_df())
        price_df()
      }))
    
  })
}
