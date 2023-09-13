box::use(
  shiny[...],
  shinyWidgets
)

box::use(
  app/logic/api_interactions
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("search_item"),
              label = "Search",
              placeholder = "Type here to search item",
              value = "Iron"),
    shinyWidgets$pickerInput(ns("select_item"),
                             label="Select Item",
                             choices = NULL)
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    search_result <- reactive({
      req(nchar(input$search_item)>2)
      api_interactions$search_item(input$search_item)
    })
    
    observeEvent(search_result(), {
      shinyWidgets::updatePickerInput(session = session, 
                                      inputId = "select_item",
                                      choices = search_result()$id,
                                      choicesOpt = list(content = search_result()$display),
                                      selected = "Iron Ore",
                                      options = list(size=5, liveSearch=TRUE,
                                                     width = '100%'))
    }
    )
    
    return(
      reactive({
        req(input$select_item)
        selected_item_data <- search_result()[search_result()$id == input$select_item,]
        list(id = selected_item_data$id,
             name = selected_item_data$name,
             img_url = selected_item_data$img_url,
             display = selected_item_data$display)
      })
    )
    
  })
}
