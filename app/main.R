box::use(
  shiny[...],
  bslib[...]
)

box::use(
  app/view/select_server,
  app/view/search_item,
  app/view/get_item_data,
  app/view/basic_plot,
  app/view/item_card
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    title = HTML("<img src='https://wow.zamimg.com/images/wow/icons/large/inv_misc_dust_01.jpg'/> Auction Dash"),
    sidebar = sidebar(open = TRUE,
                      width = 300,
                      select_server$ui(ns("select_server")),
                      search_item$ui(ns("search_item")),
                      get_item_data$ui(ns("get_item_data"))),
    nav_panel("Home",
              tags$head(HTML('<script>const whTooltips = {colorLinks: true, iconizeLinks: true, renameLinks: false, iconSize: "medium"};</script>
<script src="https://wow.zamimg.com/js/tooltips.js"></script>')),
              item_card$ui(ns("item_card")),
              basic_plot$ui(ns("basic_plot"))),
    nav_panel("Details")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    selected_server <- select_server$server("select_server")
    selected_item <- search_item$server("search_item")
    
    observe(print(selected_server()))
    observe(print(selected_item()))
    
    item_data <- get_item_data$server("get_item_data", 
                                      selected_server = selected_server,
                                      selected_item = selected_item,
                                      timerange = 90)
    
    item_card$server("item_card", 
                     selected_item, 
                     item_data)
    
    basic_plot$server("basic_plot",
                      item_data)
    
    
    # observe(print(item_data()))
    
  })
}
