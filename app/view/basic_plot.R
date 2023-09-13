box::use(
  shiny[...],
  ggplot2[...],
  bslib[...],
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  card(
    height = 250,
    full_screen = TRUE,
    card_header(textOutput(ns("card_name"))),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        open = "closed",
        checkboxGroupInput(ns("display_choice"),
                     "Display",
                     choices = c("price", "quantity"),
                     selected = "price")
      ),
      plotOutput(ns("plot"))
    )
  )
}

#' @export
server <- function(id, item_data) {
  moduleServer(id, function(input, output, session) {
    
    output$plot <- renderPlot({
      req(item_data())
      req(input$display_choice)
      p <- item_data() |> 
        ggplot(aes(x = datetime)) +
        scale_color_manual(values = c(price = "orange",
                                      quantity = "blue"))
      
      if ("price" %in% input$display_choice) {
        p <- p + geom_line(aes(y = price_avg, color = "price"))
      }
      
      if ("quantity" %in% input$display_choice) {
        p <- p + geom_line(aes(y = quantity, color = "quantity"))
      }
      
      return(p)
        
    })
    
    
    output$card_name <- renderText({
      paste("Plot: ", paste(input$display_choice, collapse=" & "))
    })
  })
}
