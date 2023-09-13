box::use(
  shiny[...],
  ggplot2[...],
  bslib[...],
)

box::use(
  app/logic/value_formating
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
      
      data <- item_data()
      
      data$price_avg <- value_formating$value_to_gold(data$price_avg)
      
      max_first  <- max(data$price_avg)   # Specify max of first y axis
      max_second <- max(data$quantity) # Specify max of second y axis
      min_first  <- min(data$price_avg)   # Specify min of first y axis
      min_second <- min(data$quantity) # Specify min of second y axis
      
      # scale and shift variables calculated based on desired mins and maxes
      scale = (max_second - min_second)/(max_first - min_first)
      shift = min_first - min_second
      
      # Function to scale secondary axis
      scale_function <- function(x, scale, shift){
        return ((x)*scale - shift)
      }
      
      # Function to scale secondary variable values
      inv_scale_function <- function(x, scale, shift){
        return ((x + shift)/scale)
      }
      
      
      
      p <- data |> 
        ggplot(aes(x = datetime)) +
        scale_color_manual(values = c(price = "orange",
                                      quantity = "blue")) +
        labs(x = "Time",
             y = "Value (gold)",
             color = "")
      
      if (length(input$display_choice) == 1) {
        if ("price" %in% input$display_choice) {
          p <- p + 
            geom_line(aes(y = price_avg, color = "price"))
        }
        
        if ("quantity" %in% input$display_choice) {
          p <- p +
            geom_line(aes(y = quantity, color = "quantity"))
        }
      } else {
        if ("price" %in% input$display_choice & "quantity" %in% input$display_choice) {
          p <- p +
            geom_line(aes(y = price_avg, color = "price")) + 
            geom_line(aes(y = inv_scale_function(quantity, scale, shift), color = "quantity")) + 
            scale_y_continuous(sec.axis = sec_axis(~scale_function(., scale, shift), 
                                                   name= "quantity")) +
            labs(x = "Time", y = "price", color = "")
        }
      }
      
      
      
      
      return(p)
      inv_scale_function(RESP, scale, shift)
    })
    
    
    output$card_name <- renderText({
      paste("Plot: ", paste(input$display_choice, collapse=" & "))
    })
  })
}
