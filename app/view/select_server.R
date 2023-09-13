box::use(
  shiny[...],
  shinyWidgets[pickerInput]
)

box::use(
  app/logic/api_interactions
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("select_region"),
                label = "Region",
                choices = NULL),
    selectInput(ns("select_server"),
                label = "Server",
                choices = NULL),
    pickerInput(ns("select_faction"),
                label = "Faction",
                choices = c("Alliance","Horde"),
                choicesOpt = list(content = c("<img src='https://nexushub.co/img/wow-classic/ui/alliance.svg' width=20px></img> Alliance",
                                              "<img src='https://nexushub.co/img/wow-classic/ui/horde.svg' width=20px></img> Horde") )),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    servers <- reactive(api_interactions$get_servers())
    
    regions <- reactive({
      req(servers())
      api_interactions$get_regions(servers())
    })
    
    
    observe({
      req(regions())
      updateSelectInput(inputId = "select_region",
                        choices = regions())
    })
    
    region_servers <- reactive({
      req(servers())
      req(input$select_region)
      api_interactions$get_region_servers(servers(), input$select_region)
    })
    
    observe({
      req(region_servers())
      updateSelectInput(inputId = "select_server",
                        choices = region_servers(),
                        selected = "Auberdine")
    })
    
    return(
      reactive({
        req(input$select_region)
        req(input$select_server)
        req(input$select_faction)
        list(region = tolower(input$select_region),
             server =   tolower(input$select_server),
             faction =  tolower(input$select_faction))
      }))
    
    
  })
}
