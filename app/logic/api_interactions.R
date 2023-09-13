box::use(
  stringr,
  glue,
  httr2,
  jsonlite,
  purrr,
  dplyr,
  reactable,
  tidyr,
  tibble
)

#' @export
get_servers <- function() {
  servers_url <- "https://api.nexushub.co/wow-classic/v1/servers"
  
  servers_json <- httr2$request(servers_url) |> httr2$req_perform() |> httr2$resp_body_json()
  
  return(servers_json)
}

#' @export
get_regions <- function(servers){
  names(servers)
}

#' @export
get_region_servers <- function(servers, region){
  unlist(servers[[region]])
}

#' @export
search_item <- function(item) {
  
  fixed_search_string <- stringr$str_replace_all(item," ","%20")
  
  search_url <- glue$glue("https://api.nexushub.co/wow-classic/v1/search?query={fixed_search_string}&limit=50&threshold=1")
  
  results_json <- httr2$request(search_url) |> httr2$req_perform() |> httr2$resp_body_json()
  
  
  tibble$tibble(id = unlist(purrr$map(results_json, ~.x[['itemId']])),
                name = unlist(purrr$map(results_json, ~.x[['name']])),
                img_url = unlist(purrr$map(results_json, ~.x[['imgUrl']]))) |> 
    dplyr$distinct() |>
    dplyr$mutate(display = glue$glue(as.character("<a href='https://www.wowhead.com/item={id}/{name}'><img src='{img_url}' style='width:25px; margin-right:5px'></img></a>{name}")))
}


#' @export
search_item_table <- function(item_data) {
  reactable$reactable(item_data)
}

#' @export
get_item_price <- function(server, faction, item, timerange=7){
  start <- Sys.time()
  prices_url <- glue$glue('https://api.nexushub.co/wow-classic/v1/items/{server}-{faction}/{item}/prices?timerange={timerange}')
  query_result <- httr2$request(prices_url) |> httr2$req_perform() |> httr2$resp_body_json()
  end <- Sys.time()
  message("item price query duration: ", end - start)
  return(query_result)
}

#' @export
get_item_info <- function(server, faction, item){
  start <- Sys.time()
  prices_url <- glue$glue('https://api.nexushub.co/wow-classic/v1/items/{server}-{faction}/{item}')
  query_result <- httr2$request(prices_url) |> httr2$req_perform() |> httr2$resp_body_json()
  end <- Sys.time()
  message("item info query duration: ", end - start)
  return(query_result)
}
