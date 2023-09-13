box::use(
  purrr[map_df],
  tibble[as_tibble],
  lubridate[as_datetime],
  dplyr[...],
)

json_to_df_price <- function(price_json) {
  start <- Sys.time()
  df <- as_tibble(do.call(rbind, price_json$data)) |> 
    mutate(across(.fns = unlist)) |> 
    mutate(datetime = as_datetime(scannedAt),
           price_avg = as.integer(marketValue),
           price_min = as.integer(minBuyout),
           quantity = as.integer(quantity)) |> 
    ungroup()
  end <- Sys.time()
  message("json_to_df_price duration: ", end - start)
  return(df)
}
