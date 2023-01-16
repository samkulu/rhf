read_get <- function(url, encoding = "UTF-8"){
  require(httr)


  # Check Proxy
  if(!exists("proxy_user")) proxy_user <<- get_proxy_user()
  if(is.na(proxy_user[1])) stop("Proxy user for authentication missing!")

  # Set Proxy
  httr::set_config(
    httr::use_proxy(url = get_proxy(), username=proxy_user[1],password = proxy_user[2] , auth="any"),
    override = TRUE
  )

  # Retrieve data
  result <- httr::GET(url)
  json <- httr::content(result, as="text", encoding = encoding)

  # Return
  return(json)
}

