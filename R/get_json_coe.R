#' Get Status Tables as JSON Data
#'
#' @param treatynum
#'
#' @return
#' @export
#'
#' @examples
#' get_json_coe("030")
#' json <- get_json_coe(conv_coe["EUeR"])
get_json_coe <- function(treatynum = NA,
                         link = "https://www.coe.int/en/web/conventions/full-list?module=signatures-by-treaty&treatynum=%treatynum%"){
  # Check
  stopifnot(!is.na(treatynum))

  # Find KEY
  url <- gsub("%treatynum%", treatynum, link)
  page <- readLines(url, encoding = "UTF-8")

  # RESPONSE
  # ...
  # <script>
  #   window.conventions_api_url='https://conventions-ws.coe.int/WS_LFRConventions/';
  #   window.conventions_api_key='hfghhgp2q5vgwg1hbn532kw71zgtww7e';
  # </script>

  # Get TOKEN
  api_key <- page[grep("window.conventions_api_key=", page)]
  key <- strsplit(api_key, split="'")[[1]][2]


  # GET NEW URL
  # https://conventions-ws.coe.int/WS_LFRConventions/api/signatures?numSTE=030&langue=ENG
  link2 <- "https://conventions-ws.coe.int/WS_LFRConventions/api/signatures?numSTE=%treatynum%&langue=ENG"
  url <- gsub("%treatynum%", treatynum, link2)

  # Create Header with token key

  # Host: conventions-ws.coe.int
  # User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:98.0) Gecko/20100101 Firefox/98.0
  # Accept: application/json
  # Accept-Language: en-US,en;q=0.5
  # Accept-Encoding: gzip, deflate, br
  # Referer: https://www.coe.int/
  # Content-Type: application/json
  # token: hfghhgp2q5vgwg1hbn532kw71zgtww7e
  # Origin: https://www.coe.int
  # Connection: keep-alive
  # Sec-Fetch-Dest: empty
  # Sec-Fetch-Mode: cors
  # Sec-Fetch-Site: same-site
  # Sec-GPC: 1


  # Check Proxy
  if(!exists("proxy_user")) proxy_user <<- get_proxy_user()
  if(is.na(proxy_user[1])) stop("Proxy user for authentication missing!")

  # Proxy-Server
  proxy <- get_proxy()

  httr::set_config(
    use_proxy(url = proxy, username=proxy_user[1],password = proxy_user[2] , auth="any"),
    override = TRUE
  )

  h <- httr::add_headers( Host = "conventions-ws.coe.int",
                    `User-Agent:` = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:98.0) Gecko/20100101 Firefox/98.0",
                    Accept = "application/json",
                    `Accept-Language:` = "en-US,en;q=0.5",
                    `Accept-Encoding` = "gzip, deflate, br",
                    `Referer` = "https://www.coe.int/",
                    `Content-Type` = "application/json",
                    token = key,
                    `Origin` = "https://www.coe.int",
                    `Connection` = "keep-alive",
                    `Sec-Fetch-Dest` = "empty",
                    `Sec-Fetch-Mode` = "cors",
                    `Sec-Fetch-Site` = "same-site",
                    `Sec-GPC` = "1"
  )

  # UBUNTU Workaround
  # Solving httr, cURL, and SSL problems on Ubuntu 20.04
  # See https://msmith.de/2020/10/02/httr-curl-ubuntu-20-04.html
  # for
  # Error in curl::curl_fetch_memory(url, handle = handle) :
  #   error:141A318A:SSL routines:tls_process_ske_dhe:dh key too small
  if(.Platform$OS.type == "unix"){
    # Needs SSL Workaround (under linux/ubuntu)

    # https://askubuntu.com/questions/1233186/ubuntu-20-04-how-to-set-lower-ssl-security-level

    # Adapted from https://github.com/Ensembl/ensembl-rest/issues/427#issue-614497457
    # by Kirill Tsukanov
    # $ curl -sS --ciphers 'DEFAULT:@SECLEVEL=1' https://rest.ensembl.org >/dev/null
    # $ curl -sS --ciphers 'DEFAULT:@SECLEVEL=2' https://rest.ensembl.org >/dev/null
    # curl: (35) error:14094410:SSL routines:ssl3_read_bytes:sslv3 alert handshake failure

    httr_config <- httr::config(ssl_cipher_list = "DEFAULT@SECLEVEL=1")
    page <- httr::with_config(config = httr_config, GET(url, h)) # Works 200

  } else {
    # Windows should work w/o special ssl security level
    page <- httr::GET(url, h)
  }

  # Check Response
  stopifnot(httr::status_code(page) == 200)
  stopifnot(httr::http_type(page) == "application/json")

  # Read json as text from http response
  json <- httr::content(page, as="text")

  # Return
  json

}
