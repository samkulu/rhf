#' Get IP
#'
#' @param dns
#'
#' @return
#' @export
#'
#' @examples
#' get_ip()
#' get_ip("coe.int")
#' get_ip("rhf.admin.ch")
get_ip <- function(dns = "google.ch"){
  curl::nslookup(dns)
}

