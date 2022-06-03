#' Read COE JSON text file
#'
#' Please check for json consistency with an online checker like
#' https://jsononline.net/json-checker
#'
#' @param json
#'
#' @return
#' @export
#'
#' @examples
read_json_coe <- function(json){
  # Read JSON Data from text file
  data <- jsonlite::fromJSON(json)

  # Return
  list(MEMBER = data[[1]],
       NonMEMBER = data[[2]])
}


