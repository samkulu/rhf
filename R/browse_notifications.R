#' Browse Notifications (under construction)
#'
#' This Application is shown as IFRAME inside of the original COE search.
#' For RSelenium we want to visit just the app without the website around.
#'
#' @param treatynum
#'
#' @return
#' @export
#'
#' @examples
#' treatynum <- conv_coe[1]
#' treatynum <- conv_coe[2]
#' treatynum <- conv_coe[3] # ZPII
#' treatynum <- conv_coe[4] # CCC
#' treatynum <- conv_coe[5] # EueZ94
#' browse_notifications(treatynum)
browse_notifications <- function(treatynum = "030", dest = NA){
  require(RSelenium)

  name <- names(treatynum)
  message(name)
  stopifnot(name %in% names(conv_coe))

  # Destination
  if(is.na(dest)){
    dest <- read_options() %>% filter(KEY == "DEST") %>% pull()
    dest <- file.path(dest, "coe")
  }

  # Create Folder if not exists
  folder <- file.path(dest, "_Notifications", name )
  if(!dir.exists(folder)) dir.create(folder, recursive = TRUE)

  # Close obsolete connections
  cons <- getAllConnections()
  if (length(cons) > 3) closeAllConnections()

  # start a server with utility function
  driver <- RSelenium::rsDriver(browser=c("firefox"))

  # Empty Box
  # url <- "https://search.coe.int/conventions#showBanner=0#showSearchBox=0#k=*#f=%5B%7B%22p%22%3A%22Title%22%2C%22i%22%3A1%2C%22o%22%3A1%2C%22m%22%3A2%7D%5D"
  # Filled Box (example treatynum 030)
  # See https://search.coe.int/conventions#showBanner=0#showSearchBox=0#k=*030#f=%5B%7B%22p%22%3A%22Title%22%2C%22i%22%3A1%2C%22o%22%3A1%2C%22m%22%3A2%7D%5D
  link <- "https://search.coe.int/conventions#showBanner=0#showSearchBox=0#k=*%treatynum%#f=%5B%7B%22p%22%3A%22Title%22%2C%22i%22%3A1%2C%22o%22%3A1%2C%22m%22%3A2%7D%5D"
  URL <- gsub("%treatynum%", treatynum, link)

  # Navigate
  # TEST driver$client$navigate("https://www.bbc.com/")
  driver$client$navigate(URL)

  # Scroll down
  webElem <- driver$client$findElement("css", "body")
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(10)
  webElem$sendKeysToElement(list(key = "end"))


  # page <- driver$client$findElements(using = "class", value = "ms-srch-item col-md-12")$getElementText()
  page <- driver$client$findElements(using = "id", value = "showVisibileItems")
  # tmp <- page[[1]]$getElementText()
  # m <- gregexpr("JJ[0-9]+C", tmp)
  # JJ <- regmatches(tmp, m)[[1]]


  # mdf <- page[[1]]$findChildElements(using = "class", value = "ms-srch-group")

  mdf <- page[[1]]$findChildElements(using = "class", value = "ms-srch-item-link")
  mdf2 <- page[[1]]$findChildElements(using="class", value = "ms-srch-item-title")
  # mdf[[1]]$getClass() # webElement
  # href <- mdf[[1]]$getElementAttribute("href")[[1]] # "http://rm.coe.int/0900001680a61df7"
  # title <- mdf[[1]]$getElementAttribute("title")[[1]] # "Notification – JJ9343C Tr./222-20 – Belgium – Signature of the Protocol amending the Additional Protocol to the Convention on the Transfer of Sentenced Persons (CETS No. 222)."
  # val <- mdf2[[1]]$getElementText()[[1]]

  stopifnot(length(mdf) == length(mdf2))

  N <- length(mdf)

  lnks <- sapply(1:N, function(x) mdf[[x]]$getElementAttribute("href")[[1]] )
  ttls <- sapply(1:N, function(x) mdf[[x]]$getElementAttribute("title")[[1]] )
  dts <- sapply(1:N, function(x) mdf2[[x]]$getElementText()[[1]] )
  dts <- as.Date(dts, "%d/%m/%Y")
  fls <- paste(format(dts,"%Y%m%d_"),as.character(ttls),".pdf",sep="")
  # Replace forbidden characters
  # https://stackoverflow.com/questions/1976007/what-characters-are-forbidden-in-windows-and-linux-directory-names

  fls <- gsub("\\/","", fls)
  fls <- gsub("\\\\","", fls)
  fls <- gsub("\"","", fls)
  fls <- gsub(":|>|<|?|*","", fls)

  fls <- substr(fls,1,250)

  idx <- !grepl(".pdf$",fls)
  fls[idx] <- paste(fls[idx],".pdf", sep="")

  result <- data.frame(Links = gsub("http:", "https:", lnks),
                       Titles = ttls,
                       Dates = dts,
                       Filename = fls,
                       Length = nchar(fls),
                       DUP = duplicated(fls),
                       Fullname = file.path(folder, fls),
                       stringsAsFactors = FALSE  )



  # Are there duplicated names?
  if(length(which(result$DUP)) > 1) browser()

  # i <- 1
  # download.file(result$Links[i], result$Fullname[i], mode = "wb")
  # In download.file(result$Links[1], result$Fullname[1], mode = "wb") :
  #   URL 'https://rm.coe.int/0900001680a61df7': status was 'SSL connect error

  # SSL connect workaround
  httr_config <- httr::config(ssl_cipher_list = "DEFAULT@SECLEVEL=1")

  for(i in 1:nrow(result)){
    cat(i, " ", result$Filename[i], "\n")
    if(file.exists(result$Fullname[i])) {
      cat(" exists \n")
    } else {
      page <- httr::with_config(config = httr_config,
                          httr::GET(result$Links[i],
                              httr::write_disk(result$Fullname[i],
                                         overwrite = FALSE))) # Works
      cat(" done \n")
    }
  }


  driver$client$close()

}






