
### STATUS TABLES HCCH ####

# Conventions (only the relevant for specific business type)
conv_hcch <- c( HUe93 = "69",
           HUe54 = "33",
           HUe61 = "41",
           HBewUe70 = "82",
           HUe65 = "17"
)

#' Download HCCH's MEMBER's Status Tables
#'
#' @return
#' @export
#'
#' @examples
#' download_hcch()
download_hcch <- function(dest = NA){

  # Destination
  if(is.na(dest)){
    dest <- read_options() %>% filter(KEY == "DEST") %>% pull()
    dest <- file.path(dest, "HCCH")
  }

  # Url Pattern
  link <- "https://www.hcch.net/de/instruments/conventions/status-table/?cid=%NUM%"

  # Chaching
  result <- list()

  for (i in 1:length(conv_hcch)) {
    name <- names(conv_hcch)[i]
    message(name)
    url <- gsub("%NUM%", conv_hcch[i],  link)
    page <- suppressWarnings(readLines(url, encoding = "UTF-8"))

    path <- file.path(dest, name)
    if(!dir.exists(path)) dir.create(path)

    # Find Date
    # - Check Stuctrue
    # - Check result for Date Class
    idx <- grep("Letzte Ergänzungen", page)
    m <- regexpr("[0-9]{1,2}-[IXV]+-[0-9]{4}", page[idx + 1])
    dt <- regmatches(page[idx + 1], m)
    ss <- strsplit(dt, split="-")[[1]]

    if(length(ss) != 3) browser()

    dt <- gsub(ss[2], as.numeric(as.roman(ss[2])), dt)
    dt <- as.Date(dt, format("%d-%m-%Y"))
    if(class(dt) != "Date") browser()

    # Find Tables
    if(class(page) == "try-error")
      return(NULL)
    else
      tables <- readHTMLTable(page)

    if(length(tables) < 1)
      return(NULL)

    # Output to File(s) rds and xlsx
    filename <- paste(format(dt,"%Y%m%d_"),name,".xlsx", sep="")
    fullname <- file.path(path, filename)
    fullname2 <- gsub(".xlsx",".rds",fullname)

    if(file.exists(fullname)) {
      cat("current status \n")
      tmp <- readRDS(fullname2)
      if(identical(tmp, tables[[1]]))
        cat("same table \n")
      else
        cat("check changes! \n")

    } else {
      # exportXLS(fullname, "StatusTable", tables[[1]])
      cat("changed table !\n")
      writexl::write_xlsx(list(StatusTable = tables[[1]]),  fullname)
      saveRDS(tables[[1]], fullname2)
    }

    result[[i]] <- tables[[1]]
  }

  # Name properly
  names(result) <- names(conv_hcch)

  # result
}