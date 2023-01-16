
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
    # page <- suppressWarnings(readLines(url, encoding = "UTF-8"))
    page <- read_get(url, encoding = "UTF-8")

    path <- file.path(dest, name)
    if(!dir.exists(path)) dir.create(path)

    # Find Date
    # - Check Stuctrue
    # - Check result for Date Class
    xx <- strsplit(page, split="\n")[[1]]
    idx <- grep("Letzte Ergänzungen", xx)
    m <- regexpr("[0-9]{1,2}-[IXV]+-[0-9]{4}", xx[idx + 1])
    dt <- regmatches(xx[idx + 1], m)
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
      cat("current status date \n")
      tmp <- readRDS(fullname2)
      if(identical(tmp, tables[[1]])){
        cat("same table \n")
      } else {
        cat("dodgy changes! \n")
        diff <- dplyr::setdiff(tmp, tables[[1]])
        dt <- Sys.Date()
        filename <- paste(format(dt,"%Y%m%d_"),name,"_dodgy.xlsx", sep="")
        fullname <- file.path(path, filename)
        fullname2 <- gsub(".xlsx",".rds",fullname)

        # Why Difference? Dodgy
        data <- list(StatusTable = tables[[1]])
        data$DIFF <- dplyr::setdiff(tmp, tables[[1]])
        # Export
        writexl::write_xlsx(data,  fullname)
        saveRDS(tables[[1]], fullname2)
      }


    } else {
      # Difference obvious! New status date
      data <- list(StatusTable = tables[[1]])
      tmp <- list.files(dirname(fullname2), "\\.rds", full.names = TRUE) %>%
              last() %>% readRDS()
      data$DIFF <- dplyr::setdiff(tmp, tables[[1]])
      data$NEW <- dplyr::setdiff(tables[[1]], tmp)

      # exportXLS(fullname, "StatusTable", tables[[1]])
      cat("changed table !\n")
      writexl::write_xlsx(data,  fullname)

      saveRDS(tables[[1]], fullname2)
    }

    result[[i]] <- tables[[1]]
  }

  # Name properly
  names(result) <- names(conv_hcch)

  # result
}
