#' Download Notifications from TreatyDataBaseNL
#'
#' This is not an official source. But the notifications from country NL
#' are very informative for other HCCH members as well.
#'
#' @param dest
#'
#' @return
#' @export
#'
#' @examples
#' download_treatydatabase_nl()
download_treatydatabase_nl <- function(dest = NA){
  # Destination
  if(is.na(dest)){
    dest <- read_options() %>% filter(KEY == "DEST") %>% pull()
    dest <- file.path(dest, "TreatyDataBaseNL")
  }

  # Conventions
  conv <- c( HUe93 = "005234",
             HUe54 = "007300",
             HUe61 = "009051",
             HBewUe70 = "002883",
             HUe65 = "004235"
  )

  # URL Pattern
  link <- "https://treatydatabase.overheid.nl/en/Verdrag/Details/%NUM%_dn.html"
  repo <- "https://repository.overheid.nl/frbr/vd/%NUM%/1/xml-en/%NUM%.xml"


  # Check all links and download if necessary
  for (i in 1:length(conv)) {
    name <- names(conv[i])
    message(name)
    url <- gsub("%NUM%",conv[i], link)
    urlRepo <-  gsub("%NUM%",conv[i], repo)

    page <- suppressWarnings(readLines(url, encoding = "UTF-8"))

    # XML Details
    # Why ? For tracking changes
    pageRepo <- suppressWarnings(readLines(urlRepo, encoding = "UTF-8"))
    ss <- pageRepo[grep("dcterms:modified", pageRepo)]
    m <- gregexpr("20[0-9]{2}\\-[0-9]{2}\\-[0-9]{2}", ss)
    dt <- regmatches(ss, m)[[1]]
    fi <- file.path(dest, name, paste(dt,"_",name, "_Notifications.xml", sep=""))

    if(!file.exists(fi))
      writeLines(pageRepo, fi, useBytes = TRUE)



    # THERE ARE ONLY LINKS !
    # tables <- readHTMLTable(page) # NOTHING
    lnks <- getHTMLLinks(page)
    lnks <- lnks[grepl("Notific", lnks)]

    fls <- list.files(file.path(dest, name), full.names = TRUE)
    if(all(basename(lnks) %in% basename(fls))) {
      cat("complete \n")
    } else {
      # Download file(s)
      # e.g. http://repository.overheid.nl/frbr/vd/009051/1/pdf/009051_Notificaties_9.pdf

      for(l in lnks) {
        # Destination of file
        fullname <- file.path(dest, name, basename(l))
        # Create Folder if not exists
        if(!dir.exists(dirname(fullname)))
          dir.create(dirname(fullname))
        # File exists?
        if(!file.exists(fullname))
          download.file(l, fullname, mode = "wb")

      }
    }

  }

  # Return VOID
}
