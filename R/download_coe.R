# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

### STATUS TABLES COE ####

# Conventions (only the relevant for specific business type)
conv_coe <- c(EUeR = "030",
          GwG = "141",
          ZPII = "182",
          CCC = "185",
          EueZ94 = "094"
)


#' Download COE's MEMBER's Status Tables
#'
#' This pkg is used for downloading the status tables from the member states
#' for specific treaties of the Council of Europe (COE).
#'
#' The information for a subset of treaties should be checked at least monhtly
#' in order to update the laenderindex on RHF-website.
#'
#'  https://rhf.admin.ch/ > Länderindex
#'
#'
#' @return
#' @export
#'
#' @examples
#' download_coe_member()
download_coe_member <- function(dest = NA){

  # Destination
  if(is.na(dest)){
    dest <- read_options() %>% filter(KEY == "DEST") %>% pull()
    dest <- file.path(dest, "COE")
  }

  # Process all treatynums
  for (treatynum in conv_coe){

    # names(conv_coe)[which(conv_coe == treatynum)]
    name <- names(which(conv_coe == treatynum))
    message(name)
    json <- get_json_coe(treatynum)

    # Create folder in destination for downloading treaties
    folder <- file.path(dest,name)
    if(!dir.exists(folder))
      dir.create(folder)

    # Latest JSON
    fls <- list.files(folder, pattern = ".json", full.names = TRUE)
    fls <- sort(fls, decreasing = TRUE)

    # Check for changes
    tmp <- readLines(fls[1], encoding = "UTF-8")
    if(identical(tmp,json)) {
      # Same Json file/data
      cat("same \n")
    } else {
      # Not the same json file export json & tables

      # Filename
      fi <- format(Sys.Date(), "%Y%m%d_")
      fi <- paste(fi, name,".json", sep="")
      filename <- file.path(folder, fi)

      if (file.exists(filename)) {
        cat("exists \n")
      } else {
        # Export json text file
        writeLines(json, filename, useBytes = TRUE)

        # Export human readable table
        filename <- gsub("\\.json","\\.xlsx",filename)
        data <- read_json_coe(json)

        # Difference
        old <- read_json_coe(tmp)
        data$diffMEMBER <- dplyr::setdiff(old$MEMBER, data$MEMBER)

        writexl::write_xlsx(data, filename)

        cat("saved \n")
      }
    }
  }

}

