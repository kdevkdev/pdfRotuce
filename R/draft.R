#' Title
#'
#' @param folder
#'
#' @return `TRUE` if all files were copied successfully, `FALSE` otherwise.
#' @export
#'
#' @examples
draft <- function(folder, overwrite = F){


  # create directories
  if(endsWith(folder,"/") || endsWith(folder,"\\")) # remove trailing slash
    folder <- substr(folder,1, nchar(folder)-1)

  # check if already exists
  stopifnot("folder already exists"=!dir.exists(folder) || overwrite)


  dir.create(paste0(folder))

  # copy template folder to newly created directory
  template_path <- system.file("./template", package = "pdfRotuce")



  r = file.copy(from = list.files(template_path, full.names = TRUE), to = folder, recursive = T)

  # return true if no F from files.copy
  return(all(r))

}

