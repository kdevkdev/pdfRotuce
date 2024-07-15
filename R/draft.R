#' Title
#'
#' @param folder
#' @param overwrite either `"all"`, `"build"`, or `"none"` (default). All overwrtites all files including the manuscript template, `"build"` just the files in the `build` folder.
#'
#'
#' @return `TRUE` if all files were copied successfully, `FALSE` otherwise.
#' @export
#'
#' @examples
draft <- function(folder, overwrite = "none"){


  # create directories
  if(endsWith(folder,"/") || endsWith(folder,"\\")) # remove trailing slash
    folder <- substr(folder,1, nchar(folder)-1)

  # check if already exists
  stopifnot("folder already exists"=!dir.exists(folder) || overwrite %in%  c("all", "build" ))

  if(dir.exists(paste0(folder, "/", "build"))){
    unlink(x = paste0(folder, "/", "build"), recursive =T)
  }

  dir.create(paste0(folder)) # should fail if already exists

  # copy template folder to newly created directory
  template_path <- system.file("./template", package = "pdfRotuce")


  # only overwrite if specified. Will still copy build directory as its deleted above
  ow = overwrite %in% c("all")
  r = file.copy(from = list.files(template_path, full.names = TRUE), to = folder, recursive = T, overwrite = ow)

  # return true if no F from files.copy
  return(all(r))

}

