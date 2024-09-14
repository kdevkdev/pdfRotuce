#' Title
#'
#' @param folder
#' @param overwrite either `"all"`, `"build"`, or `"none"` (default). All overwrtites all files including the manuscript template, `"build"` just the files in the `build` folder.
#' @param article_type type of thea article to be copied to the new folder. Currently `article` (default) and `editorial` are available
#'
#' @return `TRUE` if all files were copied successfully, `FALSE` otherwise.
#' @export
#'
#' @examples
draft <- function(folder, overwrite = "none", article_type = "article"){


  # create directories
  if(endsWith(folder,"/") || endsWith(folder,"\\")) # remove trailing slash
    folder <- substr(folder,1, nchar(folder)-1)

  # check if already exists
  stopifnot("folder already exists"=!dir.exists(folder) || overwrite %in%  c("all", "build" ))

  if(dir.exists(paste0(folder, "/", "build"))){
    unlink(x = paste0(folder, "/", "build"), recursive =T)
  }

  stopifnot("invalid article type, must be 'editorial' or 'article'" = (article_type %in% c("editorial", "article")))

  dir.create(paste0(folder)) # should fail if already exists

  # copy template folder to newly created directory
  template_path <- system.file("./template", package = "pdfRotuce")



  # build and figure directories, figure direoctires might need to be customized by type later

  # only overwrite if specified. Will still copy build directory as its deleted above
  ow = overwrite %in% c("all")

  r = file.copy(from = paste0(template_path, "/build"), to = paste0(folder), recursive = T, overwrite = ow)
  r = file.copy(from = paste0(template_path, "/figures"), to = paste0(folder), recursive = T, overwrite = ow)

  # these we also need to rename
  r = file.copy(from = paste0(template_path, "/", "metadata_", article_type, ".csv"),  to = paste0(folder, "/metadata.csv"), recursive = T, overwrite = ow)
  r = file.copy(from = paste0(template_path, "/", "manuscript_", article_type, ".docx"),  to = paste0(folder, "/manuscript.docx"), recursive = T, overwrite = ow)

  # return true if no F from files.copy
  return(all(r))

}

