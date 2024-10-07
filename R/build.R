#' Title
#'
#' @param src
#'
#' @return
#' @export
#'
#' @examples
build <- function(folder, src = "manuscript.docx", meta_csv = "metadata.csv", ...){

  # get basename without file extension
  basename = tools::file_path_sans_ext(basename(src))

  # create directories
  if(endsWith(folder,"/") || endsWith(folder,"\\")) # remove trailing slash
    folder <- substr(folder,1, nchar(folder)-1)

  doc_path = paste0(folder,"/", src)
  metacsv_path = paste0(folder,"/", meta_csv)
  build_path  = paste0(folder, "/build")
  rmd_outpath = paste0(build_path, "/", basename, ".Rmd")
  xml_outpath = paste0(folder, "/", "jatsxml_", basename, ".xml")


  # check if we can find the manuscript
  stopifnot("doc file not found at specified location" = file.exists(doc_path))

  # first markdonify and write to file
  markdownify( src_docx = doc_path, working_folder = build_path, meta_csv = metacsv_path, rmd_outpath = rmd_outpath, xml_outpath = xml_outpath, ...)

  # if successfull in writing file
  if(file.exists(rmd_outpath))
  {
    # save old option and set new
    bckp_tinytexclean <- options(tinytex.clean = FALSE)
    # try to rendr
    respath = rmarkdown::render(rmd_outpath, output_dir = build_path, clean = F, output_options = list("keep_md" = T, "keep_tex" = T))

    # restore tinytex.clean option
    options(tinytex.clean = bckp_tinytexclean)
  }
}
